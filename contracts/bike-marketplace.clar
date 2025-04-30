;; bike-marketplace.clar
;; Marketplace contract for a decentralized bike rental platform
;; Provides discovery and advanced rental management features including
;; reservations, extensions, returns, and promotional capabilities

;; ========================================
;; Error Constants
;; ========================================
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-BIKE-NOT-FOUND (err u1001))
(define-constant ERR-INVALID-DATE (err u1002))
(define-constant ERR-INVALID-DURATION (err u1003))
(define-constant ERR-ALREADY-RESERVED (err u1004))
(define-constant ERR-NOT-AVAILABLE (err u1005))
(define-constant ERR-RESERVATION-NOT-FOUND (err u1006))
(define-constant ERR-RENTAL-NOT-ACTIVE (err u1007))
(define-constant ERR-INSUFFICIENT-FUNDS (err u1008))
(define-constant ERR-INVALID-LOCATION (err u1009))
(define-constant ERR-INVALID-DISCOUNT (err u1010))

;; ========================================
;; Data Definitions
;; ========================================

;; Bike structure
(define-map bikes
  { bike-id: uint }
  {
    owner: principal,
    hourly-rate: uint,
    daily-rate: uint,
    weekly-rate: uint,
    security-deposit: uint,
    location-lat: int,
    location-lng: int,
    available: bool,
    bike-type: (string-ascii 20),
    total-rentals: uint,
    rating-sum: uint,
    rating-count: uint
  }
)

;; Reservations for future dates
(define-map reservations
  { reservation-id: uint }
  {
    bike-id: uint,
    renter: principal,
    start-time: uint,
    end-time: uint,
    total-cost: uint,
    paid: bool,
    status: (string-ascii 10)  ;; "active", "completed", "canceled"
    discount-applied: uint     ;; percentage discount applied
  }
)

;; Active rentals
(define-map active-rentals
  { bike-id: uint }
  {
    renter: principal,
    start-time: uint,
    end-time: uint,
    reservation-id: (optional uint),
    extension-count: uint
  }
)

;; User reputation and loyalty data
(define-map user-profiles
  { user: principal }
  {
    total-rentals: uint,
    loyalty-points: uint,
    rating-sum: uint,
    rating-count: uint
  }
)

;; Promotional discounts
(define-map promotions
  { promotion-id: uint }
  {
    discount-percentage: uint,
    min-rental-duration: uint, ;; in hours
    start-date: uint,
    end-date: uint,
    description: (string-ascii 100),
    active: bool
  }
)

;; Counter for generating unique IDs
(define-data-var reservation-id-counter uint u0)
(define-data-var promotion-id-counter uint u0)

;; Contract administrator
(define-data-var contract-owner principal tx-sender)

;; ========================================
;; Private Functions
;; ========================================

;; Calculate rental cost based on duration and rates
(define-private (calculate-rental-cost (bike-id uint) (duration-hours uint))
  (let (
    (bike-info (unwrap! (map-get? bikes { bike-id: bike-id }) ERR-BIKE-NOT-FOUND))
    (hourly-rate (get hourly-rate bike-info))
    (daily-rate (get daily-rate bike-info))
    (weekly-rate (get weekly-rate bike-info))
    
    ;; Calculate costs for different time periods
    (weeks (/ duration-hours u168))
    (remaining-hours-after-weeks (% duration-hours u168))
    (days (/ remaining-hours-after-weeks u24))
    (remaining-hours (% remaining-hours-after-weeks u24))
    
    ;; Calculate the total cost
    (weekly-cost (* weeks weekly-rate))
    (daily-cost (* days daily-rate))
    (hourly-cost (* remaining-hours hourly-rate))
  )
  (+ (+ weekly-cost daily-cost) hourly-cost))
)

;; Apply a discount to a rental cost
(define-private (apply-discount (base-cost uint) (discount-percentage uint))
  (if (> discount-percentage u100)
    ;; Cap discount at 100%
    u0
    (let (
      (discount-amount (/ (* base-cost discount-percentage) u100))
    )
    (- base-cost discount-amount))
  )
)

;; Check if a bike is available during a time period
(define-private (is-bike-available? (bike-id uint) (start-time uint) (end-time uint))
  (let (
    (bike (unwrap! (map-get? bikes { bike-id: bike-id }) false))
    (active-rental (map-get? active-rentals { bike-id: bike-id }))
  )
    (and 
      ;; Bike must be marked as available
      (get available bike)
      ;; No active rental, or the active rental ends before our start time
      (or 
        (is-none active-rental)
        (< (get end-time (unwrap! active-rental false)) start-time)
      )
      ;; Check against existing reservations
      (fold check-reservation-conflict true (get-reservations-for-bike bike-id))
    )
  )
)

;; Helper function for fold to check reservation conflicts
(define-private (check-reservation-conflict (result bool) (reservation-id uint))
  (if result
    (let (
      (reservation (unwrap! (map-get? reservations { reservation-id: reservation-id }) true))
    )
      (or 
        (not (is-eq (get status reservation) "active"))
        ;; Check if reservation time periods don't overlap
        (or 
          (< (get end-time reservation) start-time)
          (> (get start-time reservation) end-time)
        )
      )
    )
    false
  )
)

;; Get all reservation IDs for a bike (stub - would need to be implemented with a separate index map in practice)
(define-private (get-reservations-for-bike (bike-id uint))
  (list)  ;; This would need to be implemented with an index in a real contract
)

;; Get current time (would be implemented with an oracle in a real contract)
(define-private (get-current-time)
  block-height  ;; Using block-height as a proxy for time in this example
)

;; Get best applicable discount for a rental
(define-private (get-best-discount (duration-hours uint) (user principal))
  (let (
    (user-profile (default-to 
      { total-rentals: u0, loyalty-points: u0, rating-sum: u0, rating-count: u0 } 
      (map-get? user-profiles { user: user })))
    (loyalty-discount (if (>= (get total-rentals user-profile) u10) u5 u0))
    ;; Calculate duration discount based on rental length
    (duration-discount (cond
      ((>= duration-hours u168) u15)  ;; 15% for a week or more
      ((>= duration-hours u72) u10)   ;; 10% for 3+ days
      ((>= duration-hours u24) u5)    ;; 5% for 1+ day
      (true u0)                       ;; No discount for less than a day
    ))
  )
    ;; Return the higher discount
    (if (> loyalty-discount duration-discount)
      loyalty-discount
      duration-discount
    )
  )
)

;; ========================================
;; Read-Only Functions
;; ========================================

;; Get bike details
(define-read-only (get-bike-details (bike-id uint))
  (map-get? bikes { bike-id: bike-id })
)

;; Get reservation details
(define-read-only (get-reservation-details (reservation-id uint))
  (map-get? reservations { reservation-id: reservation-id })
)

;; Get active rental details for a bike
(define-read-only (get-active-rental (bike-id uint))
  (map-get? active-rentals { bike-id: bike-id })
)

;; Get user profile and reputation
(define-read-only (get-user-profile (user principal))
  (default-to 
    { total-rentals: u0, loyalty-points: u0, rating-sum: u0, rating-count: u0 }
    (map-get? user-profiles { user: user })
  )
)

;; Calculate user rating (out of 100)
(define-read-only (get-user-rating (user principal))
  (let (
    (profile (get-user-profile user))
    (count (get rating-count profile))
  )
    (if (> count u0)
      (/ (get rating-sum profile) count)
      u0
    )
  )
)

;; Calculate bike rating (out of 100)
(define-read-only (get-bike-rating (bike-id uint))
  (let (
    (bike (unwrap! (map-get? bikes { bike-id: bike-id }) { rating-sum: u0, rating-count: u0 }))
    (count (get rating-count bike))
  )
    (if (> count u0)
      (/ (get rating-sum bike) count)
      u0
    )
  )
)

;; Preview rental cost with applicable discounts
(define-read-only (preview-rental-cost (bike-id uint) (duration-hours uint) (user principal))
  (let (
    (base-cost (calculate-rental-cost bike-id duration-hours))
    (discount-percentage (get-best-discount duration-hours user))
  )
    {
      base-cost: base-cost,
      discount-percentage: discount-percentage,
      final-cost: (apply-discount base-cost discount-percentage)
    }
  )
)

;; ========================================
;; Public Functions
;; ========================================

;; Register a new bike in the marketplace
(define-public (register-bike 
  (bike-id uint)
  (hourly-rate uint)
  (daily-rate uint)
  (weekly-rate uint)
  (security-deposit uint)
  (location-lat int)
  (location-lng int)
  (bike-type (string-ascii 20))
)
  (let (
    (existing-bike (map-get? bikes { bike-id: bike-id }))
  )
    (asserts! (is-none existing-bike) (err u1011)) ;; Bike ID already exists
    
    (map-set bikes
      { bike-id: bike-id }
      {
        owner: tx-sender,
        hourly-rate: hourly-rate,
        daily-rate: daily-rate,
        weekly-rate: weekly-rate,
        security-deposit: security-deposit,
        location-lat: location-lat,
        location-lng: location-lng,
        available: true,
        bike-type: bike-type,
        total-rentals: u0,
        rating-sum: u0,
        rating-count: u0
      }
    )
    (ok true)
  )
)

;; Update bike details and availability
(define-public (update-bike
  (bike-id uint)
  (hourly-rate uint)
  (daily-rate uint)
  (weekly-rate uint)
  (security-deposit uint)
  (location-lat int)
  (location-lng int)
  (available bool)
)
  (let (
    (bike (unwrap! (map-get? bikes { bike-id: bike-id }) ERR-BIKE-NOT-FOUND))
  )
    ;; Ensure only the bike owner can update it
    (asserts! (is-eq tx-sender (get owner bike)) ERR-NOT-AUTHORIZED)
    
    ;; Don't allow changing availability if there's an active rental
    (asserts! 
      (or 
        available
        (is-none (map-get? active-rentals { bike-id: bike-id }))
      ) 
      (err u1012) ;; Cannot mark unavailable while rented
    )
    
    (map-set bikes
      { bike-id: bike-id }
      (merge bike {
        hourly-rate: hourly-rate,
        daily-rate: daily-rate,
        weekly-rate: weekly-rate,
        security-deposit: security-deposit,
        location-lat: location-lat,
        location-lng: location-lng,
        available: available
      })
    )
    (ok true)
  )
)

;; Reserve a bike for future use
(define-public (reserve-bike 
  (bike-id uint)
  (start-time uint)
  (end-time uint)
)
  (let (
    (bike (unwrap! (map-get? bikes { bike-id: bike-id }) ERR-BIKE-NOT-FOUND))
    (current-time (get-current-time))
    (duration-hours (/ (- end-time start-time) u3600))  ;; Convert seconds to hours
    (next-id (+ (var-get reservation-id-counter) u1))
  )
    ;; Validation checks
    (asserts! (get available bike) ERR-NOT-AVAILABLE)
    (asserts! (> start-time current-time) ERR-INVALID-DATE) ;; Start time must be in the future
    (asserts! (> end-time start-time) ERR-INVALID-DATE) ;; End time must be after start time
    (asserts! (>= duration-hours u1) ERR-INVALID-DURATION) ;; Minimum 1 hour rental
    
    ;; Check if bike is available for the requested time period
    (asserts! (is-bike-available? bike-id start-time end-time) ERR-ALREADY-RESERVED)
    
    ;; Calculate cost with applicable discounts
    (let (
      (rental-preview (preview-rental-cost bike-id duration-hours tx-sender))
      (final-cost (get final-cost rental-preview))
      (discount-percentage (get discount-percentage rental-preview))
    )
      ;; Update reservation counter
      (var-set reservation-id-counter next-id)
      
      ;; Create the reservation
      (map-set reservations
        { reservation-id: next-id }
        {
          bike-id: bike-id,
          renter: tx-sender,
          start-time: start-time,
          end-time: end-time,
          total-cost: final-cost,
          paid: false,
          status: "active",
          discount-applied: discount-percentage
        }
      )
      
      (ok next-id)
    )
  )
)

;; Pay for a reservation
(define-public (pay-for-reservation (reservation-id uint) (token-contract principal) (token-name (string-ascii 32)))
  (let (
    (reservation (unwrap! (map-get? reservations { reservation-id: reservation-id }) ERR-RESERVATION-NOT-FOUND))
    (bike (unwrap! (map-get? bikes { bike-id: (get bike-id reservation) }) ERR-BIKE-NOT-FOUND))
    (total-cost (get total-cost reservation))
    (bike-owner (get owner bike))
  )
    ;; Check authorization
    (asserts! (is-eq tx-sender (get renter reservation)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status reservation) "active") ERR-RESERVATION-NOT-FOUND)
    (asserts! (not (get paid reservation)) (err u1013)) ;; Already paid
    
    ;; Transfer payment
    (asserts! 
      (is-ok (contract-call? token-contract transfer total-cost tx-sender bike-owner none)) 
      ERR-INSUFFICIENT-FUNDS
    )
    
    ;; Update reservation as paid
    (map-set reservations
      { reservation-id: reservation-id }
      (merge reservation { paid: true })
    )
    
    (ok true)
  )
)

;; Start a bike rental (either from reservation or immediate)
(define-public (start-rental (bike-id uint) (reservation-id (optional uint)))
  (let (
    (bike (unwrap! (map-get? bikes { bike-id: bike-id }) ERR-BIKE-NOT-FOUND))
    (current-time (get-current-time))
  )
    ;; Handle reservation case
    (if (is-some reservation-id)
      (let (
        (res-id (unwrap! reservation-id ERR-RESERVATION-NOT-FOUND))
        (reservation (unwrap! (map-get? reservations { reservation-id: res-id }) ERR-RESERVATION-NOT-FOUND))
      )
        ;; Validate reservation
        (asserts! (is-eq tx-sender (get renter reservation)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get bike-id reservation) bike-id) ERR-BIKE-NOT-FOUND)
        (asserts! (is-eq (get status reservation) "active") (err u1014)) ;; Not an active reservation
        (asserts! (get paid reservation) (err u1015)) ;; Reservation not paid
        (asserts! (>= current-time (get start-time reservation)) (err u1016)) ;; Too early for pickup
        
        ;; Start the rental
        (map-set active-rentals
          { bike-id: bike-id }
          {
            renter: tx-sender,
            start-time: current-time,
            end-time: (get end-time reservation),
            reservation-id: (some res-id),
            extension-count: u0
          }
        )
        
        ;; Update bike availability
        (map-set bikes
          { bike-id: bike-id }
          (merge bike { available: false })
        )
        
        (ok true)
      )
      
      ;; Handle immediate rental (no reservation)
      (begin
        ;; Validate bike is available
        (asserts! (get available bike) ERR-NOT-AVAILABLE)
        (asserts! (is-none (map-get? active-rentals { bike-id: bike-id })) ERR-ALREADY-RESERVED)
        
        ;; For immediate rentals, we'd handle payment here, but for simplicity
        ;; we'll assume it's handled through a separate function
        
        (ok false) ;; Not implemented in this example
      )
    )
  )
)

;; Extend an active rental
(define-public (extend-rental (bike-id uint) (additional-hours uint))
  (let (
    (rental (unwrap! (map-get? active-rentals { bike-id: bike-id }) ERR-RENTAL-NOT-ACTIVE))
    (bike (unwrap! (map-get? bikes { bike-id: bike-id }) ERR-BIKE-NOT-FOUND))
    (current-time (get-current-time))
  )
    ;; Validate request
    (asserts! (is-eq tx-sender (get renter rental)) ERR-NOT-AUTHORIZED)
    (asserts! (> additional-hours u0) ERR-INVALID-DURATION)
    
    ;; Calculate extension cost - simplified for this example
    (let (
      (hourly-rate (get hourly-rate bike))
      (extension-cost (* hourly-rate additional-hours))
      (new-end-time (+ (get end-time rental) (* additional-hours u3600)))
      (bike-owner (get owner bike))
    )
      ;; Handle payment - simplified
      (asserts! 
        (is-ok (stx-transfer? extension-cost tx-sender bike-owner)) 
        ERR-INSUFFICIENT-FUNDS
      )
      
      ;; Update rental information
      (map-set active-rentals
        { bike-id: bike-id }
        (merge rental {
          end-time: new-end-time,
          extension-count: (+ (get extension-count rental) u1)
        })
      )
      
      (ok true)
    )
  )
)

;; Complete a bike rental
(define-public (complete-rental (bike-id uint) (rating (optional uint)))
  (let (
    (rental (unwrap! (map-get? active-rentals { bike-id: bike-id }) ERR-RENTAL-NOT-ACTIVE))
    (bike (unwrap! (map-get? bikes { bike-id: bike-id }) ERR-BIKE-NOT-FOUND))
    (bike-owner (get owner bike))
    (current-time (get-current-time))
  )
    ;; Either the renter or bike owner can complete the rental
    (asserts! (or (is-eq tx-sender (get renter rental)) (is-eq tx-sender bike-owner)) ERR-NOT-AUTHORIZED)
    
    ;; Update bike availability
    (map-set bikes
      { bike-id: bike-id }
      (merge bike { 
        available: true,
        total-rentals: (+ (get total-rentals bike) u1)
      })
    )
    
    ;; If a reservation was used, update it
    (match (get reservation-id rental)
      res-id (map-set reservations
        { reservation-id: res-id }
        (merge (unwrap! (map-get? reservations { reservation-id: res-id }) {})
          { status: "completed" }
        )
      )
      true
    )
    
    ;; Update user profile
    (let (
      (renter (get renter rental))
      (profile (default-to 
        { total-rentals: u0, loyalty-points: u0, rating-sum: u0, rating-count: u0 }
        (map-get? user-profiles { user: renter })
      ))
    )
      ;; Add to rental count and loyalty points
      (map-set user-profiles
        { user: renter }
        (merge profile {
          total-rentals: (+ (get total-rentals profile) u1),
          loyalty-points: (+ (get loyalty-points profile) u1)
        })
      )
      
      ;; If rating provided, update bike rating
      (match rating
        rate (if (and (>= rate u0) (<= rate u100))
          (map-set bikes
            { bike-id: bike-id }
            (merge bike {
              rating-sum: (+ (get rating-sum bike) rate),
              rating-count: (+ (get rating-count bike) u1)
            })
          )
          false
        )
        false
      )
      
      ;; Remove active rental
      (map-delete active-rentals { bike-id: bike-id })
      
      (ok true)
    )
  )
)

;; Create a promotion
(define-public (create-promotion 
  (discount-percentage uint)
  (min-rental-duration uint)
  (start-date uint)
  (end-date uint)
  (description (string-ascii 100))
)
  (let (
    (next-id (+ (var-get promotion-id-counter) u1))
  )
    ;; Only contract owner can create promotions
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    
    ;; Validate promotion parameters
    (asserts! (<= discount-percentage u100) ERR-INVALID-DISCOUNT)
    (asserts! (> min-rental-duration u0) ERR-INVALID-DURATION)
    (asserts! (> end-date start-date) ERR-INVALID-DATE)
    
    ;; Create the promotion
    (var-set promotion-id-counter next-id)
    (map-set promotions
      { promotion-id: next-id }
      {
        discount-percentage: discount-percentage,
        min-rental-duration: min-rental-duration,
        start-date: start-date,
        end-date: end-date,
        description: description,
        active: true
      }
    )
    
    (ok next-id)
  )
)

;; Cancel a reservation
(define-public (cancel-reservation (reservation-id uint))
  (let (
    (reservation (unwrap! (map-get? reservations { reservation-id: reservation-id }) ERR-RESERVATION-NOT-FOUND))
    (bike (unwrap! (map-get? bikes { bike-id: (get bike-id reservation) }) ERR-BIKE-NOT-FOUND))
    (current-time (get-current-time))
  )
    ;; Only the renter or bike owner can cancel
    (asserts! (or 
      (is-eq tx-sender (get renter reservation))
      (is-eq tx-sender (get owner bike))
    ) ERR-NOT-AUTHORIZED)
    
    ;; Check that the reservation is active and not already started
    (asserts! (is-eq (get status reservation) "active") (err u1014))
    (asserts! (> (get start-time reservation) current-time) (err u1017)) ;; Cannot cancel already started rentals
    
    ;; Update reservation status
    (map-set reservations
      { reservation-id: reservation-id }
      (merge reservation { status: "canceled" })
    )
    
    ;; If reservation was paid, handle refund logic here
    ;; (Simplified for this example)
    
    (ok true)
  )
)

;; Transfer contract ownership
(define-public (transfer-contract-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)