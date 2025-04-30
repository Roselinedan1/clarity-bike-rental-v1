;; reputation-system
;; This contract implements a reputation system for tracking and managing reputation scores 
;; for both bike owners and renters in a decentralized bike rental platform.
;; Users gain reputation based on positive behaviors and can lose it for negative ones.
;; Higher reputation unlocks benefits for platform participants.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u1001))
(define-constant ERR-USER-NOT-FOUND (err u1002))
(define-constant ERR-INVALID-RATING (err u1003))
(define-constant ERR-ALREADY-RATED (err u1004))
(define-constant ERR-NO-RENTAL-RECORD (err u1005))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u1006))

;; Constants
(define-constant MAX-RATING u5)                  ;; Maximum rating value
(define-constant MIN-RATING u1)                  ;; Minimum rating value
(define-constant INITIAL-REPUTATION-SCORE u100)  ;; Starting reputation for new users
(define-constant MIN-REPUTATION-THRESHOLD u80)   ;; Minimum reputation for good standing
(define-constant PREMIUM-REPUTATION-THRESHOLD u150) ;; Threshold for premium benefits

;; Data space definitions
;; Store reputation scores for all users
(define-map user-reputation 
  { user: principal } 
  { score: uint, num-ratings: uint, total-rating-sum: uint }
)

;; Track rental rating records to prevent duplicate ratings
(define-map rental-ratings
  { rental-id: uint, rater: principal }
  { rated: bool }
)

;; Admin principal for managing the system
(define-data-var contract-admin principal tx-sender)

;; Private functions

;; Initialize a new user reputation record if not exists
(define-private (ensure-user-reputation (user principal))
  (if (is-none (map-get? user-reputation { user: user }))
      (map-set user-reputation 
        { user: user } 
        { score: INITIAL-REPUTATION-SCORE, num-ratings: u0, total-rating-sum: u0 })
      true)
)

;; Calculate new reputation score based on rating
(define-private (calculate-new-score (current-score uint) (rating uint) (is-positive bool))
  (let ((impact (if is-positive 
                    ;; Positive ratings increase score based on rating value
                    (* rating u5)  
                    ;; Negative ratings cause a larger penalty
                    (* rating u7))))
    (if is-positive
        ;; Add points for positive behavior
        (+ current-score impact)
        ;; Subtract points for negative behavior, but never below 0
        (if (> current-score impact)
            (- current-score impact)
            u0))))
)

;; Validate that rating is within acceptable range
(define-private (is-valid-rating (rating uint))
  (and (>= rating MIN-RATING) (<= rating MAX-RATING))
)

;; Read-only functions

;; Get reputation score and details for a user
(define-read-only (get-user-reputation (user principal))
  (default-to 
    { score: INITIAL-REPUTATION-SCORE, num-ratings: u0, total-rating-sum: u0 }
    (map-get? user-reputation { user: user }))
)

;; Check if user has reputation above premium threshold
(define-read-only (has-premium-status (user principal))
  (let ((user-rep (get-user-reputation user)))
    (>= (get score user-rep) PREMIUM-REPUTATION-THRESHOLD))
)

;; Check if user has reputation above minimum threshold
(define-read-only (has-good-standing (user principal))
  (let ((user-rep (get-user-reputation user)))
    (>= (get score user-rep) MIN-REPUTATION-THRESHOLD))
)

;; Get average rating for a user (out of 5)
(define-read-only (get-average-rating (user principal))
  (let ((user-rep (get-user-reputation user)))
    (if (> (get num-ratings user-rep) u0)
        (/ (get total-rating-sum user-rep) (get num-ratings user-rep))
        u0))
)

;; Check if a rental has already been rated by a specific user
(define-read-only (has-rated-rental (rental-id uint) (rater principal))
  (default-to 
    false
    (get rated (map-get? rental-ratings { rental-id: rental-id, rater: rater })))
)

;; Public functions

;; Submit a rating for a renter after they've returned a bike
(define-public (rate-renter (renter principal) (rental-id uint) (rating uint) (is-positive bool))
  (let ((current-rep (get-user-reputation renter)))
    ;; Validate inputs
    (asserts! (is-valid-rating rating) ERR-INVALID-RATING)
    (asserts! (not (has-rated-rental rental-id tx-sender)) ERR-ALREADY-RATED)
    
    ;; Mark this rental as rated by this owner
    (map-set rental-ratings { rental-id: rental-id, rater: tx-sender } { rated: true })
    
    ;; Ensure user has a reputation record
    (ensure-user-reputation renter)
    
    ;; Update reputation
    (ok (map-set user-reputation 
          { user: renter }
          { 
            score: (calculate-new-score (get score current-rep) rating is-positive),
            num-ratings: (+ (get num-ratings current-rep) u1),
            total-rating-sum: (+ (get total-rating-sum current-rep) rating)
          }))
  )
)

;; Submit a rating for a bike owner after rental period
(define-public (rate-owner (owner principal) (rental-id uint) (rating uint) (is-positive bool))
  (let ((current-rep (get-user-reputation owner)))
    ;; Validate inputs
    (asserts! (is-valid-rating rating) ERR-INVALID-RATING)
    (asserts! (not (has-rated-rental rental-id tx-sender)) ERR-ALREADY-RATED)
    
    ;; Mark this rental as rated by this renter
    (map-set rental-ratings { rental-id: rental-id, rater: tx-sender } { rated: true })
    
    ;; Ensure user has a reputation record
    (ensure-user-reputation owner)
    
    ;; Update reputation
    (ok (map-set user-reputation 
          { user: owner }
          { 
            score: (calculate-new-score (get score current-rep) rating is-positive),
            num-ratings: (+ (get num-ratings current-rep) u1),
            total-rating-sum: (+ (get total-rating-sum current-rep) rating)
          }))
  )
)

;; Admin function to manually adjust reputation in exceptional cases
(define-public (admin-adjust-reputation (user principal) (adjustment-amount uint) (increase bool))
  (let ((current-rep (get-user-reputation user)))
    ;; Only contract admin can call this function
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
    
    ;; Ensure user has a reputation record
    (ensure-user-reputation user)
    
    ;; Apply adjustment
    (ok (map-set user-reputation 
          { user: user }
          { 
            score: (if increase
                      (+ (get score current-rep) adjustment-amount)
                      (if (> (get score current-rep) adjustment-amount)
                          (- (get score current-rep) adjustment-amount)
                          u0)),
            num-ratings: (get num-ratings current-rep),
            total-rating-sum: (get total-rating-sum current-rep)
          }))
  )
)

;; Transfer contract admin rights to a new principal
(define-public (set-contract-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
    (ok (var-set contract-admin new-admin))
  )
)

;; Verify if a user has sufficient reputation for reduced deposit
;; Can be called by other contracts in the system
(define-public (check-reduced-deposit-eligibility (user principal))
  (begin
    (asserts! (has-premium-status user) ERR-INSUFFICIENT-REPUTATION)
    (ok true)
  )
)