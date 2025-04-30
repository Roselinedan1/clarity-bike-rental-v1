```clarity
;; Define data variables
(define-data-var rental-fee-per-block uint u10) ;; in microSTX
(define-data-var deposit-amount uint u5000) ;; in microSTX
(define-map bikes
    {owner: principal, bike-id: uint}
    {
        available: bool,
        renter: (optional principal),
        rental-start: (optional uint),
        custom-rate: (optional uint)
    }
)
(define-map owner-bikes principal (list 100 uint))

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-bike-not-available (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-no-active-rental (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-bike-not-found (err u105))
(define-constant err-bike-id-taken (err u106))

;; Public functions
(define-public (register-bike (bike-id uint) (custom-rate (optional uint)))
    (let (
        (current-bikes (default-to (list) (map-get? owner-bikes tx-sender)))
        (bike-key {owner: tx-sender, bike-id: bike-id})
    )
        (asserts! (is-none (map-get? bikes bike-key)) err-bike-id-taken)
        (map-set bikes bike-key {
            available: true,
            renter: none,
            rental-start: none,
            custom-rate: custom-rate
        })
        (map-set owner-bikes tx-sender (append current-bikes bike-id))
        (ok true)
    )
)

(define-public (update-bike-rate (bike-id uint) (custom-rate (optional uint)))
    (let (
        (bike-key {owner: tx-sender, bike-id: bike-id})
        (bike (unwrap! (map-get? bikes bike-key) err-bike-not-found))
    )
        (asserts! (get available bike) err-bike-not-available)
        (map-set bikes bike-key (merge bike {custom-rate: custom-rate}))
        (ok true)
    )
)

(define-public (rent-bike (bike-owner principal) (bike-id uint))
    (let (
        (bike-key {owner: bike-owner, bike-id: bike-id})
        (bike (unwrap! (map-get? bikes bike-key) err-bike-not-found))
        (total-cost (var-get deposit-amount))
    )
        (asserts! (get available bike) err-bike-not-available)
        (try! (stx-transfer? total-cost tx-sender contract-owner))
        (map-set bikes bike-key {
            available: false,
            renter: (some tx-sender),
            rental-start: (some block-height),
            custom-rate: (get custom-rate bike)
        })
        (ok true)
    )
)

(define-public (return-bike (bike-owner principal) (bike-id uint))
    (let (
        (bike-key {owner: bike-owner, bike-id: bike-id})
        (bike (unwrap! (map-get? bikes bike-key) err-bike-not-found))
        (rental-start (unwrap! (get rental-start bike) err-no-active-rental))
        (rental-duration (- block-height rental-start))
        (rate (default-to (var-get rental-fee-per-block) (get custom-rate bike)))
        (rental-cost (* rental-duration rate))
        (deposit (var-get deposit-amount))
        (refund-amount (if (> deposit rental-cost) (- deposit rental-cost) u0))
    )
        (asserts! (is-eq (some tx-sender) (get renter bike)) err-unauthorized)
        
        ;; Transfer rental fee to bike owner
        (try! (stx-transfer? rental-cost contract-owner bike-owner))
        
        ;; Return remaining deposit to renter
        (try! (stx-transfer? refund-amount contract-owner tx-sender))
        
        (map-set bikes bike-key {
            available: true,
            renter: none,
            rental-start: none,
            custom-rate: (get custom-rate bike)
        })
        (ok rental-cost)
    )
)

;; Read only functions
(define-read-only (get-bike-status (bike-owner principal) (bike-id uint))
    (ok (map-get? bikes {owner: bike-owner, bike-id: bike-id}))
)

(define-read-only (get-owner-bikes (owner principal))
    (ok (map-get? owner-bikes owner))
)

(define-read-only (get-rental-fee-per-block)
    (ok (var-get rental-fee-per-block))
)

(define-read-only (get-deposit-amount)
    (ok (var-get deposit-amount))
)

(define-read-only (calculate-current-cost (bike-owner principal) (bike-id uint))
    (let (
        (bike-key {owner: bike-owner, bike-id: bike-id})
        (bike (unwrap! (map-get? bikes bike-key) err-bike-not-found))
        (rental-start (unwrap! (get rental-start bike) err-no-active-rental))
        (rental-duration (- block-height rental-start))
        (rate (default-to (var-get rental-fee-per-block) (get custom-rate bike)))
    )
        (ok (* rental-duration rate))
    )
)

;; Admin functions
(define-public (set-rental-fee-per-block (new-fee uint))
    (if (is-eq tx-sender contract-owner)
        (begin
            (var-set rental-fee-per-block new-fee)
            (ok true)
        )
        err-owner-only
    )
)

(define-public (set-deposit-amount (new-deposit uint))
    (if (is-eq tx-sender contract-owner)
        (begin
            (var-set deposit-amount new-deposit)
            (ok true)
        )
        err-owner-only
    )
)
```