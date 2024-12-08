;; Define data variables
(define-data-var rental-fee uint u1000) ;; in microSTX
(define-data-var deposit-amount uint u5000) ;; in microSTX
(define-map bikes principal
    {
        available: bool,
        renter: (optional principal),
        rental-start: (optional uint)
    }
)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-bike-not-available (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-no-active-rental (err u103))
(define-constant err-unauthorized (err u104))

;; Public functions
(define-public (register-bike)
    (if (is-eq tx-sender contract-owner)
        (begin
            (map-set bikes tx-sender {
                available: true,
                renter: none,
                rental-start: none
            })
            (ok true)
        )
        err-owner-only
    )
)

(define-public (rent-bike (bike-owner principal))
    (let (
        (bike (unwrap! (map-get? bikes bike-owner) err-bike-not-available))
        (total-cost (+ (var-get rental-fee) (var-get deposit-amount)))
    )
        (asserts! (get available bike) err-bike-not-available)
        (try! (stx-transfer? total-cost tx-sender contract-owner))
        (map-set bikes bike-owner {
            available: false,
            renter: (some tx-sender),
            rental-start: (some block-height)
        })
        (ok true)
    )
)

(define-public (return-bike (bike-owner principal))
    (let (
        (bike (unwrap! (map-get? bikes bike-owner) err-bike-not-available))
    )
        (asserts! (is-eq (some tx-sender) (get renter bike)) err-unauthorized)
        (try! (stx-transfer? (var-get deposit-amount) contract-owner tx-sender))
        (map-set bikes bike-owner {
            available: true,
            renter: none,
            rental-start: none
        })
        (ok true)
    )
)

;; Read only functions
(define-read-only (get-bike-status (bike-owner principal))
    (ok (map-get? bikes bike-owner))
)

(define-read-only (get-rental-fee)
    (ok (var-get rental-fee))
)

(define-read-only (get-deposit-amount)
    (ok (var-get deposit-amount))
)

;; Admin functions
(define-public (set-rental-fee (new-fee uint))
    (if (is-eq tx-sender contract-owner)
        (begin
            (var-set rental-fee new-fee)
            (ok true)
        )
        err-owner-only
    )
)
