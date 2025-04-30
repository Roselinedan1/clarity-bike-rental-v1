;; maintenance-tracking.clar
;; 
;; This smart contract manages the maintenance tracking system for a bike rental platform.
;; It allows renters to report issues with bikes and owners to document repairs.
;; Each bike maintains a transparent maintenance history that is visible to potential renters,
;; helping them make informed decisions about which bikes to rent.
;; The system also flags bikes that require urgent maintenance, temporarily removing them
;; from the available pool until issues are resolved.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-BIKE-NOT-FOUND (err u101))
(define-constant ERR-INVALID-STATUS (err u102))
(define-constant ERR-REPORT-NOT-FOUND (err u103))
(define-constant ERR-REPORT-ALREADY-RESOLVED (err u104))
(define-constant ERR-EMPTY-DESCRIPTION (err u105))

;; Maintenance status constants
(define-constant STATUS-NORMAL u1)
(define-constant STATUS-NEEDS-MAINTENANCE u2)
(define-constant STATUS-UNDER-MAINTENANCE u3)

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data structures

;; Stores the maintenance status for each bike
;; {bike-id: principal} -> {status: uint, last-maintenance: uint}
(define-map bikes-maintenance
  principal
  {
    status: uint,             ;; Current maintenance status
    last-maintenance: uint    ;; Block height of last maintenance
  }
)

;; Tracks maintenance reports for each bike
;; {bike-id: principal, report-id: uint} -> {reporter: principal, description: string, timestamp: uint, resolved: bool, resolution-details: (optional string)}
(define-map maintenance-reports
  {bike-id: principal, report-id: uint}
  {
    reporter: principal,               ;; Person who reported the issue
    description: string-utf8,          ;; Description of the issue
    timestamp: uint,                   ;; Block height when reported
    resolved: bool,                    ;; Whether the issue has been resolved
    resolution-details: (optional string-utf8)  ;; Details about resolution if resolved
  }
)

;; Counter for report IDs
(define-data-var next-report-id uint u1)

;; Track the number of reports per bike
(define-map bike-report-count principal uint)

;; Private functions

;; Get the current report count for a bike
(define-private (get-bike-report-count (bike-id principal))
  (default-to u0 (map-get? bike-report-count bike-id))
)

;; Increment the report count for a bike
(define-private (increment-report-count (bike-id principal))
  (let ((current-count (get-bike-report-count bike-id)))
    (map-set bike-report-count bike-id (+ current-count u1))
    (+ current-count u1)
  )
)

;; Check if caller is bike owner
;; Only the bike owner should be able to resolve maintenance issues
(define-private (is-bike-owner (bike-id principal))
  (is-eq tx-sender bike-id)
)

;; Get the next report ID and increment the counter
(define-private (get-next-report-id)
  (let ((current-id (var-get next-report-id)))
    (var-set next-report-id (+ current-id u1))
    current-id
  )
)

;; Register a bike in the maintenance system
(define-private (register-bike (bike-id principal))
  (match (map-get? bikes-maintenance bike-id)
    existing-entry existing-entry
    (map-set bikes-maintenance bike-id 
      {
        status: STATUS-NORMAL,
        last-maintenance: block-height
      }
    )
  )
)

;; Read-only functions

;; Get the maintenance status of a bike
(define-read-only (get-bike-status (bike-id principal))
  (match (map-get? bikes-maintenance bike-id)
    maintenance-data (ok maintenance-data)
    (err ERR-BIKE-NOT-FOUND)
  )
)

;; Get details for a specific maintenance report
(define-read-only (get-maintenance-report (bike-id principal) (report-id uint))
  (match (map-get? maintenance-reports {bike-id: bike-id, report-id: report-id})
    report (ok report)
    (err ERR-REPORT-NOT-FOUND)
  )
)

;; Get the total number of reports for a bike
(define-read-only (get-total-reports-for-bike (bike-id principal))
  (ok (get-bike-report-count bike-id))
)

;; Check if a bike needs maintenance
(define-read-only (is-maintenance-required (bike-id principal))
  (match (map-get? bikes-maintenance bike-id)
    maintenance-data (ok (not (is-eq (get status maintenance-data) STATUS-NORMAL)))
    (err ERR-BIKE-NOT-FOUND)
  )
)

;; Public functions

;; Report a maintenance issue with a bike
(define-public (report-issue (bike-id principal) (description string-utf8))
  (let ((report-id (get-next-report-id)))
    ;; Check that description is not empty
    (asserts! (> (len description) u0) (err ERR-EMPTY-DESCRIPTION))
    
    ;; Ensure bike exists in the system or register it
    (register-bike bike-id)
    
    ;; Create the maintenance report
    (map-set maintenance-reports
      {bike-id: bike-id, report-id: report-id}
      {
        reporter: tx-sender,
        description: description,
        timestamp: block-height,
        resolved: false,
        resolution-details: none
      }
    )
    
    ;; Update the bike status if it's currently normal
    (match (map-get? bikes-maintenance bike-id)
      maintenance-data 
        (if (is-eq (get status maintenance-data) STATUS-NORMAL)
          (map-set bikes-maintenance bike-id 
            {
              status: STATUS-NEEDS-MAINTENANCE,
              last-maintenance: (get last-maintenance maintenance-data)
            }
          )
          true
        )
      false
    )
    
    ;; Increment the report count for this bike
    (increment-report-count bike-id)
    
    (ok report-id)
  )
)

;; Bike owner can mark a bike as under maintenance
(define-public (start-maintenance (bike-id principal))
  ;; Only the bike owner can start maintenance
  (asserts! (is-bike-owner bike-id) (err ERR-NOT-AUTHORIZED))
  
  ;; Ensure bike exists in the system
  (match (map-get? bikes-maintenance bike-id)
    maintenance-data 
      (begin
        (map-set bikes-maintenance bike-id 
          {
            status: STATUS-UNDER-MAINTENANCE,
            last-maintenance: (get last-maintenance maintenance-data)
          }
        )
        (ok true)
      )
    (err ERR-BIKE-NOT-FOUND)
  )
)

;; Bike owner can resolve a specific maintenance report
(define-public (resolve-report (bike-id principal) (report-id uint) (resolution-details string-utf8))
  ;; Only the bike owner can resolve reports
  (asserts! (is-bike-owner bike-id) (err ERR-NOT-AUTHORIZED))
  
  ;; Ensure resolution details are not empty
  (asserts! (> (len resolution-details) u0) (err ERR-EMPTY-DESCRIPTION))
  
  ;; Check that report exists and is not already resolved
  (match (map-get? maintenance-reports {bike-id: bike-id, report-id: report-id})
    report
      (begin
        ;; Check that report is not already resolved
        (asserts! (not (get resolved report)) (err ERR-REPORT-ALREADY-RESOLVED))
        
        ;; Update the report to mark it as resolved
        (map-set maintenance-reports
          {bike-id: bike-id, report-id: report-id}
          {
            reporter: (get reporter report),
            description: (get description report),
            timestamp: (get timestamp report),
            resolved: true,
            resolution-details: (some resolution-details)
          }
        )
        
        (ok true)
      )
    (err ERR-REPORT-NOT-FOUND)
  )
)

;; Bike owner can complete maintenance and return bike to normal status
(define-public (complete-maintenance (bike-id principal))
  ;; Only the bike owner can complete maintenance
  (asserts! (is-bike-owner bike-id) (err ERR-NOT-AUTHORIZED))
  
  ;; Ensure bike exists in the system
  (match (map-get? bikes-maintenance bike-id)
    maintenance-data 
      (begin
        ;; Update bike status to normal and record the maintenance timestamp
        (map-set bikes-maintenance bike-id 
          {
            status: STATUS-NORMAL,
            last-maintenance: block-height
          }
        )
        (ok true)
      )
    (err ERR-BIKE-NOT-FOUND)
  )
)