;; Utilization Tracking Contract
;; Monitors resource usage

(define-map utilization-records
  { record-id: uint }
  {
    booking-id: uint,
    resource-owner: principal,
    booker: principal,
    actual-start-time: uint,
    actual-end-time: uint,
    units-used: uint,
    status: uint,  ;; 1 = In Progress, 2 = Completed
    notes: (string-utf8 200)
  }
)

(define-data-var record-counter uint u0)

(define-public (start-utilization
    (booking-id uint)
    (resource-owner principal)
    (booker principal)
    (units-used uint)
    (notes (string-utf8 200)))
  (let (
    (caller tx-sender)
    (record-id (+ (var-get record-counter) u1))
  )
    ;; Check if caller is the resource owner
    (asserts! (is-eq caller resource-owner) (err u2))

    (var-set record-counter record-id)
    (map-set utilization-records
      { record-id: record-id }
      {
        booking-id: booking-id,
        resource-owner: resource-owner,
        booker: booker,
        actual-start-time: block-height,
        actual-end-time: u0,  ;; Will be set when completed
        units-used: units-used,
        status: u1,  ;; In Progress
        notes: notes
      }
    )
    (ok record-id)
  )
)

(define-public (complete-utilization
    (record-id uint)
    (units-used uint)
    (notes (string-utf8 200)))
  (let ((caller tx-sender))
    (match (map-get? utilization-records { record-id: record-id })
      record-data (begin
        ;; Check if record is in progress
        (asserts! (is-eq (get status record-data) u1) (err u5))

        ;; Check if caller is the resource owner
        (asserts! (is-eq caller (get resource-owner record-data)) (err u2))

        (map-set utilization-records
          { record-id: record-id }
          (merge record-data {
            actual-end-time: block-height,
            units-used: units-used,
            status: u2,  ;; Completed
            notes: notes
          })
        )

        (ok true)
      )
      (err u6)
    )
  )
)

(define-read-only (get-utilization-record (record-id uint))
  (map-get? utilization-records { record-id: record-id })
)

(define-read-only (get-record-count)
  (var-get record-counter)
)
