;; Settlement Contract
;; Handles payment for shared capacity

(define-map settlements
  { settlement-id: uint }
  {
    booking-id: uint,
    resource-owner: principal,
    booker: principal,
    units-used: uint,
    amount-due: uint,
    paid: bool,
    payment-time: (optional uint)
  }
)

(define-data-var settlement-counter uint u0)

(define-public (create-settlement
    (booking-id uint)
    (resource-owner principal)
    (booker principal)
    (units-used uint)
    (rate-per-unit uint))
  (let (
    (caller tx-sender)
    (settlement-id (+ (var-get settlement-counter) u1))
  )
    ;; Check if caller is authorized (resource owner)
    (asserts! (is-eq caller resource-owner) (err u1))

    ;; Calculate amount due based on actual usage
    (let ((amount-due (* units-used rate-per-unit)))
      (var-set settlement-counter settlement-id)
      (map-set settlements
        { settlement-id: settlement-id }
        {
          booking-id: booking-id,
          resource-owner: resource-owner,
          booker: booker,
          units-used: units-used,
          amount-due: amount-due,
          paid: false,
          payment-time: none
        }
      )

      (ok settlement-id)
    )
  )
)

(define-public (make-payment (settlement-id uint))
  (let ((caller tx-sender))
    (match (map-get? settlements { settlement-id: settlement-id })
      settlement-data (begin
        ;; Check if caller is the booker
        (asserts! (is-eq caller (get booker settlement-data)) (err u4))
        ;; Check if not already paid
        (asserts! (not (get paid settlement-data)) (err u5))

        ;; Transfer STX from booker to resource owner
        (try! (stx-transfer? (get amount-due settlement-data) caller (get resource-owner settlement-data)))

        ;; Update settlement as paid
        (map-set settlements
          { settlement-id: settlement-id }
          (merge settlement-data {
            paid: true,
            payment-time: (some block-height)
          })
        )

        (ok true)
      )
      (err u6)
    )
  )
)

(define-read-only (get-settlement (settlement-id uint))
  (map-get? settlements { settlement-id: settlement-id })
)

(define-read-only (get-settlement-count)
  (var-get settlement-counter)
)
