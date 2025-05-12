;; Capacity Booking Contract
;; Manages reservation of resources

(define-map bookings
  { booking-id: uint }
  {
    resource-id: uint,
    booker: principal,
    resource-owner: principal,
    start-time: uint,
    end-time: uint,
    units-booked: uint,
    total-cost: uint,
    status: uint,  ;; 1 = Pending, 2 = Confirmed, 3 = Completed, 4 = Cancelled
    settlement-id: (optional uint)
  }
)

(define-data-var booking-counter uint u0)

(define-public (book-capacity
    (resource-id uint)
    (resource-owner principal)
    (capacity uint)
    (rate-per-unit uint)
    (start-time uint)
    (end-time uint)
    (units-booked uint))
  (let (
    (caller tx-sender)
    (booking-id (+ (var-get booking-counter) u1))
  )
    ;; Check if booking time is valid
    (asserts! (> end-time start-time) (err u4))
    ;; Check if requested capacity is available
    (asserts! (<= units-booked capacity) (err u3))

    ;; Calculate total cost
    (let ((total-cost (* units-booked rate-per-unit)))
      (var-set booking-counter booking-id)
      (map-set bookings
        { booking-id: booking-id }
        {
          resource-id: resource-id,
          booker: caller,
          resource-owner: resource-owner,
          start-time: start-time,
          end-time: end-time,
          units-booked: units-booked,
          total-cost: total-cost,
          status: u1,  ;; Pending
          settlement-id: none
        }
      )
      (ok booking-id)
    )
  )
)

(define-public (confirm-booking (booking-id uint))
  (let ((caller tx-sender))
    (match (map-get? bookings { booking-id: booking-id })
      booking-data (begin
        ;; Check if caller is the resource owner
        (asserts! (is-eq caller (get resource-owner booking-data)) (err u6))
        ;; Check if booking is pending
        (asserts! (is-eq (get status booking-data) u1) (err u7))

        (map-set bookings
          { booking-id: booking-id }
          (merge booking-data { status: u2 })  ;; Confirmed
        )
        (ok true)
      )
      (err u8)
    )
  )
)

(define-public (cancel-booking (booking-id uint))
  (let ((caller tx-sender))
    (match (map-get? bookings { booking-id: booking-id })
      booking-data (begin
        ;; Check if caller is the booker or resource owner
        (asserts! (or
          (is-eq caller (get booker booking-data))
          (is-eq caller (get resource-owner booking-data))
        ) (err u9))

        ;; Check if booking is not already completed
        (asserts! (not (is-eq (get status booking-data) u3)) (err u10))

        (map-set bookings
          { booking-id: booking-id }
          (merge booking-data { status: u4 })  ;; Cancelled
        )
        (ok true)
      )
      (err u8)
    )
  )
)

(define-public (update-booking-status (booking-id uint) (status uint))
  (let ((caller tx-sender))
    (match (map-get? bookings { booking-id: booking-id })
      booking-data (begin
        ;; Check if caller is the resource owner
        (asserts! (is-eq caller (get resource-owner booking-data)) (err u6))

        (map-set bookings
          { booking-id: booking-id }
          (merge booking-data { status: status })
        )
        (ok true)
      )
      (err u8)
    )
  )
)

(define-public (update-settlement-id (booking-id uint) (settlement-id uint))
  (let ((caller tx-sender))
    (match (map-get? bookings { booking-id: booking-id })
      booking-data (begin
        ;; Check if caller is the resource owner
        (asserts! (is-eq caller (get resource-owner booking-data)) (err u6))

        (map-set bookings
          { booking-id: booking-id }
          (merge booking-data { settlement-id: (some settlement-id) })
        )
        (ok true)
      )
      (err u8)
    )
  )
)

(define-read-only (get-booking (booking-id uint))
  (map-get? bookings { booking-id: booking-id })
)

(define-read-only (get-booking-count)
  (var-get booking-counter)
)
