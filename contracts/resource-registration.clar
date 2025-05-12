;; Resource Registration Contract
;; Records available logistics assets

(define-map resources
  { resource-id: uint }
  {
    owner: principal,
    name: (string-utf8 100),
    resource-type: (string-utf8 50),
    capacity: uint,
    location: (string-utf8 100),
    available: bool,
    rate-per-unit: uint
  }
)

(define-data-var resource-counter uint u0)

(define-public (register-resource
    (name (string-utf8 100))
    (resource-type (string-utf8 50))
    (capacity uint)
    (location (string-utf8 100))
    (rate-per-unit uint))
  (let (
    (caller tx-sender)
    (resource-id (+ (var-get resource-counter) u1))
  )
    (var-set resource-counter resource-id)
    (map-set resources
      { resource-id: resource-id }
      {
        owner: caller,
        name: name,
        resource-type: resource-type,
        capacity: capacity,
        location: location,
        available: true,
        rate-per-unit: rate-per-unit
      }
    )
    (ok resource-id)
  )
)

(define-public (update-resource-availability (resource-id uint) (available bool))
  (let ((caller tx-sender))
    (match (map-get? resources { resource-id: resource-id })
      resource-data (begin
        (asserts! (is-eq caller (get owner resource-data)) (err u2))
        (map-set resources
          { resource-id: resource-id }
          (merge resource-data { available: available })
        )
        (ok true)
      )
      (err u3)
    )
  )
)

(define-public (update-resource-rate (resource-id uint) (rate-per-unit uint))
  (let ((caller tx-sender))
    (match (map-get? resources { resource-id: resource-id })
      resource-data (begin
        (asserts! (is-eq caller (get owner resource-data)) (err u2))
        (map-set resources
          { resource-id: resource-id }
          (merge resource-data { rate-per-unit: rate-per-unit })
        )
        (ok true)
      )
      (err u3)
    )
  )
)

(define-read-only (get-resource (resource-id uint))
  (map-get? resources { resource-id: resource-id })
)

(define-read-only (get-resource-count)
  (var-get resource-counter)
)
