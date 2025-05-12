;; Entity Verification Contract
;; Validates supply chain participants

(define-data-var admin principal tx-sender)

;; Entity types: 1 = Manufacturer, 2 = Distributor, 3 = Retailer, 4 = Logistics Provider
(define-map entities
  { entity-id: principal }
  {
    name: (string-utf8 100),
    entity-type: uint,
    verified: bool,
    registration-time: uint
  }
)

(define-public (register-entity (name (string-utf8 100)) (entity-type uint))
  (let ((caller tx-sender))
    (asserts! (and (>= entity-type u1) (<= entity-type u4)) (err u1))
    (asserts! (is-none (map-get? entities { entity-id: caller })) (err u2))

    (map-set entities
      { entity-id: caller }
      {
        name: name,
        entity-type: entity-type,
        verified: false,
        registration-time: block-height
      }
    )
    (ok true)
  )
)

(define-public (verify-entity (entity principal))
  (let ((caller tx-sender))
    (asserts! (is-eq caller (var-get admin)) (err u3))
    (match (map-get? entities { entity-id: entity })
      entity-data (begin
        (map-set entities
          { entity-id: entity }
          (merge entity-data { verified: true })
        )
        (ok true)
      )
      (err u4)
    )
  )
)

(define-read-only (get-entity (entity principal))
  (map-get? entities { entity-id: entity })
)

(define-read-only (is-verified (entity principal))
  (match (map-get? entities { entity-id: entity })
    entity-data (ok (get verified entity-data))
    (err u4)
  )
)

(define-public (set-admin (new-admin principal))
  (let ((caller tx-sender))
    (asserts! (is-eq caller (var-get admin)) (err u3))
    (var-set admin new-admin)
    (ok true)
  )
)
