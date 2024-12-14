;; Import constants from user-auth contract
(use-trait user-auth-trait .user-auth.user-auth-trait)

;; Constants for shipment status
(define-constant STATUS_CREATED u1)
(define-constant STATUS_IN_TRANSIT u2)
(define-constant STATUS_DELIVERED u3)
(define-constant STATUS_CANCELED u4)

;; Data structures
(define-map Shipments uint
  {
    shipper: principal,
    carrier: (optional principal),
    driver: (optional principal),
    origin: (string-utf8 100),
    destination: (string-utf8 100),
    estimated-delivery: uint,
    cargo-details: (string-utf8 500),
    status: uint,
    updates: (list 20 {timestamp: uint, status: uint, description: (string-utf8 200)})
  }
)

(define-data-var shipment-id-nonce uint u0)

;; Helper function to get a new shipment ID
(define-private (get-next-shipment-id)
  (let ((current-id (var-get shipment-id-nonce)))
    (var-set shipment-id-nonce (+ current-id u1))
    current-id
  )
)

;; Create a new shipment
(define-public (create-shipment
  (origin (string-utf8 100))
  (destination (string-utf8 100))
  (estimated-delivery uint)
  (cargo-details (string-utf8 500))
  (user-auth-contract <user-auth-trait>)
)
  (let
    (
      (caller tx-sender)
      (shipment-id (get-next-shipment-id))
    )
    (asserts! (contract-call? user-auth-contract has-permission caller "create-shipment") (err u1))
    (ok (map-set Shipments shipment-id
      {
        shipper: caller,
        carrier: none,
        driver: none,
        origin: origin,
        destination: destination,
        estimated-delivery: estimated-delivery,
        cargo-details: cargo-details,
        status: STATUS_CREATED,
        updates: (list {timestamp: block-height, status: STATUS_CREATED, description: "Shipment created"})
      }
    ))
  )
)

;; Assign carrier to a shipment
(define-public (assign-carrier (shipment-id uint) (carrier-address principal) (user-auth-contract <user-auth-trait>))
  (let
    (
      (shipment (unwrap! (map-get? Shipments shipment-id) (err u404)))
      (caller tx-sender)
    )
    (asserts! (is-eq (get shipper shipment) caller) (err u2))
    (asserts! (contract-call? user-auth-contract has-permission carrier-address "accept-shipment") (err u3))
    (ok (map-set Shipments shipment-id
      (merge shipment {carrier: (some carrier-address)})
    ))
  )
)

;; Assign driver to a shipment
(define-public (assign-driver (shipment-id uint) (driver-address principal) (user-auth-contract <user-auth-trait>))
  (let
    (
      (shipment (unwrap! (map-get? Shipments shipment-id) (err u404)))
      (caller tx-sender)
    )
    (asserts! (is-eq (get carrier shipment) (some caller)) (err u2))
    (asserts! (contract-call? user-auth-contract has-permission driver-address "update-shipment") (err u3))
    (ok (map-set Shipments shipment-id
      (merge shipment {driver: (some driver-address)})
    ))
  )
)

;; Update shipment status
(define-public (update-shipment-status (shipment-id uint) (new-status uint) (description (string-utf8 200)) (user-auth-contract <user-auth-trait>))
  (let
    (
      (shipment (unwrap! (map-get? Shipments shipment-id) (err u404)))
      (caller tx-sender)
    )
    (asserts! (or
      (is-eq (get carrier shipment) (some caller))
      (is-eq (get driver shipment) (some caller))
    ) (err u2))
    (asserts! (contract-call? user-auth-contract has-permission caller "update-shipment") (err u3))
    (asserts! (and (>= new-status STATUS_CREATED) (<= new-status STATUS_CANCELED)) (err u4))
    (ok (map-set Shipments shipment-id
      (merge shipment
        {
          status: new-status,
          updates: (unwrap-panic (as-max-len? (concat (get updates shipment) (list {timestamp: block-height, status: new-status, description: description})) u20))
        }
      )
    ))
  )
)

;; Get shipment details
(define-read-only (get-shipment (shipment-id uint))
  (map-get? Shipments shipment-id)
)

;; Get all shipments for a user (shipper or carrier)
(define-read-only (get-user-shipments (user principal))
  (filter
    (lambda (shipment-id)
      (let ((shipment (unwrap-panic (map-get? Shipments shipment-id))))
        (or
          (is-eq (get shipper shipment) user)
          (is-eq (get carrier shipment) (some user))
          (is-eq (get driver shipment) (some user))
        )
      )
    )
    (map-to-list Shipments)
  )
)