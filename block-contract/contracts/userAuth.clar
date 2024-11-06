;; Define constants for user roles
(define-constant ROLE_SHIPPER u1)
(define-constant ROLE_CARRIER u2)
(define-constant ROLE_DRIVER u3)
(define-constant ROLE_CUSTOMER u4)

;; Define data maps
(define-map Users principal 
  {
    role: uint,
    registered: bool,
    profile: (optional {
      name: (string-utf8 50),
      company: (optional (string-utf8 100)),
      service-type: (optional (string-utf8 50))
    })
  }
)

(define-map Permissions uint (list 10 (string-ascii 24)))

;; Define public functions

;; Register a new user
(define-public (register (role uint))
  (let ((caller tx-sender))
    (asserts! (is-none (get role (map-get? Users caller))) (err u1)) ;; User not already registered
    (asserts! (and (>= role ROLE_SHIPPER) (<= role ROLE_CUSTOMER)) (err u2)) ;; Valid role
    (ok (map-set Users caller {role: role, registered: true, profile: none}))
  )
)

;; Set up user profile
(define-public (set-profile (name (string-utf8 50)) (company (optional (string-utf8 100))) (service-type (optional (string-utf8 50))))
  (let ((caller tx-sender))
    (asserts! (is-some (map-get? Users caller)) (err u3)) ;; User must be registered
    (ok (map-set Users caller 
      (merge (unwrap-panic (map-get? Users caller))
        {profile: (some {name: name, company: company, service-type: service-type})}
      )
    ))
  )
)

;; Check if a user is registered
(define-read-only (is-registered (user principal))
  (default-to false (get registered (map-get? Users user)))
)

;; Get user role
(define-read-only (get-user-role (user principal))
  (get role (map-get? Users user))
)

;; Get user profile
(define-read-only (get-user-profile (user principal))
  (get profile (map-get? Users user))
)

;; Set permissions for a role
(define-public (set-role-permissions (role uint) (permissions (list 10 (string-ascii 20))))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u4)) ;; Only contract owner can set permissions
    (ok (map-set Permissions role permissions))
  )
)

;; Check if a user has a specific permission
(define-read-only (has-permission (user principal) (permission (string-ascii 20)))
  (let ((user-role (unwrap-panic (get-user-role user))))
    (is-some (index-of (default-to (list) (map-get? Permissions user-role)) permission))
  )
)

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Initialize contract
(begin
  (map-set Permissions ROLE_SHIPPER (list "create-shipment" "view-shipments" "edit-profile"))
  (map-set Permissions ROLE_CARRIER (list "accept-shipment" "update-shipment" "view-shipments" "edit-profile"))
  (map-set Permissions ROLE_DRIVER (list "update-shipment" "view-assigned-shipments" "edit-profile"))
  (map-set Permissions ROLE_CUSTOMER (list "create-shipment" "view-own-shipments" "edit-profile"))
)