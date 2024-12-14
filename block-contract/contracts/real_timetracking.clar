;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-shipment-not-found (err u101))

;; Define data map
(define-map shipments
  { shipment-id: uint }
  { location: (string-ascii 50),
    status: (string-ascii 20),
    last-update: uint })
;; Define functions

;; Register a new shipment
(define-public (register-shipment (shipment-id uint) (initial-location (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (ok (map-set shipments 
                 { shipment-id: shipment-id }
                 { location: initial-location, 
                   status: "In Transit", 
                   last-update: stacks-block-height }))))


;; Update shipment location
(define-public (update-location (shipment-id uint) (new-location (string-ascii 50)))
  (let ((current-shipment (unwrap! (map-get? shipments {shipment-id: shipment-id}) err-shipment-not-found)))
    (begin
      (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
      (ok (map-set shipments {shipment-id: shipment-id}
                   (merge current-shipment 
                          {location: new-location, 
                           last-update: stacks-block-height}))))))

;; Update shipment status
(define-public (update-status (shipment-id uint) (new-status (string-ascii 20)))
  (let ((current-shipment (unwrap! (map-get? shipments {shipment-id: shipment-id}) err-shipment-not-found)))
    (begin
      (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
      (ok (map-set shipments {shipment-id: shipment-id}
                   (merge current-shipment 
                          {status: new-status, 
                           last-update: stacks-block-height}))))))

;; Get shipment information
(define-read-only (get-shipment-info (shipment-id uint))
  (map-get? shipments {shipment-id: shipment-id}))

;; Check for alerts (simplified version)
(define-read-only (check-for-alerts (shipment-id uint))
  (let ((shipment (unwrap! (map-get? shipments {shipment-id: shipment-id}) err-shipment-not-found)))
    (if (is-eq (get status shipment) "Delayed")
        (ok "Alert: Shipment is delayed")
        (ok "No alerts"))))