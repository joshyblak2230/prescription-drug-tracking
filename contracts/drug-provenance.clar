;; Prescription Drug Tracking - Drug Provenance Contract
;; Pharmaceutical supply chain tracking to prevent counterfeit drugs and ensure authenticity

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-DRUG-NOT-FOUND (err u101))
(define-constant ERR-INVALID-BATCH (err u102))
(define-constant ERR-ALREADY-EXISTS (err u103))
(define-constant ERR-INVALID-PARAMETERS (err u104))
(define-constant ERR-SUPPLY-CHAIN-ERROR (err u105))

;; Data Variables
(define-data-var contract-paused bool false)
(define-data-var next-drug-id uint u1)
(define-data-var next-batch-id uint u1)
(define-data-var total-drugs-registered uint u0)
(define-data-var total-batches-tracked uint u0)

;; Data Maps
(define-map drugs
    { drug-id: uint }
    {
        drug-name: (string-ascii 100),
        manufacturer: principal,
        drug-code: (string-ascii 50),
        active-ingredient: (string-ascii 100),
        strength: (string-ascii 20),
        dosage-form: (string-ascii 30),
        registered-date: uint,
        is-active: bool
    }
)

(define-map drug-batches
    { batch-id: uint }
    {
        drug-id: uint,
        batch-number: (string-ascii 50),
        manufacturing-date: uint,
        expiry-date: uint,
        quantity: uint,
        batch-hash: (buff 32),
        current-location: (string-ascii 100),
        current-owner: principal,
        is-authentic: bool,
        verification-count: uint
    }
)

(define-map authorized-entities
    { entity: principal }
    {
        entity-type: (string-ascii 20),
        license-number: (string-ascii 30),
        authorized: bool,
        registration-date: uint
    }
)

(define-map supply-chain-events
    { batch-id: uint, event-id: uint }
    {
        event-type: (string-ascii 30),
        from-entity: principal,
        to-entity: principal,
        location: (string-ascii 100),
        timestamp: uint,
        quantity: uint,
        notes: (string-ascii 200)
    }
)

(define-map batch-verifications
    { batch-id: uint, verifier: principal }
    {
        verification-date: uint,
        is-authentic: bool,
        verification-method: (string-ascii 50),
        notes: (string-ascii 200)
    }
)

;; Public Functions

;; Register authorized entity
(define-public (register-entity
    (entity-type (string-ascii 20))
    (license-number (string-ascii 30)))
    (begin
        (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
        
        (map-set authorized-entities
            { entity: tx-sender }
            {
                entity-type: entity-type,
                license-number: license-number,
                authorized: true,
                registration-date: block-height
            }
        )
        
        (ok true)
    )
)

;; Register new drug
(define-public (register-drug
    (drug-name (string-ascii 100))
    (drug-code (string-ascii 50))
    (active-ingredient (string-ascii 100))
    (strength (string-ascii 20))
    (dosage-form (string-ascii 30)))
    (let (
        (drug-id (var-get next-drug-id))
        (entity-info (unwrap! (map-get? authorized-entities { entity: tx-sender }) ERR-NOT-AUTHORIZED))
    )
        (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (get authorized entity-info) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get entity-type entity-info) "manufacturer") ERR-NOT-AUTHORIZED)
        
        (map-set drugs
            { drug-id: drug-id }
            {
                drug-name: drug-name,
                manufacturer: tx-sender,
                drug-code: drug-code,
                active-ingredient: active-ingredient,
                strength: strength,
                dosage-form: dosage-form,
                registered-date: block-height,
                is-active: true
            }
        )
        
        (var-set next-drug-id (+ drug-id u1))
        (var-set total-drugs-registered (+ (var-get total-drugs-registered) u1))
        
        (ok drug-id)
    )
)

;; Create drug batch
(define-public (create-batch
    (drug-id uint)
    (batch-number (string-ascii 50))
    (manufacturing-date uint)
    (expiry-date uint)
    (quantity uint)
    (batch-hash (buff 32)))
    (let (
        (batch-id (var-get next-batch-id))
        (drug (unwrap! (map-get? drugs { drug-id: drug-id }) ERR-DRUG-NOT-FOUND))
        (entity-info (unwrap! (map-get? authorized-entities { entity: tx-sender }) ERR-NOT-AUTHORIZED))
    )
        (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (get authorized entity-info) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get manufacturer drug) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active drug) ERR-DRUG-NOT-FOUND)
        (asserts! (> quantity u0) ERR-INVALID-PARAMETERS)
        (asserts! (> expiry-date manufacturing-date) ERR-INVALID-PARAMETERS)
        
        (map-set drug-batches
            { batch-id: batch-id }
            {
                drug-id: drug-id,
                batch-number: batch-number,
                manufacturing-date: manufacturing-date,
                expiry-date: expiry-date,
                quantity: quantity,
                batch-hash: batch-hash,
                current-location: "manufacturer",
                current-owner: tx-sender,
                is-authentic: true,
                verification-count: u0
            }
        )
        
        ;; Create initial supply chain event
        (map-set supply-chain-events
            { batch-id: batch-id, event-id: u0 }
            {
                event-type: "manufacturing",
                from-entity: tx-sender,
                to-entity: tx-sender,
                location: "manufacturing-facility",
                timestamp: block-height,
                quantity: quantity,
                notes: "Batch manufactured"
            }
        )
        
        (var-set next-batch-id (+ batch-id u1))
        (var-set total-batches-tracked (+ (var-get total-batches-tracked) u1))
        
        (ok batch-id)
    )
)

;; Transfer batch in supply chain
(define-public (transfer-batch
    (batch-id uint)
    (to-entity principal)
    (location (string-ascii 100))
    (quantity uint)
    (notes (string-ascii 200)))
    (let (
        (batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) ERR-INVALID-BATCH))
        (from-entity-info (unwrap! (map-get? authorized-entities { entity: tx-sender }) ERR-NOT-AUTHORIZED))
        (to-entity-info (unwrap! (map-get? authorized-entities { entity: to-entity }) ERR-NOT-AUTHORIZED))
        (event-id (get verification-count batch))
    )
        (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (get authorized from-entity-info) ERR-NOT-AUTHORIZED)
        (asserts! (get authorized to-entity-info) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get current-owner batch) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (<= quantity (get quantity batch)) ERR-INVALID-PARAMETERS)
        
        ;; Update batch ownership and location
        (map-set drug-batches
            { batch-id: batch-id }
            (merge batch {
                current-location: location,
                current-owner: to-entity,
                verification-count: (+ event-id u1)
            })
        )
        
        ;; Record supply chain event
        (map-set supply-chain-events
            { batch-id: batch-id, event-id: (+ event-id u1) }
            {
                event-type: "transfer",
                from-entity: tx-sender,
                to-entity: to-entity,
                location: location,
                timestamp: block-height,
                quantity: quantity,
                notes: notes
            }
        )
        
        (ok true)
    )
)

;; Verify batch authenticity
(define-public (verify-batch
    (batch-id uint)
    (is-authentic bool)
    (verification-method (string-ascii 50))
    (notes (string-ascii 200)))
    (let (
        (batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) ERR-INVALID-BATCH))
        (entity-info (unwrap! (map-get? authorized-entities { entity: tx-sender }) ERR-NOT-AUTHORIZED))
    )
        (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (get authorized entity-info) ERR-NOT-AUTHORIZED)
        
        ;; Record verification
        (map-set batch-verifications
            { batch-id: batch-id, verifier: tx-sender }
            {
                verification-date: block-height,
                is-authentic: is-authentic,
                verification-method: verification-method,
                notes: notes
            }
        )
        
        ;; Update batch authenticity status
        (map-set drug-batches
            { batch-id: batch-id }
            (merge batch {
                is-authentic: is-authentic,
                verification-count: (+ (get verification-count batch) u1)
            })
        )
        
        (ok true)
    )
)

;; Report counterfeit drug
(define-public (report-counterfeit (batch-id uint) (report-details (string-ascii 200)))
    (let (
        (batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) ERR-INVALID-BATCH))
        (entity-info (unwrap! (map-get? authorized-entities { entity: tx-sender }) ERR-NOT-AUTHORIZED))
    )
        (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (get authorized entity-info) ERR-NOT-AUTHORIZED)
        
        ;; Mark batch as not authentic
        (map-set drug-batches
            { batch-id: batch-id }
            (merge batch { is-authentic: false })
        )
        
        ;; Record counterfeit report
        (map-set batch-verifications
            { batch-id: batch-id, verifier: tx-sender }
            {
                verification-date: block-height,
                is-authentic: false,
                verification-method: "counterfeit-report",
                notes: report-details
            }
        )
        
        (ok true)
    )
)

;; Emergency pause contract
(define-public (pause-contract)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set contract-paused true)
        (ok true)
    )
)

;; Resume contract
(define-public (resume-contract)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set contract-paused false)
        (ok true)
    )
)

;; Read-only Functions

;; Get drug information
(define-read-only (get-drug (drug-id uint))
    (map-get? drugs { drug-id: drug-id })
)

;; Get batch information
(define-read-only (get-batch (batch-id uint))
    (map-get? drug-batches { batch-id: batch-id })
)

;; Get entity information
(define-read-only (get-entity-info (entity principal))
    (map-get? authorized-entities { entity: entity })
)

;; Get supply chain event
(define-read-only (get-supply-chain-event (batch-id uint) (event-id uint))
    (map-get? supply-chain-events { batch-id: batch-id, event-id: event-id })
)

;; Get batch verification
(define-read-only (get-batch-verification (batch-id uint) (verifier principal))
    (map-get? batch-verifications { batch-id: batch-id, verifier: verifier })
)

;; Get platform statistics
(define-read-only (get-platform-stats)
    {
        total-drugs: (var-get total-drugs-registered),
        total-batches: (var-get total-batches-tracked),
        next-drug-id: (var-get next-drug-id),
        next-batch-id: (var-get next-batch-id),
        contract-paused: (var-get contract-paused)
    }
)


;; title: drug-provenance
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

