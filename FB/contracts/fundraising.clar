;; Fundraising Benefit Contract
;; Allows users to create and participate in fundraising benefits
;; with secure fund handling and robust error checking

;; Error codes
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-BENEFIT-ENDED (err u101))
(define-constant ERR-BENEFIT-NOT-ENDED (err u102))
(define-constant ERR-INSUFFICIENT-CONTRIBUTION (err u103))
(define-constant ERR-NO-BENEFIT (err u104))
(define-constant ERR-ALREADY-FINALIZED (err u105))
(define-constant ERR-TRANSFER-FAILED (err u106))
(define-constant ERR-INVALID-TIMEFRAME (err u200))
(define-constant ERR-INVALID-MINIMUM-AMOUNT (err u201))
(define-constant ERR-INVALID-INPUT (err u202))

;; Data variables
(define-data-var admin-principal principal tx-sender)
(define-data-var nonprofit-address principal tx-sender)
(define-data-var benefit-counter uint u0)

;; Define benefit type and initialize with default values
(define-data-var benefit-type 
    {
        offering-name: (string-ascii 50),
        offering-details: (string-ascii 256),
        organizer: principal,
        begin-block: uint,
        finish-block: uint,
        minimum-amount: uint,
        top-contribution: uint,
        top-contributor: (optional principal),
        recipient: principal,
        finalized: bool
    }
    {
        offering-name: "",
        offering-details: "",
        organizer: tx-sender,
        begin-block: u0,
        finish-block: u0,
        minimum-amount: u0,
        top-contribution: u0,
        top-contributor: none,
        recipient: tx-sender,
        finalized: false
    }
)

;; Benefit status
(define-map benefits
    uint
    {
        offering-name: (string-ascii 50),
        offering-details: (string-ascii 256),
        organizer: principal,
        begin-block: uint,
        finish-block: uint,
        minimum-amount: uint,
        top-contribution: uint,
        top-contributor: (optional principal),
        recipient: principal,
        finalized: bool
    }
)

;; Contribution tracking
(define-map participant-contributions
    { benefit-id: uint, participant: principal }
    uint
)

;; Read-only functions

(define-read-only (get-benefit (benefit-id uint))
    (ok (unwrap! (map-get? benefits benefit-id) (err ERR-NO-BENEFIT)))
)

(define-read-only (get-participant-contribution (benefit-id uint) (participant principal))
    (ok (default-to u0
        (map-get? participant-contributions { benefit-id: benefit-id, participant: participant }))
    )
)

(define-read-only (is-benefit-active (benefit-id uint))
    (match (map-get? benefits benefit-id)
        benefit (ok (and 
            (>= block-height (get begin-block benefit))
            (<= block-height (get finish-block benefit))
        ))
        (err ERR-NO-BENEFIT)
    )
)

;; Public functions

(define-public (create-benefit (offering-name (string-ascii 50)) 
                             (offering-details (string-ascii 256))
                             (blocks-timeframe uint)
                             (minimum-amount uint)
                             (recipient principal))
    (let
        (
            (benefit-id (var-get benefit-counter))
            (begin-block block-height)
            (finish-block (+ block-height blocks-timeframe))
        )
        (asserts! (> blocks-timeframe u0) (err ERR-INVALID-TIMEFRAME))
        (asserts! (>= minimum-amount u0) (err ERR-INVALID-MINIMUM-AMOUNT))
        (asserts! (is-valid-string-ascii offering-name) (err ERR-INVALID-INPUT))
        (asserts! (is-valid-string-ascii offering-details) (err ERR-INVALID-INPUT))
        (asserts! (is-valid-principal recipient) (err ERR-INVALID-INPUT))
        
        (map-set benefits benefit-id
            {
                offering-name: offering-name,
                offering-details: offering-details,
                organizer: tx-sender,
                begin-block: begin-block,
                finish-block: finish-block,
                minimum-amount: minimum-amount,
                top-contribution: u0,
                top-contributor: none,
                recipient: recipient,
                finalized: false
            }
        )
        
        (var-set benefit-counter (+ benefit-id u1))
        (ok benefit-id)
    )
)

(define-public (make-contribution (benefit-id uint))
    (let
        (
            (contribution-amount (stx-get-balance tx-sender))
            (benefit (unwrap! (map-get? benefits benefit-id) (err ERR-NO-BENEFIT)))
            (current-top-contribution (get top-contribution benefit))
        )
        (asserts! (unwrap! (is-benefit-active benefit-id) (err ERR-NO-BENEFIT)) (err ERR-BENEFIT-ENDED))
        (asserts! (> contribution-amount current-top-contribution) (err ERR-INSUFFICIENT-CONTRIBUTION))
        (asserts! (>= contribution-amount (get minimum-amount benefit)) (err ERR-INSUFFICIENT-CONTRIBUTION))
        
        ;; Handle previous contribution refund if exists
        (and
            (match (get top-contributor benefit)
                prev-contributor (unwrap! (stx-transfer? current-top-contribution tx-sender prev-contributor) (err ERR-TRANSFER-FAILED))
                true
            )
            
            ;; Update benefit state
            (map-set benefits benefit-id
                (merge benefit
                    {
                        top-contribution: contribution-amount,
                        top-contributor: (some tx-sender)
                    }
                )
            )
            
            ;; Track participant contribution
            (map-set participant-contributions
                { benefit-id: benefit-id, participant: tx-sender }
                contribution-amount
            )
        )
        
        (ok true)
    )
)

(define-public (finalize-benefit (benefit-id uint))
    (let
        (
            (benefit (unwrap! (map-get? benefits benefit-id) (err ERR-NO-BENEFIT)))
        )
        (asserts! (>= block-height (get finish-block benefit)) (err ERR-BENEFIT-NOT-ENDED))
        (asserts! (not (get finalized benefit)) (err ERR-ALREADY-FINALIZED))
        
        ;; Transfer top contribution to recipient
        (and
            (match (get top-contributor benefit)
                winner (unwrap! (stx-transfer? (get top-contribution benefit) winner (get recipient benefit)) (err ERR-TRANSFER-FAILED))
                true
            )
            
            ;; Mark benefit as finalized
            (map-set benefits benefit-id
                (merge benefit { finalized: true })
            )
        )
        
        (ok true)
    )
)

;; Administrative functions

(define-public (set-nonprofit-address (new-address principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin-principal)) (err ERR-UNAUTHORIZED))
        (asserts! (is-valid-principal new-address) (err ERR-INVALID-INPUT))
        (var-set nonprofit-address new-address)
        (ok true)
    )
)

(define-public (transfer-admin-rights (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin-principal)) (err ERR-UNAUTHORIZED))
        (asserts! (is-valid-principal new-admin) (err ERR-INVALID-INPUT))
        (var-set admin-principal new-admin)
        (ok true)
    )
)

;; Helper functions

(define-private (is-valid-string-ascii (value (string-ascii 256)))
    (and
        (>= (len value) u1)
        (<= (len value) u256)
    )
)

(define-private (is-valid-principal (value principal))
    (not (is-eq value 'SP000000000000000000002Q6VF78))
)