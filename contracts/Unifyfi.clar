(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))
(define-constant ERR_MEMBER_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_MEMBER (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_NO_DIVIDENDS (err u105))
(define-constant ERR_TRANSFER_FAILED (err u106))
(define-constant ERR_LOAN_NOT_FOUND (err u107))
(define-constant ERR_LOAN_ALREADY_EXISTS (err u108))
(define-constant ERR_INSUFFICIENT_COLLATERAL (err u109))
(define-constant ERR_LOAN_NOT_APPROVED (err u110))
(define-constant ERR_LOAN_ALREADY_APPROVED (err u111))
(define-constant ERR_INVALID_LOAN_TERMS (err u112))
(define-constant ERR_LOAN_OVERDUE (err u113))
(define-constant ERR_PAYMENT_TOO_LOW (err u114))

(define-data-var total-pool uint u0)
(define-data-var dividend-rate uint u500)
(define-data-var last-dividend-block uint u0)
(define-data-var total-members uint u0)
(define-data-var next-loan-id uint u1)
(define-data-var total-loans-issued uint u0)
(define-data-var total-active-loans uint u0)
(define-data-var loan-interest-rate uint u800)

(define-map members principal {
    contribution: uint,
    last-claim-block: uint,
    join-block: uint,
    is-active: bool
})

(define-map member-dividends principal uint)

(define-map contribution-history {member: principal, block: uint} uint)

(define-map loans uint {
    borrower: principal,
    amount: uint,
    interest-rate: uint,
    term-blocks: uint,
    issued-block: uint,
    repaid-amount: uint,
    is-approved: bool,
    is-active: bool,
    is-defaulted: bool
})

(define-map loan-applications principal {
    requested-amount: uint,
    term-blocks: uint,
    collateral-contribution: uint,
    application-block: uint,
    status: (string-ascii 20)
})

(define-map borrower-loans principal (list 10 uint))

(define-public (join-credit-union (initial-contribution uint))
    (let (
        (sender tx-sender)
        (existing-member (map-get? members sender))
    )
    (asserts! (is-none existing-member) ERR_ALREADY_MEMBER)
    (asserts! (> initial-contribution u0) ERR_INVALID_AMOUNT)
    
    (try! (stx-transfer? initial-contribution sender (as-contract tx-sender)))
    
    (map-set members sender {
        contribution: initial-contribution,
        last-claim-block: stacks-block-height,
        join-block: stacks-block-height,
        is-active: true
    })
    
    (map-set contribution-history {member: sender, block: stacks-block-height} initial-contribution)
    
    (var-set total-pool (+ (var-get total-pool) initial-contribution))
    (var-set total-members (+ (var-get total-members) u1))
    
    (ok true)
    )
)

(define-public (contribute (amount uint))
    (let (
        (sender tx-sender)
        (member-data (unwrap! (map-get? members sender) ERR_MEMBER_NOT_FOUND))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (get is-active member-data) ERR_NOT_AUTHORIZED)
    
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    
    (map-set members sender (merge member-data {
        contribution: (+ (get contribution member-data) amount)
    }))
    
    (map-set contribution-history {member: sender, block: stacks-block-height} amount)
    
    (var-set total-pool (+ (var-get total-pool) amount))
    
    (ok true)
    )
)

(define-public (calculate-dividends (member principal))
    (let (
        (member-data (unwrap! (map-get? members member) ERR_MEMBER_NOT_FOUND))
        (member-contribution (get contribution member-data))
        (last-claim (get last-claim-block member-data))
        (blocks-since-claim (- stacks-block-height last-claim))
        (pool-total (var-get total-pool))
        (rate (var-get dividend-rate))
    )
    (asserts! (get is-active member-data) ERR_NOT_AUTHORIZED)
    (asserts! (> pool-total u0) ERR_NO_DIVIDENDS)
    
    (let (
        (member-share (/ (* member-contribution u10000) pool-total))
        (dividend-amount (/ (* member-share rate blocks-since-claim) u1000000))
    )
    (ok dividend-amount)
    )
    )
)

(define-public (claim-dividends)
    (let (
        (sender tx-sender)
        (dividend-amount (try! (calculate-dividends sender)))
        (member-data (unwrap! (map-get? members sender) ERR_MEMBER_NOT_FOUND))
    )
    (asserts! (> dividend-amount u0) ERR_NO_DIVIDENDS)
    (asserts! (<= dividend-amount (stx-get-balance (as-contract tx-sender))) ERR_INSUFFICIENT_BALANCE)
    
    (try! (as-contract (stx-transfer? dividend-amount tx-sender sender)))
    
    (map-set members sender (merge member-data {
        last-claim-block: stacks-block-height
    }))
    
    (map-set member-dividends sender 
        (+ (default-to u0 (map-get? member-dividends sender)) dividend-amount))
    
    (ok dividend-amount)
    )
)

(define-public (auto-payout-dividends (members-list (list 50 principal)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (ok (map process-member-payout members-list))
    )
)

(define-private (process-member-payout (member principal))
    (match (calculate-dividends member)
        success (begin
            (if (> success u0)
                (match (as-contract (stx-transfer? success tx-sender member))
                    transfer-success (begin
                        (let ((member-data (unwrap-panic (map-get? members member))))
                            (map-set members member (merge member-data {
                                last-claim-block: stacks-block-height
                            }))
                            (map-set member-dividends member 
                                (+ (default-to u0 (map-get? member-dividends member)) success))
                        )
                        success
                    )
                    transfer-error u0
                )
                u0
            )
        )
        error u0
    )
)

(define-public (set-dividend-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (asserts! (<= new-rate u2000) ERR_INVALID_AMOUNT)
        (var-set dividend-rate new-rate)
        (ok true)
    )
)

(define-public (deactivate-member (member principal))
    (let (
        (member-data (unwrap! (map-get? members member) ERR_MEMBER_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    
    (map-set members member (merge member-data {
        is-active: false
    }))
    
    (var-set total-members (- (var-get total-members) u1))
    
    (ok true)
    )
)

(define-public (emergency-withdraw (amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (asserts! (<= amount (stx-get-balance (as-contract tx-sender))) ERR_INSUFFICIENT_BALANCE)
        (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
        (ok true)
    )
)

(define-read-only (get-member-info (member principal))
    (map-get? members member)
)

(define-read-only (get-member-dividends (member principal))
    (default-to u0 (map-get? member-dividends member))
)

(define-read-only (get-contract-balance)
    (stx-get-balance (as-contract tx-sender))
)

(define-read-only (get-total-pool)
    (var-get total-pool)
)

(define-read-only (get-total-members)
    (var-get total-members)
)

(define-read-only (get-dividend-rate)
    (var-get dividend-rate)
)

(define-read-only (get-contribution-history (member principal) (stacks-block-height-param uint))
    (map-get? contribution-history {member: member, block: stacks-block-height-param})
)

(define-read-only (is-member (address principal))
    (match (map-get? members address)
        member-data (get is-active member-data)
        false
    )
)

(define-read-only (get-member-share (member principal))
    (match (map-get? members member)
        member-data (let (
            (member-contribution (get contribution member-data))
            (pool-total (var-get total-pool))
        )
        (if (> pool-total u0)
            (some (/ (* member-contribution u10000) pool-total))
            none
        ))
        none
    )
)

(define-public (apply-for-loan (requested-amount uint) (term-blocks uint))
    (let (
        (sender tx-sender)
        (member-data (unwrap! (map-get? members sender) ERR_MEMBER_NOT_FOUND))
        (existing-application (map-get? loan-applications sender))
    )
    (asserts! (is-none existing-application) ERR_LOAN_ALREADY_EXISTS)
    (asserts! (> requested-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> term-blocks u0) ERR_INVALID_LOAN_TERMS)
    (asserts! (get is-active member-data) ERR_NOT_AUTHORIZED)
    (asserts! (<= term-blocks u52560) ERR_INVALID_LOAN_TERMS)
    
    (let (
        (member-contribution (get contribution member-data))
        (max-loan-amount (/ (* member-contribution u300) u100))
    )
    (asserts! (<= requested-amount max-loan-amount) ERR_INSUFFICIENT_COLLATERAL)
    
    (map-set loan-applications sender {
        requested-amount: requested-amount,
        term-blocks: term-blocks,
        collateral-contribution: member-contribution,
        application-block: stacks-block-height,
        status: "pending"
    })
    
    (ok true)
    )
    )
)

(define-public (approve-loan (borrower principal))
    (let (
        (application-data (unwrap! (map-get? loan-applications borrower) ERR_LOAN_NOT_FOUND))
        (member-data (unwrap! (map-get? members borrower) ERR_MEMBER_NOT_FOUND))
        (loan-id (var-get next-loan-id))
        (requested-amount (get requested-amount application-data))
        (term-blocks (get term-blocks application-data))
        (current-rate (var-get loan-interest-rate))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status application-data) "pending") ERR_LOAN_ALREADY_APPROVED)
    (asserts! (>= (stx-get-balance (as-contract tx-sender)) requested-amount) ERR_INSUFFICIENT_BALANCE)
    
    (try! (as-contract (stx-transfer? requested-amount tx-sender borrower)))
    
    (map-set loans loan-id {
        borrower: borrower,
        amount: requested-amount,
        interest-rate: current-rate,
        term-blocks: term-blocks,
        issued-block: stacks-block-height,
        repaid-amount: u0,
        is-approved: true,
        is-active: true,
        is-defaulted: false
    })
    
    (map-set loan-applications borrower (merge application-data {
        status: "approved"
    }))
    
    (let (
        (current-loans (default-to (list) (map-get? borrower-loans borrower)))
    )
    (map-set borrower-loans borrower (unwrap! (as-max-len? (append current-loans loan-id) u10) ERR_INVALID_LOAN_TERMS))
    )
    
    (var-set next-loan-id (+ loan-id u1))
    (var-set total-loans-issued (+ (var-get total-loans-issued) u1))
    (var-set total-active-loans (+ (var-get total-active-loans) u1))
    
    (ok loan-id)
    )
)

(define-public (repay-loan (loan-id uint) (payment-amount uint))
    (let (
        (loan-data (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
        (sender tx-sender)
        (borrower (get borrower loan-data))
        (total-owed (calculate-total-owed loan-id))
    )
    (asserts! (is-eq sender borrower) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active loan-data) ERR_LOAN_NOT_APPROVED)
    (asserts! (> payment-amount u0) ERR_INVALID_AMOUNT)
    
    (try! (stx-transfer? payment-amount sender (as-contract tx-sender)))
    
    (let (
        (new-repaid-amount (+ (get repaid-amount loan-data) payment-amount))
        (is-fully-repaid (>= new-repaid-amount total-owed))
    )
    
    (map-set loans loan-id (merge loan-data {
        repaid-amount: new-repaid-amount,
        is-active: (not is-fully-repaid)
    }))
    
    (if is-fully-repaid
        (begin
            (var-set total-active-loans (- (var-get total-active-loans) u1))
            (ok "loan-completed")
        )
        (ok "payment-received")
    )
    )
    )
)

(define-private (calculate-total-owed (loan-id uint))
    (let (
        (loan-data (unwrap-panic (map-get? loans loan-id)))
        (principal-amount (get amount loan-data))
        (interest-rate (get interest-rate loan-data))
        (term-blocks (get term-blocks loan-data))
        (issued-block (get issued-block loan-data))
        (blocks-elapsed (- stacks-block-height issued-block))
    )
    (let (
        (interest-accrued (/ (* (* principal-amount interest-rate) blocks-elapsed) (* u10000 term-blocks)))
    )
    (+ principal-amount interest-accrued)
    )
    )
)

(define-public (mark-loan-default (loan-id uint))
    (let (
        (loan-data (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
        (borrower (get borrower loan-data))
        (issued-block (get issued-block loan-data))
        (term-blocks (get term-blocks loan-data))
        (due-block (+ issued-block term-blocks))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active loan-data) ERR_LOAN_NOT_APPROVED)
    (asserts! (> stacks-block-height due-block) ERR_INVALID_LOAN_TERMS)
    
    (map-set loans loan-id (merge loan-data {
        is-defaulted: true,
        is-active: false
    }))
    
    (let (
        (member-data (unwrap! (map-get? members borrower) ERR_MEMBER_NOT_FOUND))
    )
    (map-set members borrower (merge member-data {
        is-active: false
    }))
    )
    
    (var-set total-active-loans (- (var-get total-active-loans) u1))
    
    (ok true)
    )
)

(define-public (reject-loan (borrower principal))
    (let (
        (application-data (unwrap! (map-get? loan-applications borrower) ERR_LOAN_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status application-data) "pending") ERR_LOAN_ALREADY_APPROVED)
    
    (map-set loan-applications borrower (merge application-data {
        status: "rejected"
    }))
    
    (ok true)
    )
)

(define-public (set-loan-interest-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (asserts! (<= new-rate u2000) ERR_INVALID_AMOUNT)
        (asserts! (>= new-rate u100) ERR_INVALID_AMOUNT)
        (var-set loan-interest-rate new-rate)
        (ok true)
    )
)

(define-read-only (get-loan-application (borrower principal))
    (map-get? loan-applications borrower)
)

(define-read-only (get-loan-info (loan-id uint))
    (map-get? loans loan-id)
)

(define-read-only (get-borrower-loans (borrower principal))
    (default-to (list) (map-get? borrower-loans borrower))
)

(define-read-only (get-total-owed (loan-id uint))
    (match (map-get? loans loan-id)
        loan-data (ok (calculate-total-owed loan-id))
        ERR_LOAN_NOT_FOUND
    )
)

(define-read-only (get-loan-status (loan-id uint))
    (match (map-get? loans loan-id)
        loan-data (let (
            (is-active (get is-active loan-data))
            (is-defaulted (get is-defaulted loan-data))
            (issued-block (get issued-block loan-data))
            (term-blocks (get term-blocks loan-data))
            (due-block (+ issued-block term-blocks))
        )
        (if is-defaulted
            "defaulted"
            (if is-active
                (if (> stacks-block-height due-block)
                    "overdue"
                    "active"
                )
                "completed"
            )
        ))
        "not-found"
    )
)

(define-read-only (get-total-loans-issued)
    (var-get total-loans-issued)
)

(define-read-only (get-total-active-loans)
    (var-get total-active-loans)
)

(define-read-only (get-loan-interest-rate)
    (var-get loan-interest-rate)
)

(define-read-only (check-loan-eligibility (member principal) (requested-amount uint))
    (match (map-get? members member)
        member-data (let (
            (member-contribution (get contribution member-data))
            (max-loan-amount (/ (* member-contribution u300) u100))
            (is-active (get is-active member-data))
            (has-pending-application (is-some (map-get? loan-applications member)))
        )
        {
            eligible: (and is-active 
                          (not has-pending-application)
                          (<= requested-amount max-loan-amount)
                          (> requested-amount u0)),
            max-amount: max-loan-amount,
            current-contribution: member-contribution
        })
        {
            eligible: false,
            max-amount: u0,
            current-contribution: u0
        }
    )
)