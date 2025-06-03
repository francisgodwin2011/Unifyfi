(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))
(define-constant ERR_MEMBER_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_MEMBER (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_NO_DIVIDENDS (err u105))
(define-constant ERR_TRANSFER_FAILED (err u106))

(define-data-var total-pool uint u0)
(define-data-var dividend-rate uint u500)
(define-data-var last-dividend-block uint u0)
(define-data-var total-members uint u0)

(define-map members principal {
    contribution: uint,
    last-claim-block: uint,
    join-block: uint,
    is-active: bool
})

(define-map member-dividends principal uint)

(define-map contribution-history {member: principal, block: uint} uint)

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