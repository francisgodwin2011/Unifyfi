;; Member Referral and Reward System
;; Incentivizes credit union growth through member referrals with tiered rewards

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-INVALID-AMOUNT (err u301))
(define-constant ERR-REFERRER-NOT-FOUND (err u302))
(define-constant ERR-ALREADY-REFERRED (err u303))
(define-constant ERR-CANNOT-SELF-REFER (err u304))
(define-constant ERR-REFERRAL-NOT-FOUND (err u305))
(define-constant ERR-REWARD-ALREADY-CLAIMED (err u306))
(define-constant ERR-REFEREE-NOT-ELIGIBLE (err u307))

;; Track referral relationships
(define-map member-referrals
    principal  ;; referee (new member)
    {
        referrer: principal,
        referral-block: uint,
        referrer-reward-claimed: bool,
        referee-bonus-claimed: bool
    }
)

;; Track referrer statistics and rewards
(define-map referrer-stats
    principal  ;; referrer
    {
        total-referrals: uint,
        active-referrals: uint,
        total-rewards-earned: uint,
        referral-tier: uint,
        last-reward-block: uint
    }
)

;; Track referee contribution milestones for bonus rewards
(define-map referee-milestones
    {referee: principal, milestone: uint}
    {
        target-contribution: uint,
        bonus-reward: uint,
        achieved: bool,
        achieved-block: uint
    }
)

;; Referral reward configuration
(define-data-var base-referral-reward uint u50000) ;; 0.05 STX base reward
(define-data-var referee-welcome-bonus uint u25000) ;; 0.025 STX welcome bonus
(define-data-var contribution-reward-rate uint u5) ;; 0.5% of referee contributions
(define-data-var tier-multipliers (list 5 uint) (list u100 u125 u150 u200 u300)) ;; 1x, 1.25x, 1.5x, 2x, 3x

;; Data tracking
(define-data-var total-referral-rewards uint u0)
(define-data-var total-active-referrals uint u0)

;; Register a referral when new member joins
(define-public (register-referral (referee principal) (referrer principal))
    (let (
        (referrer-is-member (contract-call? .Unifyfi is-member referrer))
        (referee-is-member (contract-call? .Unifyfi is-member referee))
        (existing-referral (map-get? member-referrals referee))
    )
    ;; Validate referral eligibility
    (asserts! referrer-is-member ERR-REFERRER-NOT-FOUND)
    (asserts! referee-is-member ERR-REFEREE-NOT-ELIGIBLE)
    (asserts! (not (is-eq referrer referee)) ERR-CANNOT-SELF-REFER)
    (asserts! (is-none existing-referral) ERR-ALREADY-REFERRED)
    
    ;; Record referral relationship
    (map-set member-referrals referee
        {
            referrer: referrer,
            referral-block: stacks-block-height,
            referrer-reward-claimed: false,
            referee-bonus-claimed: false
        }
    )
    
    ;; Update referrer statistics
    (let (
        (current-stats (default-to 
            {total-referrals: u0, active-referrals: u0, total-rewards-earned: u0, 
             referral-tier: u0, last-reward-block: u0}
            (map-get? referrer-stats referrer)))
        (new-total (+ (get total-referrals current-stats) u1))
        (new-active (+ (get active-referrals current-stats) u1))
        (new-tier (calculate-referral-tier new-total))
    )
    (map-set referrer-stats referrer
        {
            total-referrals: new-total,
            active-referrals: new-active,
            total-rewards-earned: (get total-rewards-earned current-stats),
            referral-tier: new-tier,
            last-reward-block: stacks-block-height
        }
    )
    )
    
    ;; Set referee milestones for future bonuses
    (unwrap! (set-referee-milestones referee) ERR-INVALID-AMOUNT)
    
    (var-set total-active-referrals (+ (var-get total-active-referrals) u1))
    (ok true)
    )
)

;; Calculate referral tier based on total referrals
(define-private (calculate-referral-tier (total-referrals uint))
    (if (>= total-referrals u20) u4  ;; Platinum (20+ referrals)
        (if (>= total-referrals u10) u3  ;; Gold (10+ referrals)
            (if (>= total-referrals u5) u2  ;; Silver (5+ referrals)
                (if (>= total-referrals u2) u1  ;; Bronze (2+ referrals)
                    u0  ;; Starter (0-1 referrals)
                )
            )
        )
    )
)

;; Set referee milestone targets
(define-private (set-referee-milestones (referee principal))
    (begin
        ;; Milestone 1: 1 STX contribution
        (map-set referee-milestones {referee: referee, milestone: u1}
            {target-contribution: u1000000, bonus-reward: u10000, achieved: false, achieved-block: u0})
        
        ;; Milestone 2: 5 STX contribution  
        (map-set referee-milestones {referee: referee, milestone: u2}
            {target-contribution: u5000000, bonus-reward: u25000, achieved: false, achieved-block: u0})
        
        ;; Milestone 3: 10 STX contribution
        (map-set referee-milestones {referee: referee, milestone: u3}
            {target-contribution: u10000000, bonus-reward: u50000, achieved: false, achieved-block: u0})
        
        (ok true)
    )
)

;; Claim referral reward for bringing in new member
(define-public (claim-referral-reward (referee principal))
    (let (
        (referral-data (unwrap! (map-get? member-referrals referee) ERR-REFERRAL-NOT-FOUND))
        (referrer (get referrer referral-data))
        (sender tx-sender)
        (referrer-stats-data (unwrap! (map-get? referrer-stats referrer) ERR-REFERRER-NOT-FOUND))
    )
    ;; Only referrer can claim reward
    (asserts! (is-eq sender referrer) ERR-NOT-AUTHORIZED)
    (asserts! (not (get referrer-reward-claimed referral-data)) ERR-REWARD-ALREADY-CLAIMED)
    
    ;; Calculate reward with tier multiplier
    (let (
        (base-reward (var-get base-referral-reward))
        (tier (get referral-tier referrer-stats-data))
        (tier-multiplier (unwrap-panic (element-at (var-get tier-multipliers) tier)))
        (final-reward (/ (* base-reward tier-multiplier) u100))
    )
    
    ;; Transfer reward to referrer
    (try! (as-contract (stx-transfer? final-reward tx-sender sender)))
    
    ;; Mark reward as claimed
    (map-set member-referrals referee
        (merge referral-data {referrer-reward-claimed: true})
    )
    
    ;; Update referrer stats
    (map-set referrer-stats referrer
        (merge referrer-stats-data 
            {
                total-rewards-earned: (+ (get total-rewards-earned referrer-stats-data) final-reward),
                last-reward-block: stacks-block-height
            }
        )
    )
    
    (var-set total-referral-rewards (+ (var-get total-referral-rewards) final-reward))
    (ok final-reward)
    )
    )
)

;; Claim welcome bonus for new member
(define-public (claim-welcome-bonus)
    (let (
        (sender tx-sender)
        (referral-data (unwrap! (map-get? member-referrals sender) ERR-REFERRAL-NOT-FOUND))
        (welcome-bonus (var-get referee-welcome-bonus))
    )
    ;; Only referee can claim bonus
    (asserts! (not (get referee-bonus-claimed referral-data)) ERR-REWARD-ALREADY-CLAIMED)
    
    ;; Transfer welcome bonus to new member
    (try! (as-contract (stx-transfer? welcome-bonus tx-sender sender)))
    
    ;; Mark bonus as claimed
    (map-set member-referrals sender
        (merge referral-data {referee-bonus-claimed: true})
    )
    
    (ok welcome-bonus)
    )
)

;; Check and process referee milestone achievements
(define-public (check-referee-milestones (referee principal))
    (let (
        (member-data (unwrap! (contract-call? .Unifyfi get-member-info referee) ERR-REFEREE-NOT-ELIGIBLE))
        (total-contribution (get contribution member-data))
        (referral-data (unwrap! (map-get? member-referrals referee) ERR-REFERRAL-NOT-FOUND))
        (referrer (get referrer referral-data))
    )
    
    ;; Check each milestone
    (unwrap! (process-milestone-achievement referee u1 total-contribution referrer) ERR-INVALID-AMOUNT)
    (unwrap! (process-milestone-achievement referee u2 total-contribution referrer) ERR-INVALID-AMOUNT)
    (unwrap! (process-milestone-achievement referee u3 total-contribution referrer) ERR-INVALID-AMOUNT)
    
    (ok true)
    )
)

;; Process individual milestone achievement
(define-private (process-milestone-achievement (referee principal) (milestone-num uint) (total-contribution uint) (referrer principal))
    (let (
        (milestone-data (map-get? referee-milestones {referee: referee, milestone: milestone-num}))
    )
    (match milestone-data
        milestone-info
            (if (and (>= total-contribution (get target-contribution milestone-info))
                     (not (get achieved milestone-info)))
                (begin
                    ;; Mark milestone as achieved
                    (map-set referee-milestones {referee: referee, milestone: milestone-num}
                        (merge milestone-info 
                            {
                                achieved: true,
                                achieved-block: stacks-block-height
                            }
                        )
                    )
                    
                    ;; Award bonus to referrer
                    (let ((bonus-reward (get bonus-reward milestone-info)))
                        (unwrap! (as-contract (stx-transfer? bonus-reward tx-sender referrer)) ERR-INVALID-AMOUNT)
                        
                        ;; Update referrer stats
                        (let ((referrer-stats-data (unwrap-panic (map-get? referrer-stats referrer))))
                            (map-set referrer-stats referrer
                                (merge referrer-stats-data 
                                    {
                                        total-rewards-earned: (+ (get total-rewards-earned referrer-stats-data) bonus-reward)
                                    }
                                )
                            )
                        )
                        (ok true)
                    )
                )
                (ok true)
            )
        (ok true)
    )
    )
)

;; Admin function to fund referral reward pool
(define-public (fund-referral-pool (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (ok true)
    )
)

;; Admin functions to configure reward system
(define-public (update-base-referral-reward (new-reward uint))
    (begin
        (var-set base-referral-reward new-reward)
        (ok true)
    )
)

(define-public (update-welcome-bonus (new-bonus uint))
    (begin
        (var-set referee-welcome-bonus new-bonus)
        (ok true)
    )
)

(define-public (update-contribution-reward-rate (new-rate uint))
    (begin
        (asserts! (<= new-rate u50) ERR-INVALID-AMOUNT) ;; Max 5% rate
        (var-set contribution-reward-rate new-rate)
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-referral-info (referee principal))
    (map-get? member-referrals referee)
)

(define-read-only (get-referrer-stats (referrer principal))
    (map-get? referrer-stats referrer)
)

(define-read-only (get-referee-milestone (referee principal) (milestone-num uint))
    (map-get? referee-milestones {referee: referee, milestone: milestone-num})
)

(define-read-only (calculate-referral-reward (referrer principal))
    (let (
        (stats (map-get? referrer-stats referrer))
    )
    (match stats
        stats-data (let (
            (tier (get referral-tier stats-data))
            (base-reward (var-get base-referral-reward))
            (tier-multiplier (unwrap-panic (element-at (var-get tier-multipliers) tier)))
        )
        (/ (* base-reward tier-multiplier) u100))
        u0
    )
    )
)

(define-read-only (get-referral-system-stats)
    {
        total-active-referrals: (var-get total-active-referrals),
        total-rewards-distributed: (var-get total-referral-rewards),
        base-reward: (var-get base-referral-reward),
        welcome-bonus: (var-get referee-welcome-bonus),
        contribution-rate: (var-get contribution-reward-rate)
    }
)

(define-read-only (check-referral-eligibility (potential-referee principal) (potential-referrer principal))
    (let (
        (referrer-is-member (contract-call? .Unifyfi is-member potential-referrer))
        (referee-is-member (contract-call? .Unifyfi is-member potential-referee))
        (existing-referral (map-get? member-referrals potential-referee))
        (is-self-referral (is-eq potential-referee potential-referrer))
    )
    {
        eligible: (and referrer-is-member 
                      referee-is-member 
                      (not is-self-referral) 
                      (is-none existing-referral)),
        referrer-is-member: referrer-is-member,
        referee-is-member: referee-is-member,
        already-referred: (is-some existing-referral),
        is-self-referral: is-self-referral
    }
    )
)
