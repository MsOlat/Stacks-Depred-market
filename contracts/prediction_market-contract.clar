;; prediction_market-contract
;; A robust prediction market contract allowing users to create markets, place bets, and resolve outcomes

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_MARKET_NOT_FOUND (err u101))
(define-constant ERR_MARKET_CLOSED (err u102))
(define-constant ERR_MARKET_RESOLVED (err u103))
(define-constant ERR_INVALID_OUTCOME (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_NO_POSITION (err u106))
(define-constant ERR_MARKET_NOT_RESOLVED (err u107))
(define-constant ERR_ALREADY_CLAIMED (err u108))
(define-constant ERR_INVALID_FEE (err u109))
(define-constant ERR_INVALID_DURATION (err u110))

(define-constant PLATFORM_FEE_PERCENTAGE u250) ;; 2.5%
(define-constant MAX_FEE_PERCENTAGE u1000) ;; 10%
(define-constant MIN_MARKET_DURATION u144) ;; ~1 day in blocks
(define-constant MAX_MARKET_DURATION u144000) ;; ~1000 days in blocks

;; data maps and vars
(define-data-var next-market-id uint u1)
(define-data-var platform-fee-recipient principal CONTRACT_OWNER)
(define-data-var paused bool false)

;; Market structure
(define-map markets uint {
    creator: principal,
    question: (string-ascii 256),
    description: (string-ascii 1024),
    outcome-a: (string-ascii 128),
    outcome-b: (string-ascii 128),
    end-block: uint,
    resolution-block: uint,
    resolved: bool,
    winning-outcome: (optional uint),
    total-pool-a: uint,
    total-pool-b: uint,
    creator-fee: uint,
    created-at: uint
})

;; User positions in markets
(define-map user-positions {market-id: uint, user: principal, outcome: uint} {
    amount: uint,
    claimed: bool
})

;; Market statistics
(define-map market-stats uint {
    total-volume: uint,
    total-participants: uint,
    fees-collected: uint
})

;; User statistics
(define-map user-stats principal {
    markets-created: uint,
    total-bet: uint,
    total-won: uint,
    markets-participated: uint
})

;; private functions

(define-private (is-contract-owner)
    (is-eq tx-sender CONTRACT_OWNER))

(define-private (is-market-creator (market-id uint))
    (match (map-get? markets market-id)
        market (is-eq tx-sender (get creator market))
        false))

(define-private (is-valid-outcome (outcome uint))
    (or (is-eq outcome u1) (is-eq outcome u2)))

(define-private (calculate-payout (bet-amount uint) (winning-pool uint) (losing-pool uint))
    (if (is-eq winning-pool u0)
        bet-amount
        (/ (* bet-amount (+ winning-pool losing-pool)) winning-pool)))

(define-private (calculate-fee (amount uint) (fee-percentage uint))
    (/ (* amount fee-percentage) u10000))

(define-private (update-user-stats (user principal) (amount uint) (is-new-market bool) (is-new-participation bool))
    (let ((current-stats (default-to {markets-created: u0, total-bet: u0, total-won: u0, markets-participated: u0}
                                   (map-get? user-stats user))))
        (map-set user-stats user {
            markets-created: (if is-new-market (+ (get markets-created current-stats) u1) (get markets-created current-stats)),
            total-bet: (+ (get total-bet current-stats) amount),
            total-won: (get total-won current-stats),
            markets-participated: (if is-new-participation (+ (get markets-participated current-stats) u1) (get markets-participated current-stats))
        })))

(define-private (update-winner-stats (user principal) (winnings uint))
    (let ((current-stats (default-to {markets-created: u0, total-bet: u0, total-won: u0, markets-participated: u0}
                                   (map-get? user-stats user))))
        (map-set user-stats user (merge current-stats {total-won: (+ (get total-won current-stats) winnings)}))))

;; public functions

;; Create a new prediction market
(define-public (create-market 
    (question (string-ascii 256))
    (description (string-ascii 1024))
    (outcome-a (string-ascii 128))
    (outcome-b (string-ascii 128))
    (duration uint)
    (creator-fee uint))
    (let ((market-id (var-get next-market-id))
          (end-block (+ block-height duration)))
        (asserts! (not (var-get paused)) ERR_NOT_AUTHORIZED)
        (asserts! (and (>= duration MIN_MARKET_DURATION) (<= duration MAX_MARKET_DURATION)) ERR_INVALID_DURATION)
        (asserts! (<= creator-fee MAX_FEE_PERCENTAGE) ERR_INVALID_FEE)
        
        (map-set markets market-id {
            creator: tx-sender,
            question: question,
            description: description,
            outcome-a: outcome-a,
            outcome-b: outcome-b,
            end-block: end-block,
            resolution-block: u0,
            resolved: false,
            winning-outcome: none,
            total-pool-a: u0,
            total-pool-b: u0,
            creator-fee: creator-fee,
            created-at: block-height
        })
        
        (map-set market-stats market-id {
            total-volume: u0,
            total-participants: u0,
            fees-collected: u0
        })
        
        (update-user-stats tx-sender u0 true false)
        (var-set next-market-id (+ market-id u1))
        (ok market-id)))

;; Place a bet on a market outcome
(define-public (place-bet (market-id uint) (outcome uint) (amount uint))
    (let ((market-opt (map-get? markets market-id))
          (existing-position (map-get? user-positions {market-id: market-id, user: tx-sender, outcome: outcome})))
        (asserts! (not (var-get paused)) ERR_NOT_AUTHORIZED)
        (asserts! (is-some market-opt) ERR_MARKET_NOT_FOUND)
        (asserts! (is-valid-outcome outcome) ERR_INVALID_OUTCOME)
        (asserts! (> amount u0) ERR_INSUFFICIENT_FUNDS)
        
        (let ((market (unwrap-panic market-opt)))
            (asserts! (< block-height (get end-block market)) ERR_MARKET_CLOSED)
            (asserts! (not (get resolved market)) ERR_MARKET_RESOLVED)
            
            ;; Transfer STX from user
            (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
            
            ;; Update market pools
            (let ((new-market (if (is-eq outcome u1)
                                (merge market {total-pool-a: (+ (get total-pool-a market) amount)})
                                (merge market {total-pool-b: (+ (get total-pool-b market) amount)}))))
                (map-set markets market-id new-market)
                
                ;; Update user position
                (let ((new-amount (+ amount (default-to u0 (get amount existing-position)))))
                    (map-set user-positions {market-id: market-id, user: tx-sender, outcome: outcome} {
                        amount: new-amount,
                        claimed: false
                    }))
                
                ;; Update statistics
                (let ((stats (default-to {total-volume: u0, total-participants: u0, fees-collected: u0}
                                       (map-get? market-stats market-id))))
                    (map-set market-stats market-id {
                        total-volume: (+ (get total-volume stats) amount),
                        total-participants: (if (is-none existing-position) (+ (get total-participants stats) u1) (get total-participants stats)),
                        fees-collected: (get fees-collected stats)
                    }))
                
                (update-user-stats tx-sender amount false (is-none existing-position))
                (ok true)))))

;; Resolve a market (only creator or contract owner)
(define-public (resolve-market (market-id uint) (winning-outcome uint))
    (let ((market-opt (map-get? markets market-id)))
        (asserts! (is-some market-opt) ERR_MARKET_NOT_FOUND)
        (asserts! (is-valid-outcome winning-outcome) ERR_INVALID_OUTCOME)
        
        (let ((market (unwrap-panic market-opt)))
            (asserts! (or (is-market-creator market-id) (is-contract-owner)) ERR_NOT_AUTHORIZED)
            (asserts! (>= block-height (get end-block market)) ERR_MARKET_CLOSED)
            (asserts! (not (get resolved market)) ERR_MARKET_RESOLVED)
            
            (map-set markets market-id (merge market {
                resolved: true,
                winning-outcome: (some winning-outcome),
                resolution-block: block-height
            }))
            
            (ok true))))

;; Claim winnings from a resolved market
(define-public (claim-winnings (market-id uint) (outcome uint))
    (let ((market-opt (map-get? markets market-id))
          (position-opt (map-get? user-positions {market-id: market-id, user: tx-sender, outcome: outcome})))
        (asserts! (is-some market-opt) ERR_MARKET_NOT_FOUND)
        (asserts! (is-some position-opt) ERR_NO_POSITION)
        
        (let ((market (unwrap-panic market-opt))
              (position (unwrap-panic position-opt)))
            (asserts! (get resolved market) ERR_MARKET_NOT_RESOLVED)
            (asserts! (not (get claimed position)) ERR_ALREADY_CLAIMED)
            (asserts! (is-eq (some outcome) (get winning-outcome market)) ERR_INVALID_OUTCOME)
            
            (let ((bet-amount (get amount position))
                  (winning-pool (if (is-eq outcome u1) (get total-pool-a market) (get total-pool-b market)))
                  (losing-pool (if (is-eq outcome u1) (get total-pool-b market) (get total-pool-a market)))
                  (gross-payout (calculate-payout bet-amount winning-pool losing-pool))
                  (platform-fee (calculate-fee gross-payout PLATFORM_FEE_PERCENTAGE))
                  (creator-fee (calculate-fee gross-payout (get creator-fee market)))
                  (net-payout (- gross-payout (+ platform-fee creator-fee))))
                
                ;; Mark as claimed
                (map-set user-positions {market-id: market-id, user: tx-sender, outcome: outcome}
                    (merge position {claimed: true}))
                
                ;; Transfer winnings
                (try! (as-contract (stx-transfer? net-payout tx-sender tx-sender)))
                
                ;; Transfer fees
                (try! (as-contract (stx-transfer? platform-fee tx-sender (var-get platform-fee-recipient))))
                (try! (as-contract (stx-transfer? creator-fee tx-sender (get creator market))))
                
                ;; Update statistics
                (let ((stats (default-to {total-volume: u0, total-participants: u0, fees-collected: u0}
                                       (map-get? market-stats market-id))))
                    (map-set market-stats market-id (merge stats {
                        fees-collected: (+ (get fees-collected stats) platform-fee creator-fee)
                    })))
                
                (update-winner-stats tx-sender net-payout)
                (ok net-payout)))))

;; Get market information
(define-read-only (get-market (market-id uint))
    (map-get? markets market-id))

;; Get user position
(define-read-only (get-user-position (market-id uint) (user principal) (outcome uint))
    (map-get? user-positions {market-id: market-id, user: user, outcome: outcome}))

;; Get market statistics
(define-read-only (get-market-stats (market-id uint))
    (map-get? market-stats market-id))

;; Get user statistics
(define-read-only (get-user-stats (user principal))
    (map-get? user-stats user))

;; Get market odds
(define-read-only (get-market-odds (market-id uint))
    (match (map-get? markets market-id)
        market (let ((pool-a (get total-pool-a market))
                     (pool-b (get total-pool-b market))
                     (total-pool (+ pool-a pool-b)))
                 (if (is-eq total-pool u0)
                     (ok {odds-a: u5000, odds-b: u5000}) ;; 50-50 if no bets
                     (ok {odds-a: (/ (* pool-b u10000) total-pool),
                          odds-b: (/ (* pool-a u10000) total-pool)})))
        ERR_MARKET_NOT_FOUND))

;; Calculate potential payout
(define-read-only (calculate-potential-payout (market-id uint) (outcome uint) (bet-amount uint))
    (match (map-get? markets market-id)
        market (let ((winning-pool (if (is-eq outcome u1) (get total-pool-a market) (get total-pool-b market)))
                     (losing-pool (if (is-eq outcome u1) (get total-pool-b market) (get total-pool-a market)))
                     (new-winning-pool (+ winning-pool bet-amount))
                     (gross-payout (calculate-payout bet-amount new-winning-pool losing-pool))
                     (platform-fee (calculate-fee gross-payout PLATFORM_FEE_PERCENTAGE))
                     (creator-fee (calculate-fee gross-payout (get creator-fee market))))
                 (ok (- gross-payout (+ platform-fee creator-fee))))
        ERR_MARKET_NOT_FOUND))

;; Admin functions
(define-public (set-platform-fee-recipient (new-recipient principal))
    (begin
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        (var-set platform-fee-recipient new-recipient)
        (ok true)))

(define-public (pause-contract)
    (begin
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        (var-set paused true)
        (ok true)))

(define-public (unpause-contract)
    (begin
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        (var-set paused false)
        (ok true)))

;; Emergency function to resolve stuck markets (only contract owner)
(define-public (emergency-resolve (market-id uint) (winning-outcome uint))
    (let ((market-opt (map-get? markets market-id)))
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        (asserts! (is-some market-opt) ERR_MARKET_NOT_FOUND)
        (asserts! (is-valid-outcome winning-outcome) ERR_INVALID_OUTCOME)
        
        (let ((market (unwrap-panic market-opt)))
            (asserts! (not (get resolved market)) ERR_MARKET_RESOLVED)
            
            (map-set markets market-id (merge market {
                resolved: true,
                winning-outcome: (some winning-outcome),
                resolution-block: block-height
            }))
            
            (ok true))))