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