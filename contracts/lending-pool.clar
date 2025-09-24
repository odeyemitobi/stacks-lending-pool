;; title: lending-pool
;; version: 1.0.0
;; summary: A decentralized lending pool for sBTC and STX
;; description: Allows users to lend STX, borrow STX against sBTC collateral, and earn yield

;; Errors
(define-constant ERR_INVALID_WITHDRAW_AMOUNT (err u100))
(define-constant ERR_EXCEEDED_MAX_BORROW (err u101))
(define-constant ERR_CANNOT_BE_LIQUIDATED (err u102))

;; Constants
(define-constant LTV_PERCENTAGE u70)
(define-constant INTEREST_RATE_PERCENTAGE u10)
(define-constant LIQUIDATION_THRESHOLD_PERCENTAGE u90)
(define-constant ONE_YEAR_IN_SECS u31556952)

;; Storage
(define-data-var total-sbtc-collateral uint u0)
(define-data-var total-stx-deposits uint u1)
(define-data-var total-stx-borrows uint u0)

(define-data-var last-interest-accrual uint (get-latest-timestamp))
(define-data-var cumulative-yield-bips uint u0)

(define-map collateral
  { user: principal }
  { amount: uint }
)

(define-map deposits
  { user: principal }
  {
    amount: uint,
    yield-index: uint,
  }
)

(define-map borrows
  { user: principal }
  {
    amount: uint,
    last-accrued: uint,
  }
)

;; Helper functions

;; @desc Gets the sBTC price in STX from the oracle
;; @returns (response uint)
(define-private (get-sbtc-stx-price)
    (contract-call? .mock-oracle get-price)
)

;; @desc Gets the latest timestamp
;; @returns (response uint)
(define-private (get-latest-timestamp)
    (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))
)

;; @desc Accrues interest for all lenders based on total borrowed STX
;; @returns (response bool)
(define-private (accrue-interest)
    (let (
            (dt (- (get-latest-timestamp) (var-get last-interest-accrual)))
            (interest-numerator (* u10000
                (* (* (var-get total-stx-borrows) INTEREST_RATE_PERCENTAGE) dt)
            ))
            (interest-denominator (* ONE_YEAR_IN_SECS u100))
            (interest (/ interest-numerator interest-denominator))
            (new-yield (/ interest (var-get total-stx-deposits)))
        )
        (var-set last-interest-accrual (get-latest-timestamp))
        (var-set cumulative-yield-bips
            (+ (var-get cumulative-yield-bips) new-yield)
        )
        (ok true)
    )
)

;; @desc Gets the current debt of a user including accrued interest
;; @param user: The user's principal
;; @returns (response uint)
(define-read-only (get-debt (user principal))
    (let (
            (user-borrow (map-get? borrows { user: user }))
            (borrowed-stx (default-to u0 (get amount user-borrow)))
            (last-accrued (default-to u0 (get last-accrued user-borrow)))
            (dt (- (get-latest-timestamp) last-accrued))
            (interest-numerator (* (* borrowed-stx INTEREST_RATE_PERCENTAGE) dt))
            (interest-denominator (* ONE_YEAR_IN_SECS u100))
            (interest (/ interest-numerator interest-denominator))
        )
        (ok (+ borrowed-stx interest))
    )
)

;; @desc Gets the pending yield for a user
;; @returns (response uint)
(define-read-only (get-pending-yield)
    (let (
            (user-deposit (map-get? deposits { user: tx-sender }))
            (deposited-stx (default-to u0 (get amount user-deposit)))
            (yield-index (default-to u0 (get yield-index user-deposit)))
            (current-yield-index (var-get cumulative-yield-bips))
            (yield-diff (- current-yield-index yield-index))
        )
        (ok (/ (* deposited-stx yield-diff) u10000))
    )
)

;; Public functions - Lender Logic

;; @desc Allows users to deposit STX into the lending pool
;; @param amount: Amount of STX to deposit
;; @returns (response bool)
(define-public (deposit-stx (amount uint))
  (let (
      (user-deposit (map-get? deposits { user: tx-sender }))
      (deposited-stx (default-to u0 (get amount user-deposit)))
    )
    (unwrap-panic (accrue-interest))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set deposits { user: tx-sender } {
      amount: (+ deposited-stx amount),
      yield-index: (var-get cumulative-yield-bips),
    })
    (var-set total-stx-deposits (+ (var-get total-stx-deposits) amount))
    (ok true)
  )
)

;; @desc Allows users to withdraw their STX deposits plus earned yield
;; @param amount: Amount of STX to withdraw
;; @returns (response bool)
(define-public (withdraw-stx (amount uint))
  (let (
      (user tx-sender)
      (user-deposit (map-get? deposits { user: user }))
      (deposited-stx (default-to u0 (get amount user-deposit)))
      (yield-index (default-to u0 (get yield-index user-deposit)))
      (pending-yield (unwrap-panic (get-pending-yield)))
    )
    (asserts! (>= deposited-stx amount) ERR_INVALID_WITHDRAW_AMOUNT)
    (unwrap-panic (accrue-interest))

    (map-set deposits { user: user } {
      amount: (- deposited-stx amount),
      yield-index: (var-get cumulative-yield-bips),
    })
    (var-set total-stx-deposits (- (var-get total-stx-deposits) amount))
    (try! (as-contract (stx-transfer? (+ amount pending-yield) tx-sender user)))
    (ok true)
  )
)

;; Public functions - Borrower Logic

;; @desc Allows users to borrow STX against sBTC collateral
;; @param collateral-amount: Amount of sBTC to deposit as collateral
;; @param amount-stx: Amount of STX to borrow
;; @returns (response bool)
(define-public (borrow-stx
    (collateral-amount uint)
    (amount-stx uint)
  )
  (let (
      (user tx-sender)
      (user-collateral (map-get? collateral { user: user }))
      (deposited-sbtc (default-to u0 (get amount user-collateral)))
      (new-collateral (+ deposited-sbtc collateral-amount))
      (price (unwrap-panic (get-sbtc-stx-price)))
      (max-borrow (/ (* (* new-collateral price) LTV_PERCENTAGE) u100))
      (user-borrow (map-get? borrows { user: user }))
      (borrowed-stx (default-to u0 (get amount user-borrow)))
      (user-debt (unwrap-panic (get-debt user)))
      (new-debt (+ user-debt amount-stx))
    )
    (asserts! (<= new-debt max-borrow) ERR_EXCEEDED_MAX_BORROW)

    (unwrap-panic (accrue-interest))
    (map-set borrows { user: user } {
      amount: new-debt,
      last-accrued: (get-latest-timestamp),
    })
    (var-set total-stx-borrows (+ (var-get total-stx-borrows) amount-stx))
    (map-set collateral { user: user } { amount: new-collateral })
    (var-set total-sbtc-collateral
      (+ (var-get total-sbtc-collateral) collateral-amount)
    )
    (try! (contract-call? 'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token
      transfer collateral-amount tx-sender (as-contract tx-sender) none
    ))
    (try! (as-contract (stx-transfer? amount-stx tx-sender user)))
    (ok true)
  )
)

;; @desc Allows users to repay their STX debt and retrieve collateral
;; @returns (response bool)
(define-public (repay)
  (let (
      (user-borrow (map-get? borrows { user: tx-sender }))
      (borrowed-stx (default-to u0 (get amount user-borrow)))
      (total-debt (+ u1 (unwrap-panic (get-debt tx-sender))))
      (user-collateral (map-get? collateral { user: tx-sender }))
      (deposited-sbtc (default-to u0 (get amount user-collateral)))
    )
    (unwrap-panic (accrue-interest))

    (map-delete collateral { user: tx-sender })
    (var-set total-sbtc-collateral
      (- (var-get total-sbtc-collateral) deposited-sbtc)
    )
    (map-delete borrows { user: tx-sender })
    (var-set total-stx-borrows (- (var-get total-stx-borrows) borrowed-stx))

    (try! (stx-transfer? total-debt tx-sender (as-contract tx-sender)))
    (try! (contract-call? 'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token
      transfer deposited-sbtc (as-contract tx-sender) tx-sender none
    ))
    (ok true)
  )
)

;; @desc Liquidates an undercollateralized position
;; @param user: The user to liquidate
;; @returns (response bool)
(define-public (liquidate (user principal))
  (let (
      (user-debt (unwrap-panic (get-debt user)))
      (forfeited-borrows (if (> user-debt (var-get total-stx-borrows))
        (var-get total-stx-borrows)
        user-debt
      ))
      (user-collateral (map-get? collateral { user: user }))
      (deposited-sbtc (default-to u0 (get amount user-collateral)))
      (price (unwrap-panic (get-sbtc-stx-price)))
      (collateral-value-in-stx (* deposited-sbtc price))
      (liquidator-bounty (/ (* deposited-sbtc u10) u100))
      (pool-reward (- deposited-sbtc liquidator-bounty))
    )
    (unwrap-panic (accrue-interest))
    (asserts! (> user-debt u0) ERR_CANNOT_BE_LIQUIDATED)
    (asserts!
      (< (* collateral-value-in-stx u100)
        (* user-debt LIQUIDATION_THRESHOLD_PERCENTAGE)
      )
      ERR_CANNOT_BE_LIQUIDATED
    )

    (var-set total-sbtc-collateral
      (- (var-get total-sbtc-collateral) deposited-sbtc)
    )
    (var-set total-stx-borrows (- (var-get total-stx-borrows) forfeited-borrows))
    (map-delete borrows { user: user })
    (map-delete collateral { user: user })

    ;; Transfer liquidator bounty
    (try! (contract-call? 'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token
      transfer liquidator-bounty (as-contract tx-sender) tx-sender none
    ))

    ;; Simulate DEX swap: convert pool reward sBTC to STX at current price
    ;; In production, this would use actual DEX integration as shown in tutorial
    (let ((received-stx (* pool-reward price)))
      ;; Add yield based on the STX received from liquidation
      ;; This represents the value recovered for lenders
      (if (> received-stx u0)
        (var-set cumulative-yield-bips
          (+ (var-get cumulative-yield-bips)
            (/ (* received-stx u10000) (var-get total-stx-deposits))
          )
        )
        true
      )
    )

    (ok true)
  )
)

