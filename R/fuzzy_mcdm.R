#' @title Process Weights for Fuzzy MCDM
#' @description Internal helper to handle either explicit fuzzy weights or BWM calculation.
#' @keywords internal
.get_final_weights <- function(decision_mat, weights, bwm_criteria, bwm_best, bwm_worst) {

  n_crit <- ncol(decision_mat) / 3

  # Option 1: Explicit Weights provided
  if (!missing(weights)) {
    if (length(weights) != ncol(decision_mat)) {
      stop("Length of 'weights' must match the total columns in decision matrix (n * 3).")
    }
    return(weights)
  }

  # Option 2: BWM Calculation
  if (!missing(bwm_best) && !missing(bwm_worst)) {

    # Try to infer criteria names if not provided
    if (missing(bwm_criteria)) {
      if (!is.null(attr(decision_mat, "criteria_names"))) {
        bwm_criteria <- attr(decision_mat, "criteria_names")
      } else {
        bwm_criteria <- paste0("C", 1:n_crit)
        message("No criteria names found. Using default names: ", paste(bwm_criteria, collapse=", "))
      }
    }

    message("Calculating weights using BWM...")
    bwm_res <- calculate_bwm_weights(bwm_criteria, bwm_best, bwm_worst)

    crisp_w <- bwm_res$criteriaWeights

    if (length(crisp_w) != n_crit) {
      stop("Calculated BWM weights do not match the number of criteria in decision matrix.")
    }

    # Convert crisp BWM weights to Triangular Fuzzy Number (w, w, w)
    fuzzy_weights <- rep(crisp_w, each = 3)
    return(fuzzy_weights)
  }

  stop("You must provide either 'weights' vector OR 'bwm_best' and 'bwm_worst' parameters.")
}

#' Fuzzy TOPSIS with Vector Normalization
#'
#' @description Implements Fuzzy TOPSIS. Accepts either manual fuzzy weights or BWM parameters.
#' @param decision_mat Matrix (m x 3n). Alternatives (rows) x Fuzzy Criteria (cols).
#' @param criteria_types Character vector length n. "max" for benefit, "min" for cost.
#' @param weights (Optional) Numeric vector length 3n for fuzzy weights.
#' @param bwm_criteria (Optional) If weights are missing, BWM criteria names.
#' @param bwm_best (Optional) BWM best-to-others vector.
#' @param bwm_worst (Optional) BWM others-to-worst vector.
#' @return An object of class `fuzzy_topsis_res` containing ranking and distance details.
#' @export
fuzzy_topsis <- function(decision_mat, criteria_types, weights, bwm_criteria, bwm_best, bwm_worst) {

  if (!is.matrix(decision_mat)) stop("'decision_mat' must be a matrix.")

  # 1. Weight Handling
  final_w <- .get_final_weights(decision_mat, weights, bwm_criteria, bwm_best, bwm_worst)

  # 2. Expand Criteria Types
  n_cols <- ncol(decision_mat)
  fuzzy_cb <- character(n_cols)
  k <- 1
  for (j in seq(1, n_cols, 3)) {
    fuzzy_cb[j:(j+2)] <- criteria_types[k]
    k <- k + 1
  }

  # 3. Normalization
  norm_mat <- matrix(nrow = nrow(decision_mat), ncol = n_cols)
  denoms <- sqrt(apply(decision_mat^2, 2, sum))

  for (i in seq(1, n_cols, 3)) {
    norm_mat[, i]   <- decision_mat[, i]   / denoms[i + 2]
    norm_mat[, i+1] <- decision_mat[, i+1] / denoms[i + 1]
    norm_mat[, i+2] <- decision_mat[, i+2] / denoms[i]
  }

  # 4. Weighting
  W_diag <- diag(final_w)
  weighted_mat <- norm_mat %*% W_diag

  # 5. Ideal Solutions
  pos_ideal <- ifelse(fuzzy_cb == "max", apply(weighted_mat, 2, max), apply(weighted_mat, 2, min))
  neg_ideal <- ifelse(fuzzy_cb == "min", apply(weighted_mat, 2, max), apply(weighted_mat, 2, min))

  # 6. Distances
  temp_d_pos <- (weighted_mat - matrix(pos_ideal, nrow=nrow(decision_mat), ncol=n_cols, byrow=TRUE))^2
  temp_d_neg <- (weighted_mat - matrix(neg_ideal, nrow=nrow(decision_mat), ncol=n_cols, byrow=TRUE))^2

  d_pos_fuzzy <- matrix(0, nrow(decision_mat), 3)
  d_neg_fuzzy <- matrix(0, nrow(decision_mat), 3)

  d_pos_fuzzy[,1] <- sqrt(apply(temp_d_pos[, seq(1, n_cols, 3), drop=FALSE], 1, sum))
  d_pos_fuzzy[,2] <- sqrt(apply(temp_d_pos[, seq(2, n_cols, 3), drop=FALSE], 1, sum))
  d_pos_fuzzy[,3] <- sqrt(apply(temp_d_pos[, seq(3, n_cols, 3), drop=FALSE], 1, sum))

  d_neg_fuzzy[,1] <- sqrt(apply(temp_d_neg[, seq(1, n_cols, 3), drop=FALSE], 1, sum))
  d_neg_fuzzy[,2] <- sqrt(apply(temp_d_neg[, seq(2, n_cols, 3), drop=FALSE], 1, sum))
  d_neg_fuzzy[,3] <- sqrt(apply(temp_d_neg[, seq(3, n_cols, 3), drop=FALSE], 1, sum))

  # 7. Closeness Coefficient (R)
  denom <- d_neg_fuzzy + d_pos_fuzzy
  R_fuzzy <- matrix(0, nrow(decision_mat), 3)
  R_fuzzy[,1] <- d_neg_fuzzy[,1] / denom[,3]
  R_fuzzy[,2] <- d_neg_fuzzy[,2] / denom[,2]
  R_fuzzy[,3] <- d_neg_fuzzy[,3] / denom[,1]

  def_score <- (R_fuzzy[,1] + 4*R_fuzzy[,2] + R_fuzzy[,3]) / 6

  # 8. Prepare Plotting Data (Defuzzify Distances)
  # We need scalar D+ and D- for the "Efficiency Map" plot
  scalar_D_pos <- rowMeans(d_pos_fuzzy)
  scalar_D_neg <- rowMeans(d_neg_fuzzy)

  result_df <- data.frame(
    Alternative = 1:nrow(decision_mat),
    D_plus = scalar_D_pos,
    D_minus = scalar_D_neg,
    Score = def_score,
    Ranking = rank(-def_score, ties.method = "first")
  )

  output <- list(
    results = result_df,
    method = "TOPSIS"
  )
  class(output) <- "fuzzy_topsis_res"
  return(output)
}

#' Fuzzy VIKOR Method
#'
#' @description Implements Fuzzy VIKOR with BWM integration. Returns an object for plotting.
#' @inheritParams fuzzy_topsis
#' @param v Numeric (0-1). Weight for the strategy of maximum group utility.
#' @return An object of class `fuzzy_vikor_res` containing S, R, and Q indices.
#' @export
fuzzy_vikor <- function(decision_mat, criteria_types, v = 0.5, weights, bwm_criteria, bwm_best, bwm_worst) {

  if (!is.matrix(decision_mat)) stop("'decision_mat' must be a matrix.")

  final_w <- .get_final_weights(decision_mat, weights, bwm_criteria, bwm_best, bwm_worst)

  n_cols <- ncol(decision_mat)
  fuzzy_cb <- character(n_cols)
  k <- 1
  for (j in seq(1, n_cols, 3)) {
    fuzzy_cb[j:(j+2)] <- criteria_types[k]
    k <- k + 1
  }

  # 1. Ideal Solutions
  pos_ideal <- ifelse(fuzzy_cb == "max", apply(decision_mat, 2, max), apply(decision_mat, 2, min))
  neg_ideal <- ifelse(fuzzy_cb == "min", apply(decision_mat, 2, max), apply(decision_mat, 2, min))

  # 2. Linear Normalization
  d_mat <- matrix(0, nrow = nrow(decision_mat), ncol = n_cols)

  for (i in seq(1, n_cols, 3)) {
    if (fuzzy_cb[i] == "max") {
      denom <- pos_ideal[i+2] - neg_ideal[i]
      if(denom == 0) denom <- 1e-9
      d_mat[, i]   <- (pos_ideal[i]   - decision_mat[, i+2]) / denom
      d_mat[, i+1] <- (pos_ideal[i+1] - decision_mat[, i+1]) / denom
      d_mat[, i+2] <- (pos_ideal[i+2] - decision_mat[, i])   / denom
    } else {
      denom <- neg_ideal[i+2] - pos_ideal[i]
      if(denom == 0) denom <- 1e-9
      d_mat[, i]   <- (decision_mat[, i]   - pos_ideal[i+2]) / denom
      d_mat[, i+1] <- (decision_mat[, i+1] - pos_ideal[i+1]) / denom
      d_mat[, i+2] <- (decision_mat[, i+2] - pos_ideal[i])   / denom
    }
  }

  W_diag <- diag(final_w)
  weighted_d <- d_mat %*% W_diag

  # 3. S and R Values
  S_fuzzy <- matrix(0, nrow(decision_mat), 3)
  R_fuzzy <- matrix(0, nrow(decision_mat), 3)

  S_fuzzy[,1] <- apply(weighted_d[, seq(1, n_cols, 3), drop=FALSE], 1, sum)
  S_fuzzy[,2] <- apply(weighted_d[, seq(2, n_cols, 3), drop=FALSE], 1, sum)
  S_fuzzy[,3] <- apply(weighted_d[, seq(3, n_cols, 3), drop=FALSE], 1, sum)

  R_fuzzy[,1] <- apply(weighted_d[, seq(1, n_cols, 3), drop=FALSE], 1, max)
  R_fuzzy[,2] <- apply(weighted_d[, seq(2, n_cols, 3), drop=FALSE], 1, max)
  R_fuzzy[,3] <- apply(weighted_d[, seq(3, n_cols, 3), drop=FALSE], 1, max)

  # 4. Q Index
  s_star <- min(S_fuzzy[,1])
  s_minus <- max(S_fuzzy[,3])
  r_star <- min(R_fuzzy[,1])
  r_minus <- max(R_fuzzy[,3])

  denom_s <- s_minus - s_star
  denom_r <- r_minus - r_star

  if (denom_s == 0) denom_s <- 1
  if (denom_r == 0) denom_r <- 1

  Q_fuzzy <- matrix(0, nrow(decision_mat), 3)
  term1 <- (S_fuzzy - s_star) / denom_s
  term2 <- (R_fuzzy - r_star) / denom_r
  Q_fuzzy <- v * term1 + (1 - v) * term2

  # Defuzzification
  def_S <- (S_fuzzy[,1] + 2*S_fuzzy[,2] + S_fuzzy[,3]) / 4
  def_R <- (R_fuzzy[,1] + 2*R_fuzzy[,2] + R_fuzzy[,3]) / 4
  def_Q <- (Q_fuzzy[,1] + 2*Q_fuzzy[,2] + Q_fuzzy[,3]) / 4

  result_df <- data.frame(
    Alternative = 1:nrow(decision_mat),
    Def_S = def_S,
    Def_R = def_R,
    Def_Q = def_Q,
    Ranking = rank(def_Q, ties.method = "first")
  )

  output <- list(
    results = result_df,
    details = list(S_fuzzy = S_fuzzy, R_fuzzy = R_fuzzy, Q_fuzzy = Q_fuzzy),
    params = list(v = v)
  )

  class(output) <- "fuzzy_vikor_res"
  return(output)
}

#' Fuzzy WASPAS Method
#'
#' @description Implements Fuzzy WASPAS with BWM integration.
#' @inheritParams fuzzy_topsis
#' @param lambda Numeric (0-1). WSM vs WPM importance.
#' @export
fuzzy_waspas <- function(decision_mat, criteria_types, lambda = 0.5, weights, bwm_criteria, bwm_best, bwm_worst) {

  if (!is.matrix(decision_mat)) stop("'decision_mat' must be a matrix.")
  final_w <- .get_final_weights(decision_mat, weights, bwm_criteria, bwm_best, bwm_worst)

  n_cols <- ncol(decision_mat)
  fuzzy_cb <- character(n_cols)
  k <- 1
  for (j in seq(1, n_cols, 3)) {
    fuzzy_cb[j:(j+2)] <- criteria_types[k]
    k <- k + 1
  }

  # 1. Normalization
  norm_base <- ifelse(fuzzy_cb == "max", apply(decision_mat, 2, max), apply(decision_mat, 2, min))
  N_mat <- matrix(0, nrow(decision_mat), n_cols)

  for (j in seq(1, n_cols, 3)) {
    if (fuzzy_cb[j] == "max") {
      N_mat[, j]   <- decision_mat[, j]   / norm_base[j+2]
      N_mat[, j+1] <- decision_mat[, j+1] / norm_base[j+2]
      N_mat[, j+2] <- decision_mat[, j+2] / norm_base[j+2]
    } else {
      N_mat[, j]   <- norm_base[j] / decision_mat[, j+2]
      N_mat[, j+1] <- norm_base[j] / decision_mat[, j+1]
      N_mat[, j+2] <- norm_base[j] / decision_mat[, j]
    }
  }

  # 2. WSM (Weighted Sum)
  W_diag <- diag(final_w)
  nw_sum <- N_mat %*% W_diag
  WSM_fuzzy <- matrix(0, nrow(decision_mat), 3)
  WSM_fuzzy[,1] <- apply(nw_sum[, seq(1, n_cols, 3), drop=FALSE], 1, sum)
  WSM_fuzzy[,2] <- apply(nw_sum[, seq(2, n_cols, 3), drop=FALSE], 1, sum)
  WSM_fuzzy[,3] <- apply(nw_sum[, seq(3, n_cols, 3), drop=FALSE], 1, sum)

  # 3. WPM (Weighted Product)
  nw_prod <- matrix(0, nrow(decision_mat), n_cols)
  for (j in seq(1, n_cols, 3)) {
    nw_prod[, j]   <- N_mat[, j]   ^ final_w[j+2]
    nw_prod[, j+1] <- N_mat[, j+1] ^ final_w[j+1]
    nw_prod[, j+2] <- N_mat[, j+2] ^ final_w[j]
  }
  WPM_fuzzy <- matrix(0, nrow(decision_mat), 3)
  WPM_fuzzy[,1] <- apply(nw_prod[, seq(1, n_cols, 3), drop=FALSE], 1, prod)
  WPM_fuzzy[,2] <- apply(nw_prod[, seq(2, n_cols, 3), drop=FALSE], 1, prod)
  WPM_fuzzy[,3] <- apply(nw_prod[, seq(3, n_cols, 3), drop=FALSE], 1, prod)

  # 4. Q Index
  def_wsm <- rowSums(WSM_fuzzy) / 3
  def_wpm <- rowSums(WPM_fuzzy) / 3

  Q_val <- lambda * def_wsm + (1 - lambda) * def_wpm

  result_df <- data.frame(
    Alternative = 1:nrow(decision_mat),
    WSM = def_wsm,
    WPM = def_wpm,
    Score = Q_val,
    Ranking = rank(-Q_val, ties.method = "first")
  )

  output <- list(
    results = result_df,
    method = "WASPAS",
    lambda = lambda
  )
  class(output) <- "fuzzy_waspas_res"
  return(output)
}

#' Calculate Entropy Weights (Data-Driven)
#'
#' @description Calculates objective weights using Shannon Entropy.
#' Useful when no expert preferences (BWM) are available.
#' @param decision_mat The fuzzy decision matrix from prepare_mcdm_data
#' @return A numeric vector of weights summing to 1.
#' @export
calculate_entropy_weights <- function(decision_mat) {

  # De-fuzzify matrix for entropy calc (using mean of l,m,u)
  n_cols <- ncol(decision_mat)
  crisp_mat <- matrix(0, nrow = nrow(decision_mat), ncol = n_cols/3)

  k <- 1
  for(j in seq(1, n_cols, 3)) {
    # Simple crisp conversion: (l + 4m + u) / 6 or just mean
    crisp_mat[, k] <- (decision_mat[, j] + 4*decision_mat[, j+1] + decision_mat[, j+2]) / 6
    k <- k + 1
  }

  # Normalize (P_ij)
  # Avoid divide by zero
  col_sums <- colSums(crisp_mat)
  col_sums[col_sums == 0] <- 1
  P <- sweep(crisp_mat, 2, col_sums, "/")

  # Calculate Entropy (E_j)
  # Handle log(0)
  k_const <- 1 / log(nrow(decision_mat))
  E <- numeric(ncol(P))

  for(j in 1:ncol(P)) {
    p_vals <- P[, j]
    p_vals <- p_vals[p_vals > 0] # Ignore zeros for log
    if(length(p_vals) == 0) {
      E[j] <- 1
    } else {
      E[j] <- -k_const * sum(p_vals * log(p_vals))
    }
  }

  # Calculate Weights (d_j and w_j)
  d <- 1 - E
  w <- d / sum(d)

  return(w)
}
#' @title Internal MULTIMOORA Dominance Aggregation
#' @description Aggregates the three internal MULTIMOORA rankings (RS, RP, MF).
#' @keywords internal
.multimoora_dominance <- function(r1, r2, r3) {
  n <- length(r1)
  final_rank <- rep(0, n)
  rank_mat <- cbind(r1, r2, r3)
  available <- rep(TRUE, n)

  for (pos in 1:n) {
    current_mat <- rank_mat
    current_mat[!available, ] <- Inf

    # Find candidates who have the best (min) rank in each method
    c1 <- which.min(current_mat[, 1])
    c2 <- which.min(current_mat[, 2])
    c3 <- which.min(current_mat[, 3])
    candidates <- c(c1, c2, c3)

    # Voting Mechanism
    freq <- table(candidates)
    winner <- as.numeric(names(freq)[which.max(freq)])

    # Tie-breaking: If 3 distinct winners, choose the one with lowest sum of ranks
    if (length(freq) == 3) {
      sums <- rowSums(rank_mat[candidates, ])
      winner <- candidates[which.min(sums)]
    }

    final_rank[winner] <- pos
    available[winner] <- FALSE
  }
  return(final_rank)
}

#' Fuzzy MULTIMOORA Method
#'
#' @description Implements the Fuzzy MULTIMOORA method (Balezentis et al., 2014).
#' It consists of three parts:
#' 1. The Ratio System (RS)
#' 2. The Reference Point (RP) approach
#' 3. The Full Multiplicative Form (FMF)
#'
#' The final ranking is derived by aggregating these three using the Theory of Dominance.
#'
#' @inheritParams fuzzy_topsis
#' @return An object of class `fuzzy_multimoora_res` containing rankings for all three sub-methods and the final consensus.
#' @export
fuzzy_multimoora <- function(decision_mat, criteria_types, weights, bwm_criteria, bwm_best, bwm_worst) {
  if (!is.matrix(decision_mat)) stop("'decision_mat' must be a matrix.")

  # 1. Weights Calculation
  final_w <- .get_final_weights(decision_mat, weights, bwm_criteria, bwm_best, bwm_worst)

  n_rows <- nrow(decision_mat)
  n_cols <- ncol(decision_mat) # 3 * n_criteria

  # Expand criteria types to fuzzy columns
  fuzzy_types <- character(n_cols)
  k <- 1
  for (j in seq(1, n_cols, 3)) {
    fuzzy_types[j:(j+2)] <- criteria_types[k]
    k <- k + 1
  }

  # 2. Vector Normalization (Standard MOORA approach: x / sqrt(sum(x^2)))
  norm_mat <- matrix(0, nrow = n_rows, ncol = n_cols)
  for (i in seq(1, n_cols, 3)) {
    # Calculate denominator based on the triplet columns
    denom <- sqrt(sum(decision_mat[,i]^2 + decision_mat[,i+1]^2 + decision_mat[,i+2]^2))
    if (denom == 0) denom <- 1
    norm_mat[,i]   <- decision_mat[,i]   / denom
    norm_mat[,i+1] <- decision_mat[,i+1] / denom
    norm_mat[,i+2] <- decision_mat[,i+2] / denom
  }

  # --- PART A: Ratio System (RS) ---
  # Arithmetic: Sum(Max_Weighted) - Sum(Min_Weighted)
  # Weighted Matrix for RS (x * w)
  rs_weighted <- norm_mat
  for (j in 1:n_cols) {
    rs_weighted[, j] <- norm_mat[, j] * final_w[j]
  }

  rs_fuzzy <- matrix(0, nrow = n_rows, ncol = 3)

  for (j in seq(1, n_cols, 3)) {
    if (fuzzy_types[j] == 'max') {
      # Add Benefit
      rs_fuzzy[,1] <- rs_fuzzy[,1] + rs_weighted[, j]
      rs_fuzzy[,2] <- rs_fuzzy[,2] + rs_weighted[, j+1]
      rs_fuzzy[,3] <- rs_fuzzy[,3] + rs_weighted[, j+2]
    } else {
      # Subtract Cost (Fuzzy Subtraction: l-u, m-m, u-l)
      rs_fuzzy[,1] <- rs_fuzzy[,1] - rs_weighted[, j+2]
      rs_fuzzy[,2] <- rs_fuzzy[,2] - rs_weighted[, j+1]
      rs_fuzzy[,3] <- rs_fuzzy[,3] - rs_weighted[, j]
    }
  }
  # Defuzzify RS (Mean)
  def_rs <- rowMeans(rs_fuzzy)
  rank_rs <- rank(-def_rs, ties.method = "first")

  # --- PART B: Reference Point (RP) ---
  # Metric: Min-Max (Chebyshev distance from Ideal Reference)
  # Reference Point (Max of Weighted Norm for Benefit, Min for Cost)
  # NOTE: In MOORA, RP usually uses the Weighted Matrix calculated above.

  ref_point <- numeric(n_cols)
  for (j in 1:n_cols) {
    if (fuzzy_types[j] == 'max') ref_point[j] <- max(rs_weighted[, j])
    else ref_point[j] <- min(rs_weighted[, j])
  }

  # Calculate distances
  dists <- matrix(0, nrow = n_rows, ncol = n_cols/3)
  k <- 1
  for (j in seq(1, n_cols, 3)) {
    # Euclidean distance of fuzzy triplet from crisp reference scalar (or fuzzy ref)
    # Here treating ref_point as vector components
    d_l <- (rs_weighted[, j]   - ref_point[j])^2
    d_m <- (rs_weighted[, j+1] - ref_point[j+1])^2
    d_u <- (rs_weighted[, j+2] - ref_point[j+2])^2
    dists[, k] <- sqrt(d_l + d_m + d_u)
    k <- k + 1
  }

  # Max distance for each alternative
  def_rp <- apply(dists, 1, max)
  rank_rp <- rank(def_rp, ties.method = "first")

  # --- PART C: Full Multiplicative Form (FMF) ---
  # Logic: Product(Benefit^w) / Product(Cost^w)
  # We use the normalized matrix 'norm_mat' (NOT 'rs_weighted') with weights as exponents

  # Initialize Fuzzy Products as 1 (1,1,1)
  prod_benefit <- matrix(1, nrow = n_rows, ncol = 3)
  prod_cost    <- matrix(1, nrow = n_rows, ncol = 3)

  for (j in seq(1, n_cols, 3)) {
    w <- final_w[j+1] # Use middle weight (crisp) for exponent
    # Extract triplet
    triplet <- norm_mat[, j:(j+2)]

    if (fuzzy_types[j] == 'max') {
      # Benefit: Multiply by x^w
      prod_benefit[,1] <- prod_benefit[,1] * (triplet[,1]^w)
      prod_benefit[,2] <- prod_benefit[,2] * (triplet[,2]^w)
      prod_benefit[,3] <- prod_benefit[,3] * (triplet[,3]^w)
    } else {
      # Cost: Multiply into denominator (x^w)
      prod_cost[,1] <- prod_cost[,1] * (triplet[,1]^w)
      prod_cost[,2] <- prod_cost[,2] * (triplet[,2]^w)
      prod_cost[,3] <- prod_cost[,3] * (triplet[,3]^w)
    }
  }

  # Fuzzy Division: Benefit / Cost = (B_l/C_u, B_m/C_m, B_u/C_l)
  # Handle division by zero safety
  prod_cost[prod_cost == 0] <- 1e-9

  fmf_fuzzy <- matrix(0, nrow = n_rows, ncol = 3)
  fmf_fuzzy[,1] <- prod_benefit[,1] / prod_cost[,3]
  fmf_fuzzy[,2] <- prod_benefit[,2] / prod_cost[,2]
  fmf_fuzzy[,3] <- prod_benefit[,3] / prod_cost[,1]

  def_fmf <- rowMeans(fmf_fuzzy)
  rank_fmf <- rank(-def_fmf, ties.method = "first")

  # --- PART D: Aggregation (Theory of Dominance) ---
  final_rank <- .multimoora_dominance(rank_rs, rank_rp, rank_fmf)

  # Compile Results
  result_df <- data.frame(
    Alternative = 1:n_rows,
    RS_Score = def_rs,
    RS_Rank = rank_rs,
    RP_Score = def_rp,
    RP_Rank = rank_rp,
    FMF_Score = def_fmf,
    FMF_Rank = rank_fmf,
    Final_Rank = final_rank
  )

  output <- list(
    results = result_df,
    method = "MULTIMOORA"
  )
  class(output) <- "fuzzy_multimoora_res"
  return(output)
}

#' @title Internal NEAT Preference Calculator
#' @description Calculates preference values P(d) applying the NEAT F-PROMETHEE
#' correction mechanism to reduce approximation errors (Ziemba, 2018).
#' @keywords internal
.calc_neat_preference <- function(d1, d2, d3, d4, type, q, p, s) {
  # Initialize P matrices
  P1 <- matrix(0, nrow(d1), ncol(d1))
  P2 <- matrix(0, nrow(d1), ncol(d1))
  P3 <- matrix(0, nrow(d1), ncol(d1))
  P4 <- matrix(0, nrow(d1), ncol(d1))

  # --- 1. Simple Mapping (Standard Formulas) ---
  # Helper to apply function to a matrix
  apply_fun <- function(mat) {
    if (type == "usual") {
      ifelse(mat > 0, 1, 0)
    } else if (type == "u-shape") {
      ifelse(mat > q, 1, 0)
    } else if (type == "v-shape") {
      ifelse(mat > p, 1, ifelse(mat <= 0, 0, mat / p))
    } else if (type == "level") {
      ifelse(mat > p, 1, ifelse(mat > q, 0.5, 0))
    } else if (type == "linear") { # V-shape with indifference
      ifelse(mat > p, 1, ifelse(mat <= q, 0, (mat - q) / (p - q)))
    } else if (type == "gaussian") {
      ifelse(mat <= 0, 0, 1 - exp(-(mat^2) / (2 * s^2)))
    } else {
      stop("Unknown preference function type.")
    }
  }

  P1 <- apply_fun(d1)
  P2 <- apply_fun(d2)
  P3 <- apply_fun(d3)
  P4 <- apply_fun(d4)

  # --- 2. Correction Mechanism (The "NEAT" part) ---
  # See Ziemba (2018), Eqs 6, 8, 10, 12, 14, 16
  # We check specific vertices (d2 and d3) against thresholds

  if (type == "usual") {
    # Eq (6)
    mask_d2 <- (d1 < 0 & 0 < d2) & ((-d1 / (d2 - d1)) > 0.5)
    P2[mask_d2] <- 0
    mask_d3 <- (d3 < 0 & 0 < d4) & ((-d4 / (d3 - d4)) > 0.5)
    P3[mask_d3] <- 1

  } else if (type == "u-shape") {
    # Eq (8)
    mask_d2 <- (d1 < q & q < d2) & ((q - d1) / (d2 - d1) > 0.5)
    P2[mask_d2] <- 0
    mask_d3 <- (d3 < q & q < d4) & ((q - d4) / (d3 - d4) > 0.5)
    P3[mask_d3] <- 1

  } else if (type == "v-shape") {
    # Eq (10)
    mask_d2 <- (d1 < 0 & 0 < d2) & ((-d1 / (d2 - d1)) > 0.5)
    P2[mask_d2] <- 0
    mask_d3 <- (d3 < p & p < d4) & ((p - d4) / (d3 - d4) > 0.5)
    P3[mask_d3] <- 1

  } else if (type == "level") {
    # Eq (12)
    mask_d2 <- (d1 < q & q < d2) & ((q - d1) / (d2 - d1) > 0.5)
    P2[mask_d2] <- 0
    mask_d3 <- (d3 < p & p < d4) & ((p - d4) / (d3 - d4) > 0.5)
    P3[mask_d3] <- 1

  } else if (type == "linear") {
    # Eq (14)
    mask_d2 <- (d1 < q & q < d2) & ((q - d1) / (d2 - d1) > 0.5)
    P2[mask_d2] <- 0
    mask_d3 <- (d3 < p & p < d4) & ((p - d4) / (d3 - d4) > 0.5)
    P3[mask_d3] <- 1

  } else if (type == "gaussian") {
    # Eq (16) - Note: Only P2 is corrected in Eq 16
    mask_d2 <- (d1 < 0 & 0 < d2) & ((-d1 / (d2 - d1)) > 0.5)
    P2[mask_d2] <- 0
  }

  return(list(P1=P1, P2=P2, P3=P3, P4=P4))
}

#' Fuzzy PROMETHEE II (NEAT Implementation)
#'
#' @description Implements the NEAT F-PROMETHEE II method (Ziemba, 2018).
#' It calculates the Net Outranking Flow based on Trapezoidal Fuzzy Numbers
#' and includes the specific mapping correction mechanism to minimize
#' approximation errors.
#'
#' @param decision_mat The fuzzy decision matrix (m x 3n).
#' @param preference_params A data.frame with columns:
#'   1. `Type` ("usual", "u-shape", "v-shape", "level", "linear", "gaussian")
#'   2. `q` (indifference threshold)
#'   3. `p` (preference threshold)
#'   4. `s` (gaussian sigma)
#'   5. `Role` ("min" or "max")
#' @inheritParams fuzzy_topsis
#' @return An object of class `fuzzy_promethee_res`.
#' @export
fuzzy_promethee <- function(decision_mat, preference_params, weights,
                            bwm_criteria, bwm_best, bwm_worst) {

  # 1. Weights Handling (Crisp & Normalized)
  # Ziemba (2018): Weights must be defuzzified and normalized to 1.
  fuzzy_w <- .get_final_weights(decision_mat, weights, bwm_criteria, bwm_best, bwm_worst)

  # Extract triplets (l,m,u) and defuzzify to centroid (crisp)
  n_crit <- ncol(decision_mat) / 3
  crisp_w <- numeric(n_crit)
  for(j in 1:n_crit) {
    idx <- (j-1)*3 + 1
    # Centroid of TFN (a,b,c) -> (a + b + c) / 3 is approx,
    # but package uses Triplet. Standard approx: (l + 4m + u)/6 or mean.
    # Ziemba uses TFN centroid. For TFN (l, m, m, u):
    w_vec <- fuzzy_w[idx:(idx+2)]
    crisp_w[j] <- (w_vec[1] + w_vec[2] + w_vec[3]) / 3
  }
  # Normalize
  crisp_w <- crisp_w / sum(crisp_w)

  n_alt <- nrow(decision_mat)

  # Initialize Aggregated Preference Indices (Fuzzy TFN: P1, P2, P3, P4)
  # Note: The package uses Triplets (L,M,U), but NEAT uses Trapezoidal logic internally.
  # We will map Triplet Input -> Trapezoidal Calc -> Triplet Output.
  # TFN Input (l, m, u) is Trapezoidal (l, m, m, u).
  Pi_1 <- matrix(0, n_alt, n_alt)
  Pi_2 <- matrix(0, n_alt, n_alt)
  Pi_3 <- matrix(0, n_alt, n_alt)
  Pi_4 <- matrix(0, n_alt, n_alt)

  # 2. Iterate Criteria
  for (j in 1:n_crit) {
    # Parameters
    p_type <- as.character(preference_params[j, 1])
    q_val  <- as.numeric(preference_params[j, 2])
    p_val  <- as.numeric(preference_params[j, 3])
    s_val  <- as.numeric(preference_params[j, 4])
    role   <- as.character(preference_params[j, 5])

    # Get Fuzzy Columns (L, M, U)
    idx_l <- (j-1)*3 + 1
    idx_m <- (j-1)*3 + 2
    idx_u <- (j-1)*3 + 3
    col_l <- decision_mat[, idx_l]
    col_m <- decision_mat[, idx_m]
    col_u <- decision_mat[, idx_u]

    # Calculate Fuzzy Deviation (Trapezoidal Arithmetic: A - B)
    # A = (a1, a2, a3, a4), B = (b1, b2, b3, b4)
    # A - B = (a1-b4, a2-b3, a3-b2, a4-b1)
    # Here Input is Triangular (l, m, u) => Trapezoidal (l, m, m, u)

    if (role == "max") {
      # Benefit: Alt - Other
      d1 <- outer(col_l, col_u, "-") # a1 - b4
      d2 <- outer(col_m, col_m, "-") # a2 - b3
      d3 <- outer(col_m, col_m, "-") # a3 - b2
      d4 <- outer(col_u, col_l, "-") # a4 - b1
    } else {
      # Cost: Other - Alt
      d1 <- outer(col_u, col_l, "-") * -1 # b1 - a4 (reversed logic) -> a1_cost - b4_cost
      d2 <- outer(col_m, col_m, "-") * -1
      d3 <- outer(col_m, col_m, "-") * -1
      d4 <- outer(col_l, col_u, "-") * -1
    }

    # Calculate Preference with Correction
    P_list <- .calc_neat_preference(d1, d2, d3, d4, p_type, q_val, p_val, s_val)

    # Weighted Aggregation
    w <- crisp_w[j]
    Pi_1 <- Pi_1 + (P_list$P1 * w)
    Pi_2 <- Pi_2 + (P_list$P2 * w)
    Pi_3 <- Pi_3 + (P_list$P3 * w)
    Pi_4 <- Pi_4 + (P_list$P4 * w)
  }

  # Zero diagonal
  diag(Pi_1) <- 0; diag(Pi_2) <- 0; diag(Pi_3) <- 0; diag(Pi_4) <- 0

  # 3. Flows Calculation (Fuzzy)
  # Phi+ (Leaving): Row Sum / (n-1)
  Phi_plus_1 <- rowSums(Pi_1) / (n_alt - 1)
  Phi_plus_2 <- rowSums(Pi_2) / (n_alt - 1)
  Phi_plus_3 <- rowSums(Pi_3) / (n_alt - 1)
  Phi_plus_4 <- rowSums(Pi_4) / (n_alt - 1)

  # Phi- (Entering): Col Sum / (n-1)
  Phi_minus_1 <- colSums(Pi_1) / (n_alt - 1)
  Phi_minus_2 <- colSums(Pi_2) / (n_alt - 1)
  Phi_minus_3 <- colSums(Pi_3) / (n_alt - 1)
  Phi_minus_4 <- colSums(Pi_4) / (n_alt - 1)

  # 4. Defuzzification (Centroid)
  # Ziemba (2018): Defuzzify flows before Net Flow
  def_Phi_plus  <- (Phi_plus_1 + Phi_plus_2 + Phi_plus_3 + Phi_plus_4) / 4
  def_Phi_minus <- (Phi_minus_1 + Phi_minus_2 + Phi_minus_3 + Phi_minus_4) / 4

  # 5. Net Flow
  Phi_net <- def_Phi_plus - def_Phi_minus

  # Results
  result_df <- data.frame(
    Alternative = 1:n_alt,
    Phi_Plus = def_Phi_plus,
    Phi_Minus = def_Phi_minus,
    Phi_Net = Phi_net,
    Ranking = rank(-Phi_net, ties.method = "first")
  )

  output <- list(
    results = result_df,
    method = "NEAT F-PROMETHEE II",
    weights = crisp_w
  )
  class(output) <- "fuzzy_promethee_res"
  return(output)
}
