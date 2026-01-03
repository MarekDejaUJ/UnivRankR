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
