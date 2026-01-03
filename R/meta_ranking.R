#' @title Theory of Dominance for Ranking Consensus
#' @description
#' Calculates a consensus ranking based on the Theory of Dominance. It compares
#' three distinct rankings and determines a final position based on majority rule
#' and dominance logic.
#'
#' @param r1 Numeric vector. First ranking.
#' @param r2 Numeric vector. Second ranking.
#' @param r3 Numeric vector. Third ranking.
#' @return A numeric vector containing the final consensus ranking.
#' @keywords internal
calculate_dominance_ranking <- function(r1, r2, r3) {
  n <- length(r1)
  final_rank <- rep(0, n)

  # Create a matrix of the rankings
  rank_mat <- cbind(r1, r2, r3)

  # Initialize mask for available alternatives (all TRUE initially)
  available <- rep(TRUE, n)

  for (current_position in 1:n) {
    # Get current ranks for available alternatives
    current_mat <- rank_mat
    current_mat[!available, ] <- Inf

    # Find which alternative has the minimum rank in each method
    best_r1 <- which.min(current_mat[, 1])
    best_r2 <- which.min(current_mat[, 2])
    best_r3 <- which.min(current_mat[, 3])

    candidates <- c(best_r1, best_r2, best_r3)

    # Majority Voting
    freq_table <- table(candidates)
    winner_idx <- as.numeric(names(freq_table)[which.max(freq_table)])

    # Tie-breaking logic (if 3 distinct winners)
    if (length(freq_table) == 3) {
      c1 <- best_r1; c2 <- best_r2; c3 <- best_r3

      c1_wins <- sum(rank_mat[c1, ] < rank_mat[c2, ]) + sum(rank_mat[c1, ] < rank_mat[c3, ])
      c2_wins <- sum(rank_mat[c2, ] < rank_mat[c1, ]) + sum(rank_mat[c2, ] < rank_mat[c3, ])
      c3_wins <- sum(rank_mat[c3, ] < rank_mat[c1, ]) + sum(rank_mat[c3, ] < rank_mat[c2, ])

      wins <- c(c1_wins, c2_wins, c3_wins)

      if (which.max(wins) == 1) winner_idx <- c1
      else if (which.max(wins) == 2) winner_idx <- c2
      else winner_idx <- c3
    }

    # Assign rank and mark as unavailable
    final_rank[winner_idx] <- current_position
    available[winner_idx] <- FALSE
  }

  return(final_rank)
}


#' @title Fuzzy Meta-Ranking
#' @description
#' Aggregates results from multiple Fuzzy MCDM methods (VIKOR, TOPSIS, WASPAS)
#' to produce a robust consensus ranking.
#'
#' @param decision_mat A fuzzy decision matrix (output of `prepare_mcdm_data` or similar structure).
#' @param criteria_types Character vector defining criteria types (e.g., c("min", "max", ...)).
#' @param weights Numeric vector of weights. If NULL, entropy weights are calculated.
#' @param bwm_best (Optional) Vector for Best-Worst Method best comparison.
#' @param bwm_worst (Optional) Vector for Best-Worst Method worst comparison.
#' @param lambda Parameter for WASPAS (default 0.5).
#' @param v Parameter for VIKOR (default 0.5).
#'
#' @return A list containing:
#' \item{comparison}{Data frame with individual method rankings and the meta-rankings.}
#' \item{correlations}{Spearman correlation matrix between methods.}
#'
#' @importFrom RankAggreg BruteAggreg RankAggreg
#' @export
fuzzy_meta_ranking <- function(decision_mat,
                               criteria_types,
                               weights = NULL,
                               bwm_best = NULL,
                               bwm_worst = NULL,
                               lambda = 0.5,
                               v = 0.5) {

  # 1. Input Validation & Weights
  if (is.null(weights) && (is.null(bwm_best) || is.null(bwm_worst))) {
    message("No weights provided. Calculating Entropy Weights...")
    weights_raw <- calculate_entropy_weights(decision_mat)
    weights <- rep(weights_raw, each = 3)
  }

  # 2. Run Individual Methods
  args_base <- list(decision_mat = decision_mat, criteria_types = criteria_types)
  if (!is.null(weights)) args_base$weights <- weights
  if (!is.null(bwm_best)) {
    args_base$bwm_best <- bwm_best
    args_base$bwm_worst <- bwm_worst
  }

  # VIKOR
  args_vikor <- c(args_base, list(v = v))
  res_vikor <- do.call(fuzzy_vikor, args_vikor)

  # TOPSIS
  res_topsis <- do.call(fuzzy_topsis, args_base)

  # WASPAS
  args_waspas <- c(args_base, list(lambda = lambda))
  res_waspas <- do.call(fuzzy_waspas, args_waspas)

  # 3. Extract Rankings
  r_vikor  <- res_vikor$results$Ranking
  r_topsis <- res_topsis$results$Ranking
  r_waspas <- res_waspas$results$Ranking

  # 4. Calculate Meta-Rankings

  # A. Simple Sum Ranking (Lower sum is better)
  sum_scores <- r_vikor + r_topsis + r_waspas
  rank_sum <- rank(sum_scores, ties.method = "first")

  # B. Theory of Dominance Consensus
  rank_dominance <- calculate_dominance_ranking(r_vikor, r_topsis, r_waspas)

  # C. RankAggreg (Evolutionary Algorithm / Brute Force)
  # FIX: We must convert Ranks (Values) to Ordered Lists (Indices)
  # RankAggreg expects: Row 1 = [Index of #1 Winner, Index of #2 Winner, ...]
  ra_matrix <- rbind(
    order(r_vikor),
    order(r_topsis),
    order(r_waspas)
  )
  n_alt <- nrow(decision_mat)

  if (n_alt <= 10) {
    ra_res <- RankAggreg::BruteAggreg(ra_matrix, n_alt, distance = "Spearman")
  } else {
    ra_res <- RankAggreg::RankAggreg(ra_matrix, n_alt, method = "GA", distance = "Spearman", verbose = FALSE)
  }

  # --- ROBUST MAPPING LOGIC ---
  # Initialize vector with row names to ensure correct matching
  rank_aggreg_vec <- numeric(n_alt)
  names(rank_aggreg_vec) <- rownames(decision_mat)

  top_list <- ra_res$top.list

  # Check if top_list is Indices (Numbers) or Names (Strings)
  if (is.numeric(top_list) || all(grepl("^[0-9]+$", top_list))) {
    # It is indices (e.g., 2, 4, 3, 1)
    top_indices <- as.numeric(top_list)
    for(rank_pos in 1:n_alt) {
      # The alternative at 'top_indices[rank_pos]' gets rank 'rank_pos'
      rank_aggreg_vec[top_indices[rank_pos]] <- rank_pos
    }
  } else {
    # It is names (e.g., "Supplier_A", "Supplier_B")
    for(rank_pos in 1:n_alt) {
      # The alternative with this name gets rank 'rank_pos'
      alt_name <- top_list[rank_pos]
      rank_aggreg_vec[alt_name] <- rank_pos
    }
  }

  # Clean up names for final dataframe
  rank_aggreg_vec <- as.numeric(rank_aggreg_vec)

  # 5. Compile Results
  comparison_df <- data.frame(
    Alternative = rownames(decision_mat),
    R_VIKOR = r_vikor,
    R_TOPSIS = r_topsis,
    R_WASPAS = r_waspas,
    Meta_Sum = rank_sum,
    Meta_Dominance = rank_dominance,
    Meta_Aggreg = rank_aggreg_vec
  )

  # Correlation Matrix between methods
  cor_mat <- cor(comparison_df[,-1], method = "spearman")

  result <- list(
    comparison = comparison_df,
    correlations = cor_mat
  )

  return(result)
}
