#' @title Theory of Dominance for Ranking Consensus
#' @description
#' Calculates a consensus ranking based on the Theory of Dominance. It compares
#' multiple rankings and determines a final position based on majority rule
#' and dominance logic.
#'
#' @param rank_mat A matrix where columns are methods and rows are alternatives.
#' @return A numeric vector containing the final consensus ranking.
#' @keywords internal
calculate_dominance_ranking <- function(rank_mat) {
  n <- nrow(rank_mat)
  n_methods <- ncol(rank_mat)
  final_rank <- rep(0, n)

  # Initialize mask for available alternatives (all TRUE initially)
  available <- rep(TRUE, n)

  for (current_position in 1:n) {
    # Get current ranks for available alternatives
    current_mat <- rank_mat
    current_mat[!available, ] <- Inf

    # Find candidates: which alternative has the minimum rank in each method?
    candidates <- apply(current_mat, 2, which.min)

    # Majority Voting
    freq_table <- table(candidates)

    # Find the candidate with the most votes
    max_votes <- max(freq_table)
    winners <- as.numeric(names(freq_table)[freq_table == max_votes])

    if (length(winners) == 1) {
      winner_idx <- winners
    } else {
      # Tie-breaking: Choose the one with the lowest sum of ranks across all methods
      # (Lower sum = generally better performance)
      sums <- rowSums(rank_mat[winners, , drop = FALSE])
      winner_idx <- winners[which.min(sums)]
    }

    # Assign rank and mark as unavailable
    final_rank[winner_idx] <- current_position
    available[winner_idx] <- FALSE
  }

  return(final_rank)
}

#' @title Fuzzy Meta-Ranking
#' @description
#' Aggregates results from 5 Fuzzy MCDM methods (VIKOR, TOPSIS, WASPAS, MULTIMOORA, PROMETHEE)
#' to produce a robust consensus ranking.
#'
#' @param decision_mat A fuzzy decision matrix (output of `prepare_mcdm_data`).
#' @param criteria_types Character vector defining criteria types (e.g., c("min", "max")).
#' @param weights Numeric vector of weights. If NULL, entropy weights are calculated.
#' @param preference_params (Optional) Dataframe for PROMETHEE parameters.
#'        If NULL, defaults (Linear, q=0, p=2) are used.
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
                               preference_params = NULL,
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

  # PROMETHEE Defaults if missing
  if (is.null(preference_params)) {
    message("No preference_params provided for PROMETHEE. Using defaults (Linear, q=0, p=2).")
    n_crit <- ncol(decision_mat) / 3
    preference_params <- data.frame(
      Type = rep("linear", n_crit),
      q = rep(0, n_crit),
      p = rep(2, n_crit),
      s = rep(NA, n_crit),
      Role = rep("max", n_crit)
    )
    # Mapping roles from criteria_types
    for(j in 1:n_crit) {
      preference_params$Role[j] <- criteria_types[(j-1)*3 + 1]
    }
  }

  # 2. Run Individual Methods
  args_base <- list(decision_mat = decision_mat, criteria_types = criteria_types)
  if (!is.null(weights)) args_base$weights <- weights
  if (!is.null(bwm_best)) {
    args_base$bwm_best <- bwm_best
    args_base$bwm_worst <- bwm_worst
  }

  # A. VIKOR
  args_vikor <- c(args_base, list(v = v))
  res_vikor <- do.call(fuzzy_vikor, args_vikor)

  # B. TOPSIS
  res_topsis <- do.call(fuzzy_topsis, args_base)

  # C. WASPAS
  args_waspas <- c(args_base, list(lambda = lambda))
  res_waspas <- do.call(fuzzy_waspas, args_waspas)

  # D. MULTIMOORA
  res_mm <- do.call(fuzzy_multimoora, args_base)

  # E. PROMETHEE
  # FIX: Remove 'criteria_types' because fuzzy_promethee does not accept it.
  # (It uses preference_params$Role instead)
  args_prom <- args_base
  args_prom$criteria_types <- NULL
  args_prom <- c(args_prom, list(preference_params = preference_params))
  res_prom <- do.call(fuzzy_promethee, args_prom)

  # 3. Extract Rankings
  r_vikor   <- res_vikor$results$Ranking
  r_topsis  <- res_topsis$results$Ranking
  r_waspas  <- res_waspas$results$Ranking
  r_mm      <- res_mm$results$Final_Rank
  r_prom    <- res_prom$results$Ranking

  # 4. Calculate Meta-Rankings

  # Compile Matrix (Rows = Alts, Cols = Methods)
  rank_matrix_cols <- cbind(r_vikor, r_topsis, r_waspas, r_mm, r_prom)
  colnames(rank_matrix_cols) <- c("VIKOR", "TOPSIS", "WASPAS", "MMOORA", "PROM")

  # A. Simple Sum Ranking
  sum_scores <- rowSums(rank_matrix_cols)
  rank_sum <- rank(sum_scores, ties.method = "first")

  # B. Theory of Dominance Consensus
  rank_dominance <- calculate_dominance_ranking(rank_matrix_cols)

  # C. RankAggreg (GA / Brute Force)
  ra_matrix <- rbind(
    order(r_vikor),
    order(r_topsis),
    order(r_waspas),
    order(r_mm),
    order(r_prom)
  )

  n_alt <- nrow(decision_mat)
  if (n_alt <= 10) {
    ra_res <- RankAggreg::BruteAggreg(ra_matrix, n_alt, distance = "Spearman")
  } else {
    ra_res <- RankAggreg::RankAggreg(ra_matrix, n_alt, method = "GA", distance = "Spearman", verbose = FALSE)
  }

  # Parse RankAggreg result to ranking vector
  rank_aggreg_vec <- numeric(n_alt)
  names(rank_aggreg_vec) <- rownames(decision_mat)
  top_list <- ra_res$top.list

  if (is.numeric(top_list) || all(grepl("^[0-9]+$", top_list))) {
    top_indices <- as.numeric(top_list)
    for(rank_pos in 1:n_alt) {
      rank_aggreg_vec[top_indices[rank_pos]] <- rank_pos
    }
  } else {
    for(rank_pos in 1:n_alt) {
      alt_name <- top_list[rank_pos]
      rank_aggreg_vec[alt_name] <- rank_pos
    }
  }

  # 5. Compile Results
  comparison_df <- data.frame(
    Alternative = rownames(decision_mat),
    R_VIKOR = r_vikor,
    R_TOPSIS = r_topsis,
    R_WASPAS = r_waspas,
    R_MMOORA = r_mm,
    R_PROM = r_prom,
    Meta_Sum = rank_sum,
    Meta_Dominance = rank_dominance,
    Meta_Aggreg = as.numeric(rank_aggreg_vec)
  )

  cor_mat <- cor(comparison_df[,-1], method = "spearman")

  result <- list(
    comparison = comparison_df,
    correlations = cor_mat
  )
  return(result)
}
