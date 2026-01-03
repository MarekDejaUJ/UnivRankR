#' @title Internal BWM Assertions
#' @description Internal function to validate BWM inputs.
#' @keywords internal
.assert_bwm <- function(expression, message) {
  if (!all(expression)) {
    stop(if (is.null(message)) "Error" else message)
  }
}

#' @title Internal Data Validator
#' @description Prepares and validates comparison vectors.
#' @keywords internal
.validate_bwm_data <- function(best_to_others, others_to_worst, criteria_names) {
  .assert_bwm(length(best_to_others) > 1, "Length of comparison vectors must be > 1.")
  .assert_bwm(length(best_to_others) == length(others_to_worst), "Mismatch in vector lengths.")
  .assert_bwm(length(best_to_others) == length(criteria_names), "Mismatch between criteria names and vectors.")
  .assert_bwm(1 %in% best_to_others, "'best_to_others' must contain the value 1 (Best).")
  .assert_bwm(1 %in% others_to_worst, "'others_to_worst' must contain the value 1 (Worst).")
  .assert_bwm(all(best_to_others >= 1 & best_to_others <= 9), "Scores must be between 1 and 9.")

  list(best_to_others = best_to_others, others_to_worst = others_to_worst, criteria_names = criteria_names)
}

#' @title Internal Consistency Check
#' @keywords internal
.check_consistency <- function(model) {
  worst_idx <- match(1, model$others_to_worst)
  best_over_worst <- model$best_to_others[worst_idx]

  # a_bj * a_jw = a_bw
  list(
    is_consistent = all(model$best_to_others * model$others_to_worst == best_over_worst),
    a_bw = best_over_worst
  )
}

#' @title Internal Constraint Combiner
#' @keywords internal
.combine_constraints <- function(constraints, new_constraint) {
  idx <- length(constraints) + 1
  constraints[[idx]] <- new_constraint
  list(constraints = constraints, added = TRUE)
}

#' @title Calculate BWM Weights
#'
#' @description Calculates criteria weights using the Best-Worst Method (BWM) via Linear Programming.
#' @param criteria_names Character vector of criteria names.
#' @param best_to_others Numeric vector (1-9). Preference of Best criterion over others.
#' @param others_to_worst Numeric vector (1-9). Preference of others over Worst criterion.
#' @return A list containing `criteria_weights`, `consistency_ratio`, and metadata.
#' @import Rglpk
#' @export
calculate_bwm_weights <- function(criteria_names, best_to_others, others_to_worst) {

  # 1. Validation and Model Building
  data <- .validate_bwm_data(best_to_others, others_to_worst, criteria_names)
  consistency <- .check_consistency(data)

  n_vars <- length(best_to_others) + 1 # Weights + ksi
  ksi_idx <- n_vars

  # Basic Constraints: Sum weights = 1, weights >= 0
  lhs_sum <- c(rep(1, n_vars - 1), 0)
  constraints <- list(
    list(lhs = lhs_sum, dir = "==", rhs = 1)
  )

  # Constraints: |w_b - a_bj * w_j| <= ksi
  best_idx <- match(1, best_to_others)

  for (j in seq_along(best_to_others)) {
    if (j != best_idx) {
      # Equation 1: w_b - a_bj * w_j - ksi <= 0
      lhs1 <- rep(0, n_vars)
      lhs1[best_idx] <- 1
      lhs1[j] <- -best_to_others[j]
      lhs1[ksi_idx] <- -1
      constraints <- .combine_constraints(constraints, list(lhs = lhs1, dir = "<=", rhs = 0))$constraints

      # Equation 2: -w_b + a_bj * w_j - ksi <= 0
      lhs2 <- lhs1 * -1
      lhs2[ksi_idx] <- -1 # ksi always subtracted
      constraints <- .combine_constraints(constraints, list(lhs = lhs2, dir = "<=", rhs = 0))$constraints
    }
  }

  # Repeat logic for Others-to-Worst: |w_j - a_jw * w_w| <= ksi
  worst_idx <- match(1, others_to_worst)

  for (j in seq_along(others_to_worst)) {
    if (j != worst_idx) {
      # Equation 1: w_j - a_jw * w_w - ksi <= 0
      lhs1 <- rep(0, n_vars)
      lhs1[j] <- 1
      lhs1[worst_idx] <- -others_to_worst[j]
      lhs1[ksi_idx] <- -1
      constraints <- .combine_constraints(constraints, list(lhs = lhs1, dir = "<=", rhs = 0))$constraints

      # Equation 2: -w_j + a_jw * w_w - ksi <= 0
      lhs2 <- lhs1 * -1
      lhs2[ksi_idx] <- -1
      constraints <- .combine_constraints(constraints, list(lhs = lhs2, dir = "<=", rhs = 0))$constraints
    }
  }

  # 2. Solver Setup
  mat_lhs <- t(sapply(constraints, function(x) x$lhs))
  vec_dir <- sapply(constraints, function(x) x$dir)
  vec_rhs <- unlist(sapply(constraints, function(x) x$rhs))

  # Objective: Minimize ksi
  objective <- rep(0, n_vars)
  objective[ksi_idx] <- 1

  res <- Rglpk::Rglpk_solve_LP(objective, mat_lhs, vec_dir, vec_rhs, max = FALSE)

  # 3. Process Results
  weights <- res$solution[1:(n_vars - 1)]
  ksi_val <- res$solution[n_vars]

  # Consistency Index Table (for 1-9 scale)
  ci_table <- c(0, 0.44, 1.0, 1.63, 2.30, 3.00, 3.73, 4.47, 5.23)
  idx_bw <- as.integer(consistency$a_bw)
  idx_bw <- ifelse(idx_bw > 9, 9, idx_bw) # Safety clip

  cr <- ksi_val / ci_table[idx_bw]
  if (idx_bw == 1) cr <- 0

  list(
    criteriaNames = criteria_names,
    criteriaWeights = weights,
    consistencyRatio = cr,
    ksi = ksi_val
  )
}
