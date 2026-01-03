#' @title Internal Syntax Parser
#' @keywords internal
.parse_mcdm_syntax <- function(syntax) {
  clean_syntax <- gsub("\n", "", syntax)
  lines <- strsplit(clean_syntax, ";")[[1]]
  mapping <- list()
  for (line in lines) {
    if (trimws(line) == "") next
    parts <- strsplit(line, "=~")[[1]]
    if (length(parts) == 2) {
      crit_name <- trimws(parts[1])
      items <- trimws(strsplit(parts[2], "\\+")[[1]])
      mapping[[crit_name]] <- items
    }
  }
  return(mapping)
}

#' @title Internal Saaty Scaler
#' @keywords internal
.scale_to_saaty <- function(vec) {
  if (any(vec < 0, na.rm = TRUE)) stop("Negative values detected.")
  vec[is.na(vec) | vec == 99] <- 0

  valid_mask <- vec > 0
  vals <- vec[valid_mask]
  if (length(vals) == 0) return(vec)

  min_v <- min(vals)
  max_v <- max(vals)

  if (min_v == max_v) {
    vec[valid_mask] <- 1
  } else {
    vec[valid_mask] <- 1 + (vals - min_v) * (8 / (max_v - min_v))
  }
  return(vec)
}

#' @title Internal Fuzzifier
#' @keywords internal
.fuzzify_vec <- function(vec) {
  l <- pmax(1, vec - 1)
  m <- vec
  u <- pmin(9, vec + 1)
  # Handle pure zeros (missing data)
  is_zero <- (vec == 0)
  l[is_zero] <- 0; m[is_zero] <- 0; u[is_zero] <- 0
  return(cbind(l, m, u))
}

#' Prepare Data for Fuzzy MCDM
#'
#' @description Transforms raw survey data into a fuzzy decision matrix.
#' It calculates composite scores based on syntax, scales them to 1-9,
#' aggregates expert responses by Alternative, and then fuzzifies the results.
#'
#' @param data A data frame containing raw items.
#' @param syntax A string defining criteria (e.g., "Cost =~ c1 + c2").
#' @param alternative_col String. The name of the column identifying the Alternatives.
#'        If NULL, rows are treated as distinct alternatives (single expert mode).
#' @param aggr_fun Function. How to aggregate experts (default: mean).
#' @return A matrix ($m x 3n$) where m is the number of Alternatives.
#' @export
prepare_mcdm_data <- function(data, syntax, alternative_col = NULL, aggr_fun = mean) {

  if (!is.data.frame(data)) stop("'data' must be a data frame.")
  mapping <- .parse_mcdm_syntax(syntax)
  criteria_names <- names(mapping)

  # 1. Calculate Composites & Scale (Per Row/Expert)
  # We build a temporary dataframe of scaled scores
  temp_scores <- data.frame(row_id = 1:nrow(data))

  for (crit in criteria_names) {
    items <- mapping[[crit]]
    missing <- items[!items %in% names(data)]
    if (length(missing) > 0) stop(paste("Items missing:", paste(missing, collapse=", ")))

    # Composite (Mean of items)
    if (length(items) > 1) {
      raw_comp <- rowMeans(data[, items, drop = FALSE], na.rm = TRUE)
    } else {
      raw_comp <- data[[items]]
    }

    # Scale to 1-9
    temp_scores[[crit]] <- .scale_to_saaty(raw_comp)
  }

  # 2. Aggregation (Experts -> Alternatives)
  if (!is.null(alternative_col)) {
    if (!alternative_col %in% names(data)) stop("Alternative column not found in data.")

    temp_scores$Alternative_ID <- data[[alternative_col]]

    # Aggregate by Alternative
    agg_data <- aggregate(. ~ Alternative_ID, data = temp_scores[, -1], FUN = aggr_fun)

    # Sort to ensure order
    agg_data <- agg_data[order(agg_data$Alternative_ID), ]
    row_names <- agg_data$Alternative_ID
    score_mat <- as.matrix(agg_data[, criteria_names])

  } else {
    # No aggregation (1 row = 1 alternative)
    score_mat <- as.matrix(temp_scores[, criteria_names])
    row_names <- 1:nrow(score_mat)
  }

  # 3. Fuzzify (Crisp Mean -> Fuzzy Triplet)
  decision_list <- list()
  for (i in seq_along(criteria_names)) {
    crit <- criteria_names[i]
    decision_list[[crit]] <- .fuzzify_vec(score_mat[, i])
  }

  final_mat <- do.call(cbind, decision_list)
  rownames(final_mat) <- row_names
  attr(final_mat, "criteria_names") <- criteria_names

  return(final_mat)
}
