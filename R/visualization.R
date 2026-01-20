#' Plot Comprehensive MCDM Map (Performance vs Risk)
#'
#' @description Creates a cIPMA-inspired visualization.
#' X-Axis: Overall Performance (Group Utility S, Inverted).
#' Y-Axis: Risk/Bottleneck (Individual Regret R).
#' Bubble Size: Robustness (1 - Q). Larger bubbles = Better Compromise.
#'
#' @param vikor_res Result object from fuzzy_vikor
#' @import ggplot2
#' @import ggrepel

# --- Advanced Theme ---
#' @keywords internal
.mcdm_theme_adv <- function() {
  list(
    theme_light(base_size = 12),
    scale_fill_gradient(low = "#90A4AE", high = "#2E7D32"), # Grey-Blue to Green
    scale_size_continuous(range = c(4, 16)),
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40", size = 11),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.position = "right",
      axis.title = element_text(face = "bold")
    )
  )
}

#' Plot VIKOR Strategic Map (Advanced)
#' @description Uses Exponential Scaling for bubbles and Median Quadrants.
#' @export
plot.fuzzy_vikor_res <- function(x, ...) {
  df <- x$results

  # 1. Math: Invert S and Normalize to 0-100
  s_min <- min(df$Def_S); s_max <- max(df$Def_S)
  df$Perf <- ((s_max - df$Def_S) / (s_max - s_min)) * 100

  # 2. Math: Exponential Bubble Size (Emphasis on Leader)
  q_inv <- 1 - ((df$Def_Q - min(df$Def_Q)) / (max(df$Def_Q) - min(df$Def_Q)))
  df$PowerSize <- (q_inv + 0.1)^3

  # 3. Math: Strategic Quadrants (Using Median)
  mid_perf <- median(df$Perf)
  mid_risk <- median(df$Def_R)

  ggplot(df, aes(x = Perf, y = Def_R)) +
    # Strategic Zones Background (Green for Winner)
    annotate("rect", xmin=mid_perf, xmax=Inf, ymin=-Inf, ymax=mid_risk, fill="#E8F5E9", alpha=0.5) +

    # Crosshairs
    geom_vline(xintercept = mid_perf, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = mid_risk, linetype = "dashed", color = "grey50") +

    # Strategic Labels (All 4 Quadrants)
    annotate("text", x = max(df$Perf), y = min(df$Def_R), label = "STABLE LEADER\n(High Perf, Low Risk)",
             hjust=1, vjust=0, size=3, fontface="bold.italic", color="darkgreen") +
    annotate("text", x = max(df$Perf), y = max(df$Def_R), label = "OPPORTUNITY\n(High Perf, High Risk)",
             hjust=1, vjust=1, size=3, fontface="italic", color="#E65100") +
    annotate("text", x = min(df$Perf), y = min(df$Def_R), label = "SAFE MEDIOCRE\n(Low Perf, Low Risk)",
             hjust=0, vjust=0, size=3, fontface="italic", color="grey40") +
    annotate("text", x = min(df$Perf), y = max(df$Def_R), label = "AVOID\n(Low Perf, High Risk)",
             hjust=0, vjust=1, size=3, fontface="italic", color="#B71C1C") +

    # Bubbles
    geom_point(aes(size = PowerSize, fill = Perf), shape = 21, color = "black", alpha = 0.8) +
    geom_text_repel(aes(label = paste0("Alt ", Alternative)), box.padding = 0.5) +

    # Expand axes slightly to fit labels
    scale_x_continuous(expand = expansion(mult = 0.2)) +

    labs(
      title = "VIKOR Strategic Map",
      subtitle = "Green Zone = Best Compromise. Red Zone = Worst Options.",
      x = "Group Performance Index (S inverted)",
      y = "Risk / Regret (R)",
      size = "Dominance",
      fill = "Perf Score"
    ) +
    .mcdm_theme_adv()
}

#' Plot TOPSIS Efficiency Map (Advanced)
#' @description Uses Distance Ratios for quadrants and connection lines.
#' @description NOW INCLUDES: Geometric Distance on dotted lines (Lower is Better).
#' @export
plot.fuzzy_topsis_res <- function(x, ...) {
  df <- x$results

  # Math: Exponential Pop for Bubble Size
  df$PowerSize <- (df$Score)^4

  # 1. Define Target Coordinates (Local Ideal Point for the plot)
  # We use the bounds of the data to place the "Gold Diamond"
  target_x <- max(df$D_minus) * 1.02
  target_y <- min(df$D_plus) * 0.98

  # 2. Calculate Visual Distance (Length of the dotted line)
  # This answers: "How far is this option from the Gold Diamond?"
  df$VisualDist <- sqrt((df$D_minus - target_x)^2 + (df$D_plus - target_y)^2)

  ggplot(df, aes(x = D_minus, y = D_plus)) +

    # Connection Lines to Ideal
    geom_segment(aes(xend = target_x, yend = target_y), linetype = "dotted", color = "grey50") +

    # Label: Geometric Distance (Lower = Closer/Better)
    geom_label(aes(x = (D_minus + target_x) / 2,
                   y = (D_plus + target_y) / 2,
                   label = sprintf("%.3f", VisualDist)),
               size = 3, nudge_y = 0.002, fontface = "italic", color = "grey30", fill = "white", label.size = 0, alpha = 0.8) +

    # Bubbles
    geom_point(aes(size = PowerSize, fill = Score), shape = 21, color = "black", alpha = 0.9) +
    geom_text_repel(aes(label = paste0("Alt ", Alternative)), box.padding = 0.6, force = 5) +

    # Ideal Marker (Gold Diamond)
    annotate("point", x = target_x, y = target_y, shape=18, size=6, color="#FFD700") +
    annotate("text", x = target_x, y = target_y, label="IDEAL", vjust=2, size=3.5, fontface="bold") +

    # Axes Expansion
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.2))) +
    scale_y_continuous(expand = expansion(mult = c(0.2, 0.1))) +

    labs(
      title = "TOPSIS Efficiency Map",
      subtitle = "Dotted lines show gap to Ideal. Labels show Geometric Distance.\nShorter Line (Smaller Number) is Better.",
      x = "Dist. from Worst (D-)",
      y = "Dist. to Best (D+)",
      size = "Closeness^4",
      fill = "Score"
    ) +
    .mcdm_theme_adv()
}

#' Plot WASPAS Consistency Map (Advanced)
#' @export
plot.fuzzy_waspas_res <- function(x, ...) {
  df <- x$results

  df$Deviation <- abs(df$WSM - df$WPM)
  df$Consistency <- 1 - (df$Deviation / max(df$Deviation))

  ggplot(df, aes(x = WSM, y = WPM)) +
    # Consistency Band
    geom_ribbon(aes(ymin = WSM - 0.05, ymax = WSM + 0.05), fill = "grey90", alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +

    geom_point(aes(size = Score^3, fill = Consistency), shape = 21, color = "black", alpha = 0.8) +
    geom_text_repel(aes(label = paste0("Alt ", Alternative)), box.padding = 0.5) +

    scale_x_continuous(expand = expansion(mult = 0.15)) +

    labs(
      title = "WASPAS Consistency Map",
      subtitle = "Points below diagonal are 'Volatile' (High Avg, Low Consistency).\nNotice: Alternatives in grey relie on unbalanced scores.",
      x = "Weighted Sum (Additive)",
      y = "Weighted Product (Multiplicative)",
      size = "Combined",
      fill = "Consistency"
    ) +
    .mcdm_theme_adv()
}

#' Plot MULTIMOORA Strategic Position Map
#'
#' @description Visualizes the trade-off between the Ratio System (Additive Utility)
#' and the Reference Point (Min-Max Regret).
#'
#' @param x An object of class `fuzzy_multimoora_res`.
#' @param ... Additional arguments passed to plot.
#' @export
plot.fuzzy_multimoora_res <- function(x, ...) {
  df <- x$results

  # 1. Calculation for visualization
  n_alts <- nrow(df)
  df$RankStrength <- (n_alts - df$Final_Rank + 1)^2
  mid_rs <- median(df$RS_Score)
  mid_rp <- median(df$RP_Score)

  ggplot(df, aes(x = RS_Score, y = RP_Score)) +
    # Strategic Zone
    annotate("rect", xmin = mid_rs, xmax = Inf, ymin = -Inf, ymax = mid_rp,
             fill = "#E8F5E9", alpha = 0.5) +
    # Guidelines
    geom_vline(xintercept = mid_rs, linetype = "dashed", color = "grey60") +
    geom_hline(yintercept = mid_rp, linetype = "dashed", color = "grey60") +
    # Labels
    annotate("text", x = max(df$RS_Score), y = min(df$RP_Score),
             label = "LEADERS\n(High Utility, Low Regret)",
             hjust = 1, vjust = 0, size = 3, fontface = "bold.italic", color = "darkgreen") +
    # Bubbles
    geom_point(aes(size = RankStrength, fill = as.factor(Final_Rank)),
               shape = 21, color = "black", alpha = 0.8) +
    geom_text_repel(aes(label = paste0("Alt ", Alternative)), box.padding = 0.5) +

    # --- FIX START: Theme first, then Scales ---
    .mcdm_theme_adv() +
    scale_fill_brewer(palette = "RdYlGn", direction = -1, name = "Final\nRank") +
    scale_size(guide = "none") +
    # --- FIX END ---

    labs(
      title = "MULTIMOORA Strategic Map",
      subtitle = "Comparison of Ratio System vs Reference Point.",
      x = "Ratio System (Maximize)",
      y = "Reference Point (Minimize)"
    )
}

#' Plot PROMETHEE II Ranking
#'
#' @description Visualizes the Net Outranking Flows (Phi Net).
#'
#' @param x An object of class `fuzzy_promethee_res`.
#' @param ... Additional arguments.
#' @export
plot.fuzzy_promethee_res <- function(x, ...) {
  df <- x$results
  df <- df[order(df$Phi_Net), ]
  df$Alt_Label <- factor(paste0("Alt ", df$Alternative), levels = paste0("Alt ", df$Alternative))

  ggplot(df, aes(x = Alt_Label, y = Phi_Net)) +
    geom_segment(aes(x = Alt_Label, xend = Alt_Label, y = 0, yend = Phi_Net),
                 color = "grey50", linewidth = 0.8) +
    geom_point(aes(fill = Phi_Net), size = 6, shape = 21, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#D32F2F", alpha = 0.6) +
    geom_text(aes(label = sprintf("%.2f", Phi_Net),
                  vjust = ifelse(Phi_Net > 0, -1.5, 2)),
              size = 3.5, fontface = "bold") +
    coord_flip() +

    # --- FIX START: Theme first, then Scales ---
    .mcdm_theme_adv() +
    scale_fill_gradient2(low = "#B71C1C", mid = "white", high = "#2E7D32", midpoint = 0,
                         name = "Net Flow") +
    # --- FIX END ---

    labs(
      title = "PROMETHEE II Ranking",
      subtitle = "Net Outranking Flow (Phi). Positive = Dominating.",
      x = "",
      y = "Net Flow (Phi)"
    )
}

# Fix for R CMD check global variable warnings
utils::globalVariables(c(
  "Def_S", "Def_R", "D_plus", "D_minus", "Score", "WSM", "WPM",
  "Performance", "Ranking", "Group", "Label", "Alternative",
  "RS_Score", "RP_Score", "FMF_Score", "Final_Rank", "RankStrength",
  "Phi_Net", "Phi_Plus", "Phi_Minus", "Alt_Label"
))
