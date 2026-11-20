# Mean response by treatment arm, three surveys side by side.
# Run from this folder. Reads ../Data/{ih,nk,sudan}.csv (Finished + consent filter).
# Output: plots/<file>.png
# Arms: side A = Israel / Azerbaijan / SAF ; side B = Palestine / Armenia / RSF.
# Format: Witness = civilian witness (relative's post, "_CW"), News = news-outlet personal story ("_Personal"),
#         Stats = statistics ("_Statistics").

suppressMessages({library(tidyverse); library(patchwork)})
dir.create("plots", showWarnings = FALSE)

source("00_prepare.R")   # surveys, arm_levels, raw, dd_all
side_cols <- c(Control = "grey45", A = "#2B6CB0", B = "#DD6B20")
side_labs <- c(Control = "Control", A = "Israel / Azerbaijan / SAF suffering",
               B = "Palestine / Armenia / RSF suffering")

base_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 13), legend.position = "bottom",
        panel.spacing.x = unit(1.5, "lines"), axis.text.x = element_text(size = 8.5, lineheight = 0.9))

summarise_arm <- function(df, var) {
  df %>% filter(!is.na(.data[[var]])) %>%
    group_by(title, arm, side) %>%
    summarise(n = n(), m = mean(.data[[var]]), se = sd(.data[[var]]) / sqrt(n), .groups = "drop") %>%
    mutate(lo = m - 1.96 * se, hi = m + 1.96 * se,
           arm = factor(arm, levels = arm_levels),
           title = factor(title, levels = surveys$title))
}

# ======================================================================================
# Full response distribution: surveys stacked; pooled "All treated" bar on top of each survey;
# Control bar (when the item was asked in Control); non-substantive answers in grey at the right.
dist_data <- function(df, var, levels_lab, control = TRUE) {
  d <- df %>% filter(!is.na(.data[[var]]))
  arms <- d %>% filter(side != "Control")
  pooled <- arms %>% mutate(arm = "All treated", side = "Pooled")
  keep <- if (control) bind_rows(pooled, d) else bind_rows(pooled, arms)
  keep %>%
    count(title, arm, side, resp = .data[[var]]) %>%
    group_by(title, arm, side) %>% mutate(N = sum(n), pct = n / N) %>% ungroup() %>%
    mutate(resp = factor(resp, levels = seq_along(levels_lab), labels = levels_lab),
           arm = factor(arm, levels = rev(c("All treated", arm_levels))),
           title = factor(title, levels = surveys$title),
           # survey prefix keeps identical labels (e.g. two "All treated (n=1203)") on separate rows;
           # the prefix is stripped for display in scale_y_discrete()
           arm_lab = paste0(title, "||", sub("\n", " ", as.character(arm)), "  (n=", N, ")")) %>%
    arrange(title, arm) %>%
    mutate(arm_lab = factor(arm_lab, levels = unique(arm_lab)))   # bottom-to-top order within survey
}

plot_dist <- function(dd, title, subtitle, palette, dark, legend_rows = 1) {
  ggplot(dd, aes(pct, arm_lab, fill = resp)) +
    geom_col(width = 0.75, colour = "white", linewidth = 0.3, position = position_stack(reverse = TRUE)) +
    geom_text(aes(label = ifelse(pct >= 0.06, scales::percent(pct, accuracy = 1), ""),
                  colour = as.integer(resp) %in% dark, group = resp),   # group = resp: stack like the bars
              position = position_stack(vjust = 0.5, reverse = TRUE), size = 3, show.legend = FALSE) +
    facet_grid(title ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_y_discrete(labels = function(x) sub("^.*\\|\\|", "", x)) +
    scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
    scale_fill_manual(values = palette, name = NULL, drop = FALSE) +
    scale_colour_manual(values = c(`FALSE` = "grey15", `TRUE` = "white")) +
    labs(x = NULL, y = NULL, title = title, subtitle = subtitle) +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(), legend.position = "top", legend.justification = "left",
          strip.placement = "outside", strip.text.y.left = element_text(angle = 90, face = "bold", size = 12),
          panel.spacing.y = unit(1, "lines"), axis.text.y = element_text(size = 9.5),
          plot.title.position = "plot", plot.margin = margin(10, 20, 10, 10)) +
    guides(fill = guide_legend(nrow = legend_rows))
}

save_plot <- function(p, file, n_bars = 7) {
  ggsave(file.path("plots", file), p, width = 12, height = 2.2 + 1.1 * n_bars, dpi = 200, bg = "white")
  cat("saved plots/", file, "\n", sep = "")
}

AB <- "Israel / Azerbaijan / SAF"; BA <- "Palestinians / Armenians / RSF"
grey_ns <- c("#C9C9C9", "#9A9A9A")
ord5 <- c("#EDF8B1", "#7FCDBB", "#41B6C4", "#2C7FB8", "#253494")

# ---------------- Q1: post frequency (treated only)
save_plot(plot_dist(dist_data(dd_all, "q1",
    c("Never seen", "Rarely, if ever", "A few times/week", "At least once/day", "A few times/day"), control = FALSE),
  "Q1. How often do you see posts such as this one on social media?",
  "Response distribution by treatment arm; top bar pools all treated arms (not asked in Control)",
  ord5, dark = 4:5), "Q1_post_frequency.png")

# ---------------- Q2: likelihood of sharing (treated only)
save_plot(plot_dist(dist_data(dd_all, "q2",
    c("Not likely at all", "Not very likely", "Somewhat likely", "Very likely"), control = FALSE),
  "Q2. How likely would you be to share this post on your social media account?",
  "Response distribution by treatment arm; top bar pools all treated arms (not asked in Control)",
  ord5[c(1, 3, 4, 5)], dark = 3:4), "Q2_share_likelihood.png")

# ---------------- Q5: sympathies
save_plot(plot_dist(dist_data(dd_all, "q5",
    c(paste("Entirely", AB), paste("Mostly", AB), "Equally with both",
      paste("Mostly", BA), paste("Entirely", BA), "With neither", "Not sure")),
  "Q5. Thinking more carefully about the conflict, would you say your sympathies lie...",
  "Response distribution by arm; blue = Israel / Azerbaijan / SAF side, orange = Palestinian / Armenian / RSF side",
  c("#2B6CB0", "#90CDF4", "#EDEDED", "#FBD38D", "#DD6B20", grey_ns), dark = c(1, 5), legend_rows = 2),
  "Q5_sympathies.png", n_bars = 8)

# ---------------- Q6: are the military actions justified
save_plot(plot_dist(dist_data(dd_all, "q6",
    c("Defending its interests; actions justified", "Has gone too far; actions not justified",
      "Don't know enough")),
  "Q6. Thinking about [Israel's / Azerbaijan's / the SAF's] military action, which statement best describes what you think?",
  "Response distribution by arm",
  c("#2B6CB0", "#DD6B20", grey_ns[1]), dark = 1:2), "Q6_actions_justified.png", n_bars = 8)

# ---------------- Q7: US approach
save_plot(plot_dist(dist_data(dd_all, "q7",
    c("Let it pursue its policy, no U.S. pressure", "Diplomatic pressure, keep aid",
      "Reduce military aid until war ends", "Don't know enough")),
  "Q7. If [Israel / Azerbaijan / the SAF] continues its military action, which approach should the U.S. take?",
  "Response distribution by arm",
  c("#2B6CB0", "#E9D8A6", "#DD6B20", grey_ns[1]), dark = c(1, 3)), "Q7_us_approach.png", n_bars = 8)

# ---------------- Q8: leader attention to this survey's own conflict
save_plot(plot_dist(dist_data(dd_all, "q8",
    c("Too little attention", "Right amount of attention", "Too much attention")),
  "Q8. Are U.S. leaders giving [this conflict] too much, too little, or the right amount of attention?",
  "Item for each survey's own conflict: Israel-Hamas war / Nagorno-Karabakh / Sudan civil war",
  c("#2C7FB8", "#DDE8D5", "#B2182B"), dark = c(1, 3)), "Q8_leader_attention.png", n_bars = 8)
