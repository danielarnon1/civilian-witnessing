# Main experimental analysis: effect of each treatment arm vs Control, separately by survey.
# Model: outcome ~ arm (Control = reference), OLS with HC2 robust SEs, no covariates. Unadjusted p-values.
# Run from this folder. Outputs: treatment_effects.csv, plots/TE_<outcome>.png
# Outcome definitions are in 00_prepare.R.

source("00_prepare.R")
suppressMessages(library(estimatr))
dir.create("plots", showWarnings = FALSE)

outcomes <- tribble(
  ~var,           ~label,                                            ~direction,
  "symp_A",       "Q5 Sympathies (1-5; neither / not sure = 3)", "Positive = more sympathy toward Israel / Azerbaijan / SAF",
  "q6_toofar",    "Q6 A has gone too far (share)", "Positive = more say Israel / Azerbaijan / SAF has gone too far; its actions are not justified",
  "q6_defend",    "Q6 A is defending its interests (share)", "Positive = more say Israel / Azerbaijan / SAF is defending its interests; its actions are justified",
  "dk_justified", "Q6 Don't know (share)",                           "Positive = more 'don't know enough'",
  "q7_reduce",    "Q7 U.S. should reduce military aid to A (share)", "Positive = more choose: reduce military aid to Israel / Azerbaijan / SAF until it ends the war",
  "q7_pressure",  "Q7 U.S. should apply diplomatic pressure, keep aid (share)", "Positive = more choose: diplomatic pressure on Israel / Azerbaijan / SAF but keep aid",
  "q7_nopress",   "Q7 U.S. should let A act without pressure (share)", "Positive = more choose: let Israel / Azerbaijan / SAF pursue its policy without U.S. pressure",
  "dk_us",        "Q7 Don't know (share)",                           "Positive = more 'don't know enough'",
  "attn_little",  "Q8 Too little attention (share)",                 "Positive = more say U.S. leaders give this conflict too little attention",
  "ceasefire",    "Q10 Importance of ceasefire / peace (1-5)",       "Positive = more important",
  "humanit",      "Q11 Importance of humanitarian relief (1-5)",     "Positive = more important",
  "milaid_A",     "Q12 Importance of military aid to Israel / Azerbaijan / SAF (1-5)", "Positive = more important"
)

# ---------------------------------------------------------------- estimate
estimate_one <- function(ttl, v) {
  d <- dd_all %>% filter(title == ttl, !is.na(.data[[v]])) %>%
    mutate(arm = relevel(factor(sub("\n", " ", arm)), ref = "Control"))
  m <- lm_robust(reformulate("arm", v), data = d, se_type = "HC2")
  ci <- confint(m)
  tibble(title = ttl, var = v, term = names(coef(m)), est = coef(m), se = m$std.error,
         lo = ci[, 1], hi = ci[, 2], p = m$p.value) %>%
    filter(term != "(Intercept)") %>%
    mutate(arm = sub("^arm", "", term),
           n_arm = as.integer(table(d$arm)[arm]),
           ctrl_mean = mean(d[[v]][d$arm == "Control"]),
           n_ctrl = sum(d$arm == "Control"), n_total = nrow(d))
}

te <- expand_grid(title = surveys$title, var = outcomes$var) %>%
  pmap_dfr(function(title, var) estimate_one(title, var)) %>%
  left_join(dd_all %>% distinct(arm = sub("\n", " ", arm), side, fmt), by = "arm") %>%
  mutate(stars = case_when(p < .01 ~ "**", p < .05 ~ "*", p < .10 ~ "+", TRUE ~ ""))

write.csv(te %>% select(title, var, arm, side, fmt, est, se, lo, hi, p, n_arm, ctrl_mean, n_ctrl, n_total),
          "treatment_effects.csv", row.names = FALSE)
cat("saved treatment_effects.csv\n")

# ---------------------------------------------------------------- plot
side_cols <- c(A = "#2B6CB0", B = "#DD6B20")
side_labs <- c(A = "Israel / Azerbaijan / SAF suffering", B = "Palestine / Armenia / RSF suffering")
arm_order <- setdiff(sub("\n", " ", arm_levels), "Control")

plot_te <- function(v, file) {
  o <- outcomes %>% filter(var == v)
  d <- te %>% filter(var == v) %>%
    # n in model = respondents with a valid answer on this outcome (all arms), out of the survey's full N
    mutate(N_survey = as.integer(table(dd_all$title)[title]),
           facet = sprintf("%s   (Control mean = %.2f; n in model = %s of %s; Control n = %s)", title, ctrl_mean,
                           scales::comma(n_total), scales::comma(N_survey), scales::comma(n_ctrl)),
           facet = factor(facet, levels = unique(facet[order(match(title, surveys$title))])),
           key = factor(paste0(title, "||", arm),
                        levels = rev(paste0(rep(surveys$title, each = length(arm_order)), "||", arm_order))),
           lab = sprintf("%+.2f%s", est, stars),
           # label outside the CI, on the side away from zero
           lab_x = ifelse(est >= 0, hi, lo), lab_h = ifelse(est >= 0, -0.2, 1.2))
  rng <- max(abs(c(d$lo, d$hi)))
  ggplot(d, aes(est, key, colour = side)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
    geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.45, linewidth = 0.8) +
    geom_text(aes(x = lab_x, label = lab, hjust = lab_h), size = 3.3, show.legend = FALSE) +
    facet_wrap(~facet, ncol = 1, scales = "free_y") +
    scale_y_discrete(labels = function(x) sub("^.*\\|\\|", "", x)) +
    scale_x_continuous(limits = c(-rng, rng) * 1.18, breaks = scales::pretty_breaks(7)) +
    scale_colour_manual(values = side_cols, labels = side_labs, name = NULL) +
    labs(x = "Difference from Control (95% CI, HC2 robust SEs)", y = NULL,
         title = paste0("Treatment effects: ", o$label),
         subtitle = paste0(o$direction, ".  ** p<.01  * p<.05  + p<.10 (unadjusted)")) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          strip.text = element_text(face = "bold", hjust = 0, size = 11.5),
          legend.position = "bottom", plot.title.position = "plot",
          panel.spacing.y = unit(1.2, "lines"))
  ggsave(file.path("plots", file), width = 10, height = 9, dpi = 200, bg = "white")
  cat("saved plots/", file, "\n", sep = "")
}

plot_te("symp_A",       "TE_Q5_sympathies.png")
plot_te("q6_toofar",    "TE_Q6_gone_too_far.png")
plot_te("q6_defend",    "TE_Q6_defending_interests.png")
plot_te("dk_justified", "TE_Q6_dont_know.png")
plot_te("q7_reduce",    "TE_Q7_reduce_aid.png")
plot_te("q7_pressure",  "TE_Q7_diplomatic_pressure.png")
plot_te("q7_nopress",   "TE_Q7_no_pressure.png")
plot_te("dk_us",        "TE_Q7_dont_know.png")
plot_te("attn_little",  "TE_Q8_too_little_attention.png")
plot_te("ceasefire",    "TE_Q10_ceasefire.png")
plot_te("humanit",      "TE_Q11_humanitarian.png")
plot_te("milaid_A",     "TE_Q12_military_aid.png")
