# Pooled treatment effects by direction: all three formats about side A pooled, all three about side B pooled,
# each vs Control, separately by survey.
# Model: outcome ~ side (Control = reference), OLS with HC2 robust SEs, no covariates. Unadjusted p-values.
# Run from this folder. Outputs: pooled_effects.csv, plots/POOLED_<outcome>.png
# Outcome definitions are in 00_prepare.R; same outcome list as 04_treatment_effects.R.

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

pooled_label <- function(ttl, side) {
  s <- surveys[surveys$title == ttl, ]
  paste0(ifelse(side == "A", s$A_lab, s$B_lab), " (all 3 posts)")
}

# ---------------------------------------------------------------- estimate
estimate_one <- function(ttl, v) {
  d <- dd_all %>% filter(title == ttl, !is.na(.data[[v]])) %>%
    mutate(side = relevel(factor(side), ref = "Control"))
  m <- lm_robust(reformulate("side", v), data = d, se_type = "HC2")
  ci <- confint(m)
  tibble(title = ttl, var = v, term = names(coef(m)), est = coef(m), se = m$std.error,
         lo = ci[, 1], hi = ci[, 2], p = m$p.value) %>%
    filter(term != "(Intercept)") %>%
    mutate(side = sub("^side", "", term),
           arm = map_chr(side, ~ pooled_label(ttl, .x)),
           n_arm = as.integer(table(d$side)[side]),
           ctrl_mean = mean(d[[v]][d$side == "Control"]),
           n_ctrl = sum(d$side == "Control"), n_total = nrow(d))
}

pe <- expand_grid(title = surveys$title, var = outcomes$var) %>%
  pmap_dfr(function(title, var) estimate_one(title, var)) %>%
  mutate(stars = case_when(p < .01 ~ "**", p < .05 ~ "*", p < .10 ~ "+", TRUE ~ ""))

write.csv(pe %>% select(title, var, arm, side, est, se, lo, hi, p, n_arm, ctrl_mean, n_ctrl, n_total),
          "pooled_effects.csv", row.names = FALSE)
cat("saved pooled_effects.csv\n")

# ---------------------------------------------------------------- plot
side_cols <- c(A = "#2B6CB0", B = "#DD6B20")
side_labs <- c(A = "Israel / Azerbaijan / SAF suffering (pooled)", B = "Palestine / Armenia / RSF suffering (pooled)")

plot_pooled <- function(v, file) {
  o <- outcomes %>% filter(var == v)
  d <- pe %>% filter(var == v) %>%
    # n in model = respondents with a valid answer on this outcome (all arms), out of the survey's full N
    mutate(N_survey = as.integer(table(dd_all$title)[title]),
           facet = sprintf("%s   (Control mean = %.2f; n in model = %s of %s; Control n = %s)", title, ctrl_mean,
                           scales::comma(n_total), scales::comma(N_survey), scales::comma(n_ctrl)),
           facet = factor(facet, levels = unique(facet[order(match(title, surveys$title))])),
           key = factor(paste0(title, "||", arm),
                        levels = rev(paste0(rep(surveys$title, each = 2), "||",
                                            c(rbind(map2_chr(surveys$title, "A", pooled_label),
                                                    map2_chr(surveys$title, "B", pooled_label)))))),
           lab = sprintf("%+.2f%s", est, stars),
           lab_x = ifelse(est >= 0, hi, lo), lab_h = ifelse(est >= 0, -0.2, 1.2))
  rng <- max(abs(c(d$lo, d$hi)))
  ggplot(d, aes(est, key, colour = side)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
    geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.55, linewidth = 0.9) +
    geom_text(aes(x = lab_x, label = lab, hjust = lab_h), size = 3.5, show.legend = FALSE) +
    facet_wrap(~facet, ncol = 1, scales = "free_y") +
    scale_y_discrete(labels = function(x) sub("^.*\\|\\|", "", x)) +
    scale_x_continuous(limits = c(-rng, rng) * 1.2, breaks = scales::pretty_breaks(7)) +
    scale_colour_manual(values = side_cols, labels = side_labs, name = NULL) +
    labs(x = "Difference from Control (95% CI, HC2 robust SEs)", y = NULL,
         title = paste0("Pooled treatment effects: ", o$label),
         subtitle = paste0(o$direction, ".  ** p<.01  * p<.05  + p<.10 (unadjusted)")) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          strip.text = element_text(face = "bold", hjust = 0, size = 11.5),
          legend.position = "bottom", plot.title.position = "plot",
          panel.spacing.y = unit(1.2, "lines"))
  ggsave(file.path("plots", file), width = 10, height = 5.5, dpi = 200, bg = "white")
  cat("saved plots/", file, "\n", sep = "")
}

plot_pooled("symp_A",       "POOLED_Q5_sympathies.png")
plot_pooled("q6_toofar",    "POOLED_Q6_gone_too_far.png")
plot_pooled("q6_defend",    "POOLED_Q6_defending_interests.png")
plot_pooled("dk_justified", "POOLED_Q6_dont_know.png")
plot_pooled("q7_reduce",    "POOLED_Q7_reduce_aid.png")
plot_pooled("q7_pressure",  "POOLED_Q7_diplomatic_pressure.png")
plot_pooled("q7_nopress",   "POOLED_Q7_no_pressure.png")
plot_pooled("dk_us",        "POOLED_Q7_dont_know.png")
plot_pooled("attn_little",  "POOLED_Q8_too_little_attention.png")
plot_pooled("ceasefire",    "POOLED_Q10_ceasefire.png")
plot_pooled("humanit",      "POOLED_Q11_humanitarian.png")
plot_pooled("milaid_A",     "POOLED_Q12_military_aid.png")
