# Cross-survey pooling: stack all three surveys and estimate each treatment's effect with survey fixed effects.
# Side A = Israel / Azerbaijan / SAF (the actor whose conduct Q6/Q7 ask about); side B = Palestinians / Armenians / RSF.
# Models (OLS; SEs clustered by participantId (CR1, Stata-style) because 855 people took both NK and Sudan; survey fixed effects):
#   (1) pooled by side:  outcome ~ side (A, B vs Control) + survey
#   (2) by post type:    outcome ~ post (A Witness ... B Stats vs Control) + survey
#   Heterogeneity across surveys: outcome ~ side * survey; joint Wald test that the side effects are equal in all
#   three surveys (side x survey terms = 0).
# Two weightings: respondent-weighted (IH has ~2.4x the respondents of each other survey, so it dominates), and
# survey-equal (each survey's respondents weighted so every survey counts equally).
# est_sd = effect in control-group SDs (outcome standardized within survey), for comparing across conflicts.
# Not part of the dashboard. Outputs: cross_survey_effects.csv, plots/CROSS_*.png

source("00_prepare.R")
suppressMessages(library(estimatr))

OUT_LAB <- c(symp_A = "Q5 Sympathy toward A (1-5, middle = 3)", justified = "Q6 A's actions justified (1-3)",
             dk_justified = "Q6 Don't know (share)", q7_reduce = "Q7 Reduce military aid to A (share)", q7_pressure = "Q7 Diplomatic pressure, keep aid (share)",
             q7_nopress = "Q7 Let A act, no U.S. pressure (share)",
             dk_us = "Q7 Don't know (share)", attn_little = "Q8 Too little attention (share)",
             ceasefire = "Q10 Ceasefire importance (1-5)", humanit = "Q11 Humanitarian relief to B (1-5)",
             milaid_A = "Q12 Military aid to A (1-5)")

d <- dd_all %>%
  mutate(survey_f = factor(title, surveys$title),
         side_f = relevel(factor(side), "Control"),
         post = ifelse(side == "Control", "Control", paste(side, fmt)),
         post_f = relevel(factor(post), "Control"),
         # 855 people answered both the NK and Sudan surveys: cluster SEs by person (IH has no ID, so each row is its own cluster)
         cluster_id = ifelse(!is.na(participantId) & participantId != "", participantId, paste0("row", row_number()))) %>%
  group_by(title) %>% mutate(w_equal = (nrow(dd_all) / 3) / n()) %>% ungroup()

# standardize each outcome within survey by that survey's control-group SD (common scale across conflicts)
std_within <- function(x, v) x %>% group_by(title) %>%
  mutate(z = (.data[[v]] - mean(.data[[v]][side == "Control"], na.rm = TRUE)) / sd(.data[[v]][side == "Control"], na.rm = TRUE)) %>%
  ungroup()

wald <- function(m, pat) {
  k <- grep(pat, names(coef(m))); k <- k[!is.na(coef(m)[k])]
  b <- coef(m)[k]; V <- vcov(m)[k, k, drop = FALSE]
  s <- as.numeric(t(b) %*% solve(V, b)); c(stat = s, df = length(k), p = pchisq(s, length(k), lower.tail = FALSE))
}

res <- map_dfr(names(OUT_LAB), function(v) {
  x <- d %>% filter(!is.na(.data[[v]])) %>% std_within(v)
  map_dfr(c(respondent = "respondent", equal = "equal"), function(wt) {
    w <- if (wt == "equal") x$w_equal else NULL
    fit <- function(y, rhs) lm_robust(reformulate(rhs, y), data = x, weights = w, clusters = cluster_id, se_type = "stata")
    m1 <- fit(v, c("side_f", "survey_f")); m2 <- fit(v, c("post_f", "survey_f"))
    z1 <- fit("z", c("side_f", "survey_f")); z2 <- fit("z", c("post_f", "survey_f"))
    mh <- fit(v, "side_f * survey_f")
    het <- wald(mh, "^side_f.*:survey_f")
    tidy_m <- function(m, mz, pre, model) {
      ci <- confint(m); k <- grep(paste0("^", pre), names(coef(m)))
      ciz <- confint(mz)
      tibble(model = model, term = sub(paste0("^", pre), "", names(coef(m))[k]), est = coef(m)[k],
             lo = ci[k, 1], hi = ci[k, 2], p = m$p.value[k],
             est_sd = coef(mz)[k], lo_sd = ciz[k, 1], hi_sd = ciz[k, 2])
    }
    bind_rows(tidy_m(m1, z1, "side_f", "Pooled by side"), tidy_m(m2, z2, "post_f", "By post type")) %>%
      mutate(var = v, weighting = wt, n = nrow(x), het_p = het[["p"]], het_df = het[["df"]])
  })
}) %>%
  # BH across the 9 outcomes within each model x weighting family (as a robustness reference)
  group_by(model, weighting, term) %>% mutate(q_bh_outcomes = p.adjust(p, "BH")) %>% ungroup() %>%
  mutate(stars = case_when(p < .01 ~ "**", p < .05 ~ "*", p < .10 ~ "+", TRUE ~ ""))

write.csv(res, "cross_survey_effects.csv", row.names = FALSE)
cat("saved cross_survey_effects.csv\n\n")

res %>% filter(model == "Pooled by side") %>%
  mutate(cell = sprintf("%+.3f%s (p=%.3f)", est, stars, p)) %>%
  select(var, weighting, term, cell, het_p) %>%
  pivot_wider(names_from = c(weighting, term), values_from = cell) %>%
  mutate(het_p = signif(het_p, 2)) %>% as.data.frame() %>% print(row.names = FALSE)

# ---------------------------------------------------------------- figure
side_cols <- c(A = "#2B6CB0", B = "#DD6B20")
plot_cross <- function(mod, file, h) {
  pd <- res %>% filter(model == mod, weighting == "respondent") %>%
    mutate(side = substr(term, 1, 1),
           fmt = factor(sub("^[AB] ", "", term), c("Witness", "News", "Stats")),
           # outcome label carries the cross-survey heterogeneity note on a second line
           out_lab = paste0(OUT_LAB[var], ifelse(het_p < .05, sprintf("\n(differs across surveys, p = %s)", format.pval(het_p, digits = 2, eps = .001)), "")),
           outcome = factor(out_lab, rev(unique(out_lab[order(match(var, names(OUT_LAB)))]))),
           lab = sprintf("%+.2f%s", est, stars))
  rng <- max(abs(c(pd$lo, pd$hi)))
  dodge <- position_dodge(width = if (mod == "Pooled by side") 0.55 else 0.8)
  g <- ggplot(pd, aes(est, outcome, colour = side, group = term)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
    (if (mod == "Pooled by side")
       geom_pointrange(aes(xmin = lo, xmax = hi), position = dodge, size = 0.45, linewidth = 0.8)
     else geom_pointrange(aes(xmin = lo, xmax = hi, shape = fmt), position = dodge, size = 0.4, linewidth = 0.7)) +
    scale_colour_manual(values = side_cols, labels = c(A = "Israel / Azerbaijan / SAF posts", B = "Palestine / Armenia / RSF posts"),
                        name = NULL) +
    scale_shape_manual(values = c(Witness = 16, News = 17, Stats = 15), name = NULL) +
    scale_x_continuous(limits = c(-rng, rng) * 1.35) +
    labs(x = "Difference from Control, all three surveys pooled (95% CI; survey fixed effects; SEs clustered by participant)", y = NULL,
         title = paste0("Cross-survey pooled treatment effects: ", tolower(mod)),
         subtitle = paste0("Respondents from all three surveys stacked (N = ", scales::comma(nrow(d)),
                           "). Blue = posts about Israel / Azerbaijan / SAF suffering; orange = Palestine / Armenia / RSF.\n",
                           "Shares are on a 0-1 scale. Outcomes are on different scales; compare signs and significance across rows.")) +
    theme_minimal(base_size = 11.5) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(colour = "grey93"),
          legend.position = "bottom", plot.title.position = "plot", axis.text.y = element_text(lineheight = 0.9))
  if (mod == "Pooled by side") {
    g <- g + geom_text(aes(x = ifelse(est >= 0, hi, lo), label = lab, hjust = ifelse(est >= 0, -0.15, 1.15)),
                       position = dodge, size = 3.2, show.legend = FALSE)
  }
  ggsave(file.path("plots", file), g, width = 11, height = h, dpi = 200, bg = "white")
  cat("saved plots/", file, "\n", sep = "")
}
plot_cross("Pooled by side", "CROSS_pooled_by_side.png", 8)
plot_cross("By post type", "CROSS_by_post_type.png", 10)
