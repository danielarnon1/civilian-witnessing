# Survey-order check: many respondents took BOTH the Nagorno-Karabakh and Sudan surveys (matched on participantId).
# Did taking the other survey first change answers or treatment effects?
# Order from timestamps (m/d/yy hh:mm): the survey that started first came first; same-minute starts are resolved by
# whether one survey ended before (or the minute) the other started; still-overlapping pairs are "ambiguous" and
# dropped from the order comparisons. (The Israel-Hamas file has no participantId, so overlap with it cannot be checked.)
# Groups within each survey: "First" (took the other survey later), "Second" (took the other one earlier), "Only".
# Tests (OLS, HC2), per survey and outcome:
#   (a) Levels:   outcome ~ arm + order          -> joint Wald test of the order terms
#   (b) Effects:  outcome ~ side * order         -> joint Wald test of side x order (do treatment effects differ?)
#   (c) Balance:  chi-square of arm x order      (assignment should be independent of order)
# BH q-values across the 9 outcomes within each survey x test. Outputs: survey_order_tests.csv, plots/ORDER_*.png

source("00_prepare.R")
suppressMessages({ library(estimatr); library(lubridate) })

OUTS <- c(symp_A = "Q5 Sympathy toward A (1-5, middle = 3)", justified = "Q6 A's actions justified (1-3)",
          dk_justified = "Q6 Don't know (share)", us_lenient = "Q7 Leave A unpressured (1-3, DK = 2)",
          dk_us = "Q7 Don't know (share)", attn_little = "Q8 Too little attention (share)",
          ceasefire = "Q10 Ceasefire importance (1-5)", humanit = "Q11 Humanitarian relief to B (1-5)",
          milaid_A = "Q12 Military aid to A (1-5)")

d <- dd_all %>% filter(title != "Israel-Hamas") %>%
  mutate(t_start = mdy_hm(StartDate), t_end = mdy_hm(EndDate))

# ---- pair NK and Sudan records by participantId (one duplicated NK id: keep its earliest record)
nk <- d %>% filter(survey == "nk")    %>% group_by(participantId) %>% slice_min(t_start, n = 1, with_ties = FALSE) %>% ungroup()
sd <- d %>% filter(survey == "sudan") %>% group_by(participantId) %>% slice_min(t_start, n = 1, with_ties = FALSE) %>% ungroup()
pairs <- inner_join(nk %>% select(participantId, s_nk = t_start, e_nk = t_end),
                    sd %>% select(participantId, s_sd = t_start, e_sd = t_end), by = "participantId") %>%
  mutate(order = case_when(s_nk < s_sd & e_nk <= s_sd ~ "nk_first",
                           s_sd < s_nk & e_sd <= s_nk ~ "sudan_first",
                           s_nk < s_sd ~ "nk_first",          # started first but windows touch within a minute
                           s_sd < s_nk ~ "sudan_first",
                           TRUE ~ "ambiguous"))
cat("Respondents in both surveys:", nrow(pairs), "\n"); print(table(pairs$order))
cat("Days between the two surveys (median):",
    median(abs(as.numeric(difftime(pairs$s_sd, pairs$s_nk, units = "days")))), "\n")

d <- d %>% left_join(pairs %>% select(participantId, pair_order = order), by = "participantId") %>%
  mutate(order = case_when(is.na(pair_order) ~ "Only",
                           pair_order == "ambiguous" ~ NA_character_,
                           survey == "nk" & pair_order == "nk_first" ~ "First",
                           survey == "sudan" & pair_order == "sudan_first" ~ "First",
                           TRUE ~ "Second"),
         order_f = factor(order, c("Only", "First", "Second")),
         arm_f = relevel(factor(sub("\n", " ", arm)), "Control"),
         side_f = relevel(factor(side), "Control"))
print(d %>% count(title, order))

wald <- function(m, pat) {
  k <- grep(pat, names(coef(m))); k <- k[!is.na(coef(m)[k])]
  b <- coef(m)[k]; V <- vcov(m)[k, k, drop = FALSE]
  s <- as.numeric(t(b) %*% MASS::ginv(V) %*% b); r <- qr(V)$rank
  c(stat = s, df = r, p = pchisq(s, r, lower.tail = FALSE))
}

# ---- (c) balance of random assignment across order groups
bal <- d %>% filter(!is.na(order_f)) %>% group_by(title) %>%
  summarise(balance_p = suppressWarnings(chisq.test(table(droplevels(arm_f), order_f))$p.value), .groups = "drop")
cat("\nAssignment balance across order groups (chi-square p):\n"); print(bal)

# ---- (a) levels and (b) effect heterogeneity
tests <- map_dfr(c("Nagorno-Karabakh", "Sudan"), function(ttl) map_dfr(names(OUTS), function(v) {
  x <- d %>% filter(title == ttl, !is.na(order_f), !is.na(.data[[v]]))
  ma <- lm_robust(reformulate(c("arm_f", "order_f"), v), data = x, se_type = "HC2")
  mb <- lm_robust(reformulate("side_f * order_f", v), data = x, se_type = "HC2")
  wa <- wald(ma, "^order_f"); wb <- wald(mb, "^side_f.*:order_f")
  ci <- confint(ma)
  tibble(survey = ttl, var = v, n = nrow(x),
         first_vs_only = coef(ma)[["order_fFirst"]], first_p = ma$p.value[["order_fFirst"]],
         second_vs_only = coef(ma)[["order_fSecond"]], second_p = ma$p.value[["order_fSecond"]],
         levels_p = wa[["p"]], effects_p = wb[["p"]], effects_df = wb[["df"]])
})) %>% group_by(survey) %>%
  mutate(levels_q = p.adjust(levels_p, "BH"), effects_q = p.adjust(effects_p, "BH")) %>% ungroup() %>%
  left_join(bal %>% rename(survey = title), by = "survey")
write.csv(tests, "survey_order_tests.csv", row.names = FALSE)
cat("\nsaved survey_order_tests.csv\n")
tests %>% transmute(survey, outcome = var,
                    `First vs Only` = sprintf("%+.3f (p=%.3f)", first_vs_only, first_p),
                    `Second vs Only` = sprintf("%+.3f (p=%.3f)", second_vs_only, second_p),
                    `levels q` = round(levels_q, 3), `effects differ p` = round(effects_p, 3), `effects q` = round(effects_q, 3)) %>%
  as.data.frame() %>% print(row.names = FALSE)

# ---- figure: pooled-side treatment effects within each order group
eff <- map_dfr(c("Nagorno-Karabakh", "Sudan"), function(ttl) map_dfr(names(OUTS), function(v) map_dfr(c("Only", "First", "Second"), function(o) {
  x <- d %>% filter(title == ttl, order == o, !is.na(.data[[v]]))
  m <- lm_robust(reformulate("side_f", v), data = x, se_type = "HC2"); ci <- confint(m)
  k <- grep("^side_f", names(coef(m)))
  tibble(survey = ttl, var = v, order = o, side = sub("side_f", "", names(coef(m))[k]),
         est = coef(m)[k], lo = ci[k, 1], hi = ci[k, 2], p = m$p.value[k], n = nrow(x))
})))
het_lab <- tests %>% transmute(survey, var, note = sprintf("effects differ by order: p = %.2f", effects_p))
pd <- eff %>% left_join(het_lab, by = c("survey", "var")) %>%
  mutate(outcome = factor(OUTS[var], OUTS),
         order = factor(order, c("Second", "First", "Only"),
                        c("Took this survey 2nd", "Took this survey 1st", "Took only this survey")))
g <- ggplot(pd, aes(est, order, colour = side)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.35) +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.55), size = 0.3, linewidth = 0.6) +
  geom_text(data = pd %>% distinct(survey, outcome, note), aes(x = -Inf, y = 3.45, label = note), inherit.aes = FALSE,
            hjust = -0.03, size = 2.6, colour = "grey35", fontface = "italic") +
  facet_grid(outcome ~ survey, scales = "free_x", switch = "y", labeller = labeller(outcome = label_wrap_gen(22))) +
  scale_colour_manual(values = c(A = "#2B6CB0", B = "#DD6B20"), name = NULL,
                      labels = c(A = "Posts about Azerbaijan / SAF", B = "Posts about Armenia / RSF")) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.1))) +
  labs(x = "Treatment effect vs. control within each group (95% CI)", y = NULL,
       title = "Did taking the other survey first change the treatment effects?",
       subtitle = "Pooled posts about each side vs. control, estimated separately for respondents who took this survey first, second, or only this one.\nThe p-value tests whether effects differ across the three groups (joint side x order test).") +
  theme_minimal(base_size = 10.5) +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
        strip.text.x = element_text(face = "bold", size = 12), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(), legend.position = "bottom", plot.title.position = "plot",
        panel.spacing.y = unit(0.6, "lines"))
ggsave("plots/ORDER_treatment_effects.png", g, width = 11, height = 15, dpi = 200, bg = "white")
cat("saved plots/ORDER_treatment_effects.png\n")
