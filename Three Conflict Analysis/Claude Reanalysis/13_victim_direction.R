# Hypothesis tests in the paper's terms: movement "in the victim's direction".
#   H1  personal narrative (witness, news story) moves attitudes toward the victim's side
#   H2  statistics move attitudes toward the victim's side
#   H3a personal narrative > statistics          H3b witness > news story
#
# Victim-direction effect of a format f: VD_f = (b_A,f - b_B,f) / 2, where b are effects vs control from
# outcome ~ arm (outcomes coded higher = more favorable to side A). Positive VD = shift toward the side shown.
# Contrasts are linear combinations of the arm coefficients (robust vcov).
# Outcomes: Q5 sympathy, Q6 actions justified, Q7 U.S. approach (all midpoint coding) and their INDEX
#   = mean of the three, each standardized by the survey's control-group SD (Anderson-style simple average).
# One-sided items (only one side's posts can move them toward the victim): Q12 military aid to A after A posts,
#   Q11 humanitarian relief to B after B posts.
# Specifications: (1) unadjusted; (2) PRIMARY: Lin (2013) adjustment for pre-treatment own-conflict sympathy
#   (prior_A: +1 A, -1 B, 0 both / neither / don't know; the 15 respondents missing it are dropped); (3) NK & Sudan: first-time takers only (drops people who
#   had already taken the other survey). Pooled across surveys: standardized outcomes, generic arms, survey fixed
#   effects (and prior x survey in the adjusted model), SEs clustered by participantId.
# BH q-values across the 4 outcomes within survey x specification x estimand.
# Outputs: victim_direction.csv, plots/VICTIM_direction.png

source("00_prepare.R")
suppressMessages({ library(estimatr); library(lubridate) })

# ---- first-time takers flag (NK / Sudan): drop respondents who took the other survey earlier
dd <- dd_all %>% mutate(t_start = suppressWarnings(mdy_hm(StartDate)))
both <- inner_join(dd %>% filter(survey == "nk") %>% group_by(participantId) %>% summarise(s_nk = min(t_start)),
                   dd %>% filter(survey == "sudan") %>% group_by(participantId) %>% summarise(s_sd = min(t_start)),
                   by = "participantId")
second_nk <- both$participantId[both$s_sd < both$s_nk]
second_sd <- both$participantId[both$s_nk < both$s_sd]

d <- dd %>%
  mutate(took_other_first = (survey == "nk" & participantId %in% second_nk) | (survey == "sudan" & participantId %in% second_sd),
         post = ifelse(side == "Control", "Control", paste(side, fmt)),
         post_f = relevel(factor(post), "Control"),
         prior = replace_na(prior_A, 0), prior_miss = as.numeric(is.na(prior_A)),
         cluster_id = ifelse(!is.na(participantId) & participantId != "", participantId, paste0("row", row_number()))) %>%
  group_by(title) %>%
  mutate(across(c(symp_A, justified, us_lenient),
                ~ (.x - mean(.x[side == "Control"], na.rm = TRUE)) / sd(.x[side == "Control"], na.rm = TRUE), .names = "z_{.col}")) %>%
  mutate(index = rowMeans(cbind(z_symp_A, z_justified, z_us_lenient), na.rm = TRUE)) %>%
  ungroup()

OUTS <- c(index = "Index (Q5-Q7, control SDs)", z_symp_A = "Q5 Sympathy (control SDs)",
          z_justified = "Q6 Actions justified (control SDs)", z_us_lenient = "Q7 U.S. approach (control SDs)")
arms <- c("A Witness", "A News", "A Stats", "B Witness", "B News", "B Stats")

# contrast weights over the six arm coefficients (order = arms)
W <- list(
  "Witness"                     = c(.5, 0, 0, -.5, 0, 0),
  "News story"                  = c(0, .5, 0, 0, -.5, 0),
  "Statistics"                  = c(0, 0, .5, 0, 0, -.5),
  "All posts"                   = c(1, 1, 1, -1, -1, -1) / 6,
  "H3a: personal - statistics"  = c(.25, .25, -.5, -.25, -.25, .5),
  "H3b: witness - news story"   = c(.5, -.5, 0, -.5, .5, 0))

lincom <- function(m, prefix) {
  nm <- paste0(prefix, arms); b <- coef(m)[nm]; V <- vcov(m)[nm, nm]
  df <- if (!is.null(m$df.residual)) m$df.residual else Inf
  map_dfr(names(W), function(k) {
    w <- W[[k]]; est <- sum(w * b); se <- sqrt(as.numeric(t(w) %*% V %*% w))
    tibble(estimand = k, est = est, se = se, lo = est - 1.96 * se, hi = est + 1.96 * se, p = 2 * pnorm(-abs(est / se)))
  })
}

fit_one <- function(x, y, spec, pooled = FALSE) {
  x <- x %>% filter(!is.na(.data[[y]]))
  if (!pooled) {
    m <- if (spec == "Unadjusted") lm_robust(reformulate("post_f", y), data = x, se_type = "HC2")
         else lm_lin(reformulate("post_f", y), covariates = ~ prior, data = x %>% filter(prior_miss == 0), se_type = "HC2")
  } else {
    rhs <- if (spec == "Unadjusted") c("post_f", "title") else c("post_f", "title * prior")
    m <- lm_robust(reformulate(rhs, y), data = x, clusters = cluster_id, se_type = "stata")
  }
  lincom(m, "post_f") %>% mutate(n = nrow(x))
}

specs <- c("Unadjusted", "Adjusted for prior sympathy (primary)")
res <- bind_rows(
  # per survey
  expand_grid(ttl = surveys$title, y = names(OUTS), spec = specs) %>%
    pmap_dfr(function(ttl, y, spec) fit_one(d %>% filter(title == ttl), y, spec) %>% mutate(survey = ttl, outcome = y, spec = spec)),
  # NK / Sudan first-time takers, primary spec
  expand_grid(ttl = c("Nagorno-Karabakh", "Sudan"), y = names(OUTS)) %>%
    pmap_dfr(function(ttl, y) fit_one(d %>% filter(title == ttl, !took_other_first), y, specs[2]) %>%
               mutate(survey = ttl, outcome = y, spec = "Primary, first-time takers only")),
  # pooled across the three surveys
  expand_grid(y = names(OUTS), spec = specs) %>%
    pmap_dfr(function(y, spec) fit_one(d, y, spec, pooled = TRUE) %>% mutate(survey = "Pooled (3 surveys)", outcome = y, spec = spec))
) %>%
  group_by(survey, spec, estimand) %>% mutate(q_bh = p.adjust(p, "BH")) %>% ungroup() %>%
  mutate(stars = case_when(p < .01 ~ "**", p < .05 ~ "*", p < .10 ~ "+", TRUE ~ ""))

# ---- one-sided items: Q12 (aid to A) after A posts; Q11 (relief to B) after B posts, in control SDs
one_sided <- map_dfr(c(surveys$title), function(ttl) {
  x <- d %>% filter(title == ttl)
  map_dfr(list(c("milaid_A", "A", "Q12 Military aid to A, after A posts"), c("humanit", "B", "Q11 Humanitarian relief to B, after B posts")), function(s) {
    v <- s[1]; sd_c <- sd(x[[v]][x$side == "Control"], na.rm = TRUE)
    m <- lm_lin(reformulate("post_f", v), covariates = ~ prior, data = x %>% filter(!is.na(.data[[v]]), prior_miss == 0), se_type = "HC2")
    nm <- paste0("post_f", s[2], c(" Witness", " News", " Stats")); b <- coef(m)[nm] / sd_c; V <- vcov(m)[nm, nm] / sd_c^2
    w <- list(Witness = c(1, 0, 0), "News story" = c(0, 1, 0), Statistics = c(0, 0, 1), "All posts" = c(1, 1, 1) / 3,
              "H3a: personal - statistics" = c(.5, .5, -1), "H3b: witness - news story" = c(1, -1, 0))
    map_dfr(names(w), function(k) { e <- sum(w[[k]] * b); se <- sqrt(as.numeric(t(w[[k]]) %*% V %*% w[[k]]))
      tibble(survey = ttl, outcome = s[3], estimand = k, est = e, se = se, lo = e - 1.96 * se, hi = e + 1.96 * se, p = 2 * pnorm(-abs(e / se))) })
  })
}) %>% mutate(spec = "Adjusted for prior sympathy (primary)", stars = case_when(p < .01 ~ "**", p < .05 ~ "*", p < .10 ~ "+", TRUE ~ ""))

write.csv(bind_rows(res, one_sided), "victim_direction.csv", row.names = FALSE)
cat("saved victim_direction.csv\n\n")
res %>% filter(outcome == "index") %>%
  transmute(survey, spec = substr(spec, 1, 22), estimand, eff = sprintf("%+.3f%s (se %.3f)", est, stars, se)) %>%
  pivot_wider(names_from = estimand, values_from = eff) %>% as.data.frame() %>% print(row.names = FALSE)
cat("\nSE gain from prior-sympathy adjustment (index, 'All posts'):\n")
res %>% filter(outcome == "index", estimand == "All posts", spec %in% specs) %>% select(survey, spec, se) %>%
  pivot_wider(names_from = spec, values_from = se) %>% as.data.frame() %>% print(row.names = FALSE)
cat("\nOne-sided items (control SDs):\n")
one_sided %>% transmute(survey, outcome, estimand, eff = sprintf("%+.2f%s", est, stars)) %>%
  pivot_wider(names_from = estimand, values_from = eff) %>% as.data.frame() %>% print(row.names = FALSE)

# ---- figure
pd <- res %>% filter(spec != "Unadjusted" | TRUE) %>%
  mutate(survey = factor(survey, c(surveys$title, "Pooled (3 surveys)")),
         outcome = factor(OUTS[outcome], OUTS),
         estimand = factor(estimand, rev(names(W))),
         type = ifelse(grepl("^H3", estimand), "Contrast", "Effect toward the victim"),
         spec = factor(spec, c("Unadjusted", "Adjusted for prior sympathy (primary)", "Primary, first-time takers only")))
g <- ggplot(pd, aes(est, estimand, colour = spec)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.35) +
  geom_hline(yintercept = 2.5, colour = "grey80", linewidth = 0.3, linetype = "dashed") +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.65), size = 0.22, linewidth = 0.55) +
  facet_grid(outcome ~ survey, labeller = labeller(outcome = label_wrap_gen(18))) +
  scale_colour_manual(values = c("Unadjusted" = "grey55", "Adjusted for prior sympathy (primary)" = "#1f4c86",
                                 "Primary, first-time takers only" = "#C05020"), name = NULL) +
  labs(x = "Effect in control-group SDs (95% CI). Positive = toward the side whose suffering was shown; contrasts: positive = first format moves more",
       y = NULL, title = "Victim-direction effects by format, and the H3a / H3b contrasts",
       subtitle = "Effects average posts about both sides (sign-flipped so that positive = toward the victim). Below the dashed line: H3a (personal narrative minus statistics) and H3b (witness minus news story).") +
  theme_minimal(base_size = 10) +
  theme(strip.text.y = element_text(angle = 0, face = "bold", hjust = 0), strip.text.x = element_text(face = "bold", size = 11),
        panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), legend.position = "bottom", plot.title.position = "plot")
ggsave("plots/VICTIM_direction.png", g, width = 15, height = 10, dpi = 200, bg = "white")
cat("saved plots/VICTIM_direction.png\n")
