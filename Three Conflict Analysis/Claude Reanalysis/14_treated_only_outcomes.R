# Outcomes asked only after a post (no control group): compare formats and sides directly.
#   Donation (behavioral): "Would you consider donating to [group]?" yes/no, and the amount among yes.
#     Amounts are free text with absurd values (up to 6e17), so: amounts above $10,000 set to missing, then
#     winsorized at the survey's 99th percentile among donors; analyzed as log(1 + $).
#   Q4 emotions (afraid, exhausted, angry, sad; 1-4), Q2 likelihood of sharing (1-4), Q1 seen posts like this (1-5).
#   Credibility doubt: Q3 three words contain a skeptical word (fake, propaganda, biased, staged, ...).
# Model per survey: outcome ~ 0 + arm (six arm means); contrasts averaged over both sides:
#   H3a personal (witness + news) - statistics; H3b witness - news story; side: B posts - A posts.
# Outputs: treated_only_outcomes.csv, plots/TREATED_only_contrasts.png

source("00_prepare.R")
suppressMessages({ library(estimatr); library(tidytext) })

raw_files <- map_dfr(surveys$survey, function(f) {
  x <- read.csv(file.path("..", "Data", paste0(f, ".csv")), check.names = FALSE)
  x <- x[x$Finished %in% c(TRUE, "TRUE", "True") & x[["Consent 2"]] == "Yes, I am", ]
  dcols <- grep("^D[A-Z]{2}$", names(x), value = TRUE); acols <- grep("^D[A-Z]{2}a$", names(x), value = TRUE)
  tibble(survey = f, ResponseId = x$ResponseId,
         donate_txt = apply(x[dcols], 1, function(r) paste(r[!is.na(r) & r != ""], collapse = "")),
         amt_txt = apply(x[acols], 1, function(r) paste(r[!is.na(r) & r != ""], collapse = "")))
})

skeptic <- c("fake", "propaganda", "biased", "bias", "manipulative", "manipulation", "manipulated", "staged", "lies", "lie",
             "liar", "lying", "fabricated", "misleading", "exaggerated", "exaggeration", "unreal", "doubt", "doubtful",
             "skeptical", "sceptical", "suspicious", "untrue", "false", "scam", "fiction", "fictional", "agenda", "onesided")
sk <- dd_all %>% filter(side != "Control") %>% select(ResponseId, survey, text = `Post-Treatment 3`) %>%
  mutate(text = blank_na(text)) %>% filter(!is.na(text)) %>%
  unnest_tokens(word, text) %>% mutate(word = str_replace_all(word, "[^a-z]", "")) %>%
  group_by(ResponseId, survey) %>% summarise(skeptic = as.numeric(any(word %in% skeptic)), .groups = "drop")

feel4 <- c("Not at all" = 1, "Somewhat" = 2, "Strongly" = 3, "Very strongly" = 4)
d <- dd_all %>% filter(side != "Control") %>%
  left_join(raw_files, by = c("survey", "ResponseId")) %>% left_join(sk, by = c("survey", "ResponseId")) %>%
  mutate(donate = case_when(donate_txt == "Yes" ~ 1, donate_txt == "No" ~ 0),
         amt = suppressWarnings(as.numeric(gsub("[$, ]", "", amt_txt))),
         amt = ifelse(donate == 1 & !is.na(amt) & amt >= 0 & amt <= 10000, amt, NA)) %>%
  group_by(survey) %>% mutate(amt = pmin(amt, quantile(amt, .99, na.rm = TRUE)), log_amt = log1p(amt)) %>% ungroup() %>%
  mutate(afraid = unname(feel4[`Post-Treatment 4_1`]), exhausted = unname(feel4[`Post-Treatment 4_2`]),
         angry = unname(feel4[`Post-Treatment 4_3`]), sad = unname(feel4[`Post-Treatment 4_4`]),
         arm_f = factor(paste(side, fmt), c("A Witness", "A News", "A Stats", "B Witness", "B News", "B Stats")))

cat("Donation data: yes/no answered", sum(!is.na(d$donate)), "| amounts kept", sum(!is.na(d$amt)),
    "| dropped as >$10,000 or invalid", sum(d$donate == 1 & is.na(d$amt), na.rm = TRUE), "\n")

OUTS <- c(donate = "Would donate (share)", log_amt = "Amount if donating, log(1+$)", sad = "Q4 Sad (1-4)", angry = "Q4 Angry (1-4)",
          afraid = "Q4 Afraid (1-4)", exhausted = "Q4 Exhausted (1-4)", q2 = "Q2 Would share (1-4)",
          q1 = "Q1 Seen posts like this (1-5)", skeptic = "Q3 skeptical word (share)")
W <- list("H3a: personal - statistics" = c(.25, .25, -.5, .25, .25, -.5),
          "H3b: witness - news story"  = c(.5, -.5, 0, .5, -.5, 0),
          "Side: B posts - A posts"    = c(-1, -1, -1, 1, 1, 1) / 3)

res <- expand_grid(ttl = surveys$title, v = names(OUTS)) %>% pmap_dfr(function(ttl, v) {
  x <- d %>% filter(title == ttl, !is.na(.data[[v]]))
  m <- lm_robust(reformulate("0 + arm_f", v), data = x, se_type = "HC2")
  b <- coef(m); V <- vcov(m)
  bind_rows(
    tibble(estimand = paste("Mean:", levels(d$arm_f)), est = b, se = sqrt(diag(V))),
    map_dfr(names(W), function(k) { w <- W[[k]]; tibble(estimand = k, est = sum(w * b), se = sqrt(as.numeric(t(w) %*% V %*% w))) })) %>%
    mutate(survey = ttl, var = v, n = nrow(x), sd_all = sd(x[[v]]), lo = est - 1.96 * se, hi = est + 1.96 * se, p = 2 * pnorm(-abs(est / se)))
}) %>% group_by(survey, estimand) %>% mutate(q_bh = ifelse(grepl("^Mean", estimand), NA, p.adjust(p, "BH"))) %>% ungroup() %>%
  mutate(stars = case_when(grepl("^Mean", estimand) ~ "", p < .01 ~ "**", p < .05 ~ "*", p < .10 ~ "+", TRUE ~ ""))
write.csv(res, "treated_only_outcomes.csv", row.names = FALSE)
cat("saved treated_only_outcomes.csv\n\n")
res %>% filter(!grepl("^Mean", estimand)) %>%
  transmute(survey, outcome = var, estimand = sub(":.*", "", estimand), eff = sprintf("%+.3f%s", est, stars)) %>%
  pivot_wider(names_from = c(survey, estimand), values_from = eff) %>% as.data.frame() %>% print(row.names = FALSE)
cat("\nDonation shares by arm:\n")
res %>% filter(var == "donate", grepl("^Mean", estimand)) %>% transmute(survey, arm = sub("Mean: ", "", estimand), share = round(est, 3)) %>%
  pivot_wider(names_from = survey, values_from = share) %>% as.data.frame() %>% print(row.names = FALSE)

pd <- res %>% filter(!grepl("^Mean", estimand)) %>%
  mutate(est_sd = est / sd_all, lo_sd = lo / sd_all, hi_sd = hi / sd_all,
         outcome = factor(OUTS[var], rev(OUTS)), survey = factor(survey, surveys$title),
         estimand = factor(estimand, names(W)))
g <- ggplot(pd, aes(est_sd, outcome, colour = estimand)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.35) +
  geom_pointrange(aes(xmin = lo_sd, xmax = hi_sd), position = position_dodge(width = 0.6), size = 0.25, linewidth = 0.6) +
  facet_wrap(~survey, nrow = 1) +
  scale_colour_manual(values = c("H3a: personal - statistics" = "#1f4c86", "H3b: witness - news story" = "#2E9E7A",
                                 "Side: B posts - A posts" = "#DD6B20"), name = NULL) +
  labs(x = "Difference in SDs of the outcome (95% CI)", y = NULL,
       title = "Outcomes asked only after a post: format and side contrasts",
       subtitle = "Positive H3a = personal narratives higher than statistics; positive H3b = witness higher than news story; positive side = higher after Palestine / Armenia / RSF posts.") +
  theme_minimal(base_size = 10.5) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 11), plot.title.position = "plot")
ggsave("plots/TREATED_only_contrasts.png", g, width = 13, height = 6.5, dpi = 200, bg = "white")
cat("saved plots/TREATED_only_contrasts.png\n")
