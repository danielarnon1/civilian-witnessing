# Cross-conflict spillover: does a post about one conflict change views of OTHER conflicts?
# The only post-treatment items that ask about other conflicts are the Q8 leader-attention ratings:
#   8_1 Russia-Ukraine, 8_2 Israel-Hamas, 8_3 Myanmar, 8_4 Sudan, 8_5 Nagorno-Karabakh (8_5 only in NK / Sudan surveys).
# (Pr6 sympathies about other conflicts were asked before treatment, so they cannot be outcomes.)
# Outcomes: share saying U.S. leaders give that conflict "too little attention", and share saying "too much".
# Model: outcome ~ side (A posts, B posts vs Control), OLS with HC2 robust SEs, separately by survey.
# Outputs: spillover_effects.csv, plots/SPILLOVER_attention.png

source("00_prepare.R")
suppressMessages(library(estimatr))

items <- c("Russia-Ukraine" = "Post-Treatment 8_1", "Israel-Hamas" = "Post-Treatment 8_2",
           "Myanmar" = "Post-Treatment 8_3", "Sudan" = "Post-Treatment 8_4", "Nagorno-Karabakh" = "Post-Treatment 8_5")
own <- c("Israel-Hamas" = "Israel-Hamas", "Nagorno-Karabakh" = "Nagorno-Karabakh", "Sudan" = "Sudan")

res <- map_dfr(surveys$title, function(ttl) {
  s <- surveys[surveys$title == ttl, ]
  d <- dd_all %>% filter(title == ttl) %>% mutate(side_f = relevel(factor(side), "Control"))
  map_dfr(names(items), function(conf) {
    col <- items[[conf]]
    if (!col %in% names(d) || all(is.na(d[[col]]) | d[[col]] == "")) return(tibble())
    x <- d %>% filter(!is.na(.data[[col]]), .data[[col]] != "") %>%
      mutate(little = as.numeric(.data[[col]] == "Too little attention"),
             much   = as.numeric(.data[[col]] == "Too much attention"))
    map_dfr(c(little = "little", much = "much"), function(v) {
      m <- lm_robust(reformulate("side_f", v), data = x, se_type = "HC2"); ci <- confint(m)
      k <- grep("^side_f", names(coef(m)))
      tibble(survey = ttl, conflict = conf, outcome = v, side = sub("side_f", "", names(coef(m))[k]),
             side_lab = ifelse(side == "A", s$A_lab, s$B_lab),
             est = coef(m)[k], lo = ci[k, 1], hi = ci[k, 2], p = m$p.value[k],
             ctrl_share = mean(x[[v]][x$side == "Control"]), n = nrow(x))
    })
  })
}) %>%
  mutate(own = conflict == own[survey],
         # BH across all spillover (non-own) tests, per outcome
         stars = case_when(p < .01 ~ "**", p < .05 ~ "*", p < .10 ~ "+", TRUE ~ "")) %>%
  group_by(outcome, own) %>% mutate(q_bh = p.adjust(p, "BH")) %>% ungroup()

write.csv(res, "spillover_effects.csv", row.names = FALSE)
cat("saved spillover_effects.csv\n")
res %>% filter(outcome == "little") %>%
  transmute(survey, conflict = paste0(conflict, ifelse(own, " (own)", "")), side_lab,
            effect_pts = sprintf("%+.1f%s", 100 * est, stars), ctrl = sprintf("%.0f%%", 100 * ctrl_share), q_bh = round(q_bh, 3)) %>%
  as.data.frame() %>% print(row.names = FALSE)

# ---------------------------------------------------------------- figure
conf_order <- c("Israel-Hamas", "Nagorno-Karabakh", "Sudan", "Russia-Ukraine", "Myanmar")
pd <- res %>% filter(outcome == "little") %>%
  mutate(survey = factor(paste0("Posts about the ", c("Israel-Hamas" = "Israel-Hamas war", "Nagorno-Karabakh" = "Nagorno-Karabakh conflict",
                                                      "Sudan" = "Sudanese civil war")[survey], " (", survey, " survey)"),
                         paste0("Posts about the ", c("Israel-Hamas war", "Nagorno-Karabakh conflict", "Sudanese civil war"),
                                " (", surveys$title, " survey)")),
         conf_lab = paste0(conflict, ifelse(own, "  [same conflict]", ""), "\nControl: ", round(100 * ctrl_share), "% too little"),
         key = factor(paste(survey, conf_lab), unique(paste(survey, conf_lab)[order(survey, !own, match(conflict, conf_order))]) %>% rev()),
         side = factor(side, c("A", "B")),
         lab = sprintf("%+.1f%s", 100 * est, stars))
rng <- max(abs(c(pd$lo, pd$hi))) * 100
g <- ggplot(pd, aes(100 * est, key, colour = side)) +
  # shade the row for the conflict the post was about (discrete tile keeps facet rows independent)
  geom_tile(data = pd %>% filter(own) %>% distinct(survey, key), aes(x = 0, y = key), width = Inf, height = 1,
            fill = "grey93", inherit.aes = FALSE) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_pointrange(aes(xmin = 100 * lo, xmax = 100 * hi), position = position_dodge(width = 0.6), size = 0.4, linewidth = 0.75) +
  geom_text(aes(x = 100 * ifelse(est >= 0, hi, lo), label = lab, hjust = ifelse(est >= 0, -0.15, 1.15)),
            position = position_dodge(width = 0.6), size = 3, show.legend = FALSE) +
  facet_wrap(~survey, ncol = 1, scales = "free_y") +
  scale_y_discrete(labels = function(x) sub("^Posts about.*?survey\\) ", "", x)) +
  scale_x_continuous(limits = c(-rng, rng) * 1.3) +
  scale_colour_manual(values = c(A = "#2B6CB0", B = "#DD6B20"), name = NULL,
                      labels = c(A = "Posts about Israel / Azerbaijan / SAF suffering", B = "Posts about Palestine / Armenia / RSF suffering")) +
  labs(x = "Change in share saying U.S. leaders give this conflict TOO LITTLE attention (percentage points vs. control, 95% CI)", y = NULL,
       title = "Spillover: does a post about one conflict change views of other conflicts?",
       subtitle = paste0("Q8: \"Are U.S. leaders giving each of the following issues too much, too little, or the right amount of attention?\"\n",
                         "Shaded row = the conflict the post was about. Nagorno-Karabakh was not asked in the Israel-Hamas survey. ",
                         "** p<.01 * p<.05 + p<.10 (unadjusted).")) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), legend.position = "bottom",
        strip.text = element_text(face = "bold", hjust = 0, size = 11.5), plot.title.position = "plot",
        axis.text.y = element_text(lineheight = 0.9), panel.spacing.y = unit(1, "lines"))
ggsave("plots/SPILLOVER_attention.png", g, width = 11, height = 11, dpi = 200, bg = "white")
cat("saved plots/SPILLOVER_attention.png\n")
