# Robustness of Q5-Q7 to how non-committal answers are coded.
# The posts changed who gave a substantive answer in some arms (e.g., Sudan SAF statistics post: +23 pts answering Q6),
# so dropping "don't know / neither / not sure" mixes persuasion with selection. Three codings, pooled posts vs control:
#   (1) Midpoint (primary): everyone kept, non-committal answers at the scale midpoint
#   (2) Substantive only: the original coding (non-committal answers dropped)
#   (3) Shares: toward A / middle (equally, neither, not sure, don't know) / toward B, everyone kept
# Plus the selection test itself: does the post change the share giving a substantive answer?
# Outputs: coding_robustness.csv, plots/ROBUST_codings.png

source("00_prepare.R")
suppressMessages(library(estimatr))

d <- dd_all %>% mutate(side_f = relevel(factor(side), "Control"),
                       subst_q5 = as.numeric(q5 <= 5), subst_q6 = as.numeric(q6 <= 2), subst_q7 = as.numeric(q7 <= 3),
                       q6_A = as.numeric(q6 == 1), q6_B = as.numeric(q6 == 2),
                       q7_A = as.numeric(q7 == 1), q7_B = as.numeric(q7 == 3))

SPECS <- tribble(
  ~item, ~coding,                                 ~var,               ~scale,
  "Q5 Sympathy",  "1 Midpoint (primary), 1-5",     "symp_A",           "scale",
  "Q5 Sympathy",  "2 Substantive only, 1-5",       "symp_A_subst",     "scale",
  "Q5 Sympathy",  "3a Share toward A",             "q5_A",             "share",
  "Q5 Sympathy",  "3b Share in the middle",        "q5_mid",           "share",
  "Q5 Sympathy",  "3c Share toward B",             "q5_B",             "share",
  "Q5 Sympathy",  "4 Gave a substantive answer",   "subst_q5",         "share",
  "Q6 Justified", "1 Midpoint (primary), 1-3",     "justified",        "scale",
  "Q6 Justified", "2 Substantive only, share",     "justified_subst",  "share",
  "Q6 Justified", "3a Share 'justified'",          "q6_A",             "share",
  "Q6 Justified", "3b Share 'don't know'",         "dk_justified",     "share",
  "Q6 Justified", "3c Share 'gone too far'",       "q6_B",             "share",
  "Q7 U.S. approach", "1 Midpoint (primary), 1-3", "us_lenient",       "scale",
  "Q7 U.S. approach", "2 Substantive only, 1-3",   "us_lenient_subst", "scale",
  "Q7 U.S. approach", "3a Share 'no pressure'",    "q7_A",             "share",
  "Q7 U.S. approach", "3b Share 'don't know'",     "dk_us",            "share",
  "Q7 U.S. approach", "3c Share 'reduce aid'",     "q7_B",             "share")

res <- pmap_dfr(SPECS, function(item, coding, var, scale) { v <- var; map_dfr(surveys$title, function(ttl) {
  x <- d %>% filter(title == ttl, !is.na(.data[[v]]))
  m <- lm_robust(reformulate("side_f", v), data = x, se_type = "HC2"); ci <- confint(m)
  k <- grep("^side_f", names(coef(m)))
  tibble(item, coding, var, scale, survey = ttl, side = sub("side_f", "", names(coef(m))[k]),
         est = coef(m)[k], lo = ci[k, 1], hi = ci[k, 2], p = m$p.value[k], n = nrow(x),
         ctrl_mean = mean(x[[v]][x$side == "Control"]))
})}) %>% mutate(stars = case_when(p < .01 ~ "**", p < .05 ~ "*", p < .10 ~ "+", TRUE ~ ""))
write.csv(res, "coding_robustness.csv", row.names = FALSE)
cat("saved coding_robustness.csv\n")
res %>% transmute(item, coding, survey, side, eff = sprintf("%+.3f%s", est, stars), n) %>%
  pivot_wider(names_from = c(survey, side), values_from = eff, id_cols = c(item, coding)) %>%
  as.data.frame() %>% print(row.names = FALSE)

pd <- res %>% mutate(survey = factor(survey, surveys$title), coding = factor(coding, rev(unique(SPECS$coding))),
                     side_lab = ifelse(side == "A", "Posts about Israel / Azerbaijan / SAF", "Posts about Palestine / Armenia / RSF"))
g <- ggplot(pd, aes(est, coding, colour = side_lab)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.35) +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.55), size = 0.25, linewidth = 0.6) +
  facet_grid(item ~ survey, scales = "free_y", space = "free_y") +
  scale_colour_manual(values = c("Posts about Israel / Azerbaijan / SAF" = "#2B6CB0", "Posts about Palestine / Armenia / RSF" = "#DD6B20"), name = NULL) +
  labs(x = "Difference from control (95% CI). Scales in their own units; shares 0-1", y = NULL,
       title = "How non-committal answers are coded: midpoint vs. dropped vs. three-way shares",
       subtitle = "If a post moves people out of 'don't know' as well as across sides, the substantive-only coding overstates persuasion.") +
  theme_minimal(base_size = 10.5) +
  theme(strip.text.y = element_text(angle = 0, face = "bold", hjust = 0), strip.text.x = element_text(face = "bold", size = 12),
        panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), legend.position = "bottom", plot.title.position = "plot")
ggsave("plots/ROBUST_codings.png", g, width = 13, height = 10, dpi = 200, bg = "white")
cat("saved plots/ROBUST_codings.png\n")
