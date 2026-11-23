# (1) Israel-Hamas outcomes not analyzed elsewhere (pooled posts about each side vs control, OLS HC2, BH across them):
#     Q9 importance of recovering the hostages (1-5); Q13 thermometers: Democrats, Republicans, Harris, Trump (0-100);
#     Q14 thermometers: supporters of Israel, supporters of Palestine, Jews, Muslims (0-100);
#     Q16-18 comfort with a Palestinian as friend / neighbor / child's spouse; Q19-21 same for an Israeli (1-4).
# (2) Manipulation check right after the post (victim's name, or casualty number in the statistics posts).
#     The 6-26 questionnaire PDFs list different answer options from the fielded surveys, so the "correct" answer is
#     taken as each arm's most common answer. The NK statistics arms split between two common answers (flagged).
# Outputs: ih_extra_outcomes.csv, manipulation_check.csv, plots/IH_extra_outcomes.png

source("00_prepare.R")
suppressMessages(library(estimatr))

imp5 <- c("Not at all important" = 1, "Slightly important" = 2, "Moderately important" = 3, "Very important" = 4, "Extremely important" = 5)
comf4 <- c("Not at all comfortable" = 1, "Not too comfortable" = 2, "Somewhat comfortable" = 3, "Extremely comfortable" = 4)
ih <- dd_all %>% filter(title == "Israel-Hamas") %>%
  mutate(side_f = relevel(factor(side), "Control"),
         hostages = unname(imp5[`Post-Treatment 9`]),
         th_dem = as.numeric(`Post-Treatment 13_1`), th_rep = as.numeric(`Post-Treatment 13_2`),
         th_harris = as.numeric(`Post-Treatment 13_3`), th_trump = as.numeric(`Post-Treatment 13_4`),
         th_isr_sup = as.numeric(`Post-Treatment 14_5`), th_pal_sup = as.numeric(`Post-Treatment 14_6`),
         th_jews = as.numeric(`Post-Treatment 14_7`), th_muslims = as.numeric(`Post-Treatment 14_8`),
         pal_friend = unname(comf4[`Post-Treatment 16`]), pal_neighbor = unname(comf4[`Post-Treatment 17`]),
         pal_marry = unname(comf4[`Post-Treatment 18`]), isr_friend = unname(comf4[`Post-Treatment 19`]),
         isr_neighbor = unname(comf4[`Post-Treatment 20`]), isr_marry = unname(comf4[`Post-Treatment 21`]))

OUTS <- c(hostages = "Q9 Recovering hostages important (1-5)",
          th_isr_sup = "Q14 Thermometer: supporters of Israel", th_pal_sup = "Q14 Thermometer: supporters of Palestine",
          th_jews = "Q14 Thermometer: Jews", th_muslims = "Q14 Thermometer: Muslims",
          th_dem = "Q13 Thermometer: Democrats", th_rep = "Q13 Thermometer: Republicans",
          th_harris = "Q13 Thermometer: Harris", th_trump = "Q13 Thermometer: Trump",
          isr_friend = "Q19 Israeli as friend (1-4)", isr_neighbor = "Q20 Israeli as neighbor (1-4)", isr_marry = "Q21 Israeli marrying your child (1-4)",
          pal_friend = "Q16 Palestinian as friend (1-4)", pal_neighbor = "Q17 Palestinian as neighbor (1-4)", pal_marry = "Q18 Palestinian marrying your child (1-4)")

res <- map_dfr(names(OUTS), function(v) {
  x <- ih %>% filter(!is.na(.data[[v]]))
  m <- lm_robust(reformulate("side_f", v), data = x, se_type = "HC2"); ci <- confint(m); k <- grep("^side_f", names(coef(m)))
  sd_c <- sd(x[[v]][x$side == "Control"])
  tibble(var = v, label = OUTS[[v]], side = sub("side_f", "", names(coef(m))[k]), est = coef(m)[k], lo = ci[k, 1], hi = ci[k, 2],
         p = m$p.value[k], n = nrow(x), ctrl_mean = mean(x[[v]][x$side == "Control"]), est_sd = coef(m)[k] / sd_c,
         lo_sd = ci[k, 1] / sd_c, hi_sd = ci[k, 2] / sd_c)
}) %>% mutate(q_bh = p.adjust(p, "BH"), stars = case_when(p < .01 ~ "**", p < .05 ~ "*", p < .10 ~ "+", TRUE ~ ""))
write.csv(res, "ih_extra_outcomes.csv", row.names = FALSE)
cat("saved ih_extra_outcomes.csv\n")
res %>% transmute(label, side, eff = sprintf("%+.2f%s", est, stars), q = round(q_bh, 3), ctrl = round(ctrl_mean, 1)) %>%
  pivot_wider(names_from = side, values_from = c(eff, q)) %>% as.data.frame() %>% print(row.names = FALSE)

g <- ggplot(res %>% mutate(label = factor(label, rev(OUTS)),
                           side_lab = ifelse(side == "A", "Posts about Israeli suffering", "Posts about Palestinian suffering")),
            aes(est_sd, label, colour = side_lab)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.35) +
  geom_pointrange(aes(xmin = lo_sd, xmax = hi_sd), position = position_dodge(width = 0.55), size = 0.3, linewidth = 0.6) +
  scale_colour_manual(values = c("Posts about Israeli suffering" = "#2B6CB0", "Posts about Palestinian suffering" = "#DD6B20"), name = NULL) +
  labs(x = "Difference from control in control-group SDs (95% CI)", y = NULL,
       title = "Israel-Hamas survey: hostages, feeling thermometers and social distance",
       subtitle = "Pooled posts about each side vs. control. Social-distance items: higher = more comfortable.") +
  theme_minimal(base_size = 10.5) + theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
                                          legend.position = "bottom", plot.title.position = "plot")
ggsave("plots/IH_extra_outcomes.png", g, width = 10, height = 7, dpi = 200, bg = "white")
cat("saved plots/IH_extra_outcomes.png\n\n")

# ---- manipulation check pass rates
chk_cols <- list(ih = c(Israel_CW = "AIC", Israel_Personal = "AIP", Israel_Statistics = "AIS",
                        Palestine_CW = "APC", Palestine_Personal = "APP", Palestine_Statistics = "APS"),
                 nk = c(Azerbaijan_CW = "AZC", Azerbaijan_Personal = "AZP", Azerbaijan_Statistics = "AZS",
                        Armenia_CW = "AMC", Armenia_Personal = "AMP", Armenia_Statistics = "AMS"),
                 sudan = c(SAF_CW = "ASC", SAF_Personal = "ASP", SAF_Statistics = "ASS",
                           RSF_CW = "ARC", RSF_Personal = "ARP", RSF_Statistics = "ARS"))
mc <- imap_dfr(chk_cols, function(cols, sv) imap_dfr(cols, function(col, cond) {
  x <- dd_all %>% filter(survey == sv, condition == paste0("Condition_", cond))
  a <- blank_na(x[[col]]); tb <- sort(table(a), decreasing = TRUE)
  tibble(survey = sv, condition = cond, n = nrow(x), answered = sum(!is.na(a)), modal_answer = names(tb)[1],
         pass_rate = as.numeric(tb[1]) / sum(!is.na(a)), second_answer = names(tb)[2], second_share = as.numeric(tb[2]) / sum(!is.na(a)))
})) %>% mutate(flag = ifelse(second_share > 0.25, "ambiguous: two common answers", ""))
write.csv(mc, "manipulation_check.csv", row.names = FALSE)
cat("saved manipulation_check.csv\n")
print(mc %>% mutate(across(c(pass_rate, second_share), ~ round(.x, 2))) %>% as.data.frame(), row.names = FALSE)
