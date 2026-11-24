# Q6 reconceptualized as three 0/1 outcomes (full sample; each option vs. everything else; the three sum to 1):
#   A has gone too far (not justified) | A is defending its interests (justified) | don't know enough
# Pooled across the three surveys: outcome ~ side (A posts, B posts vs control) + survey fixed effects,
#   SEs clustered by participant (855 took both NK and Sudan). Also each survey on its own (HC2).
# Effects in percentage points. Side A = Israel / Azerbaijan / SAF; side B = Palestine / Armenia / RSF.
# Outputs: q6_binary_effects.csv, plots/Q6_binary_pooled.png

source("00_prepare.R")
suppressMessages(library(estimatr))

OUTS <- c(q6_toofar = "A has gone too far;\nactions not justified", q6_defend = "A is defending its interests;\nactions justified",
          dk_justified = "Don't know enough")

d <- dd_all %>% mutate(side_f = relevel(factor(side), "Control"), survey_f = factor(title, surveys$title),
                       cluster_id = ifelse(!is.na(participantId) & participantId != "", participantId, paste0("row", row_number())))

tidy_side <- function(m) {
  ci <- confint(m); k <- grep("^side_f", names(coef(m)))
  tibble(side = sub("side_f", "", names(coef(m))[k]), est = 100 * coef(m)[k], lo = 100 * ci[k, 1], hi = 100 * ci[k, 2], p = m$p.value[k])
}
res <- map_dfr(names(OUTS), function(v) {
  x <- d %>% filter(!is.na(.data[[v]]))
  pooled <- tidy_side(lm_robust(reformulate(c("side_f", "survey_f"), v), data = x, clusters = cluster_id, se_type = "stata")) %>%
    mutate(survey = "Pooled (3 surveys)", ctrl = 100 * mean(x[[v]][x$side == "Control"]), n = nrow(x))
  each <- map_dfr(surveys$title, function(ttl) {
    y <- x %>% filter(title == ttl)
    tidy_side(lm_robust(reformulate("side_f", v), data = y, se_type = "HC2")) %>%
      mutate(survey = ttl, ctrl = 100 * mean(y[[v]][y$side == "Control"]), n = nrow(y))
  })
  bind_rows(pooled, each) %>% mutate(var = v)
}) %>% group_by(survey, side) %>% mutate(q_bh = p.adjust(p, "BH")) %>% ungroup() %>%
  mutate(stars = case_when(p < .01 ~ "**", p < .05 ~ "*", p < .10 ~ "+", TRUE ~ ""))
write.csv(res, "q6_binary_effects.csv", row.names = FALSE)
cat("saved q6_binary_effects.csv\n")
res %>% transmute(survey, outcome = var, side, eff = sprintf("%+.1f%s", est, stars), ctrl = round(ctrl, 1)) %>%
  pivot_wider(names_from = side, values_from = eff) %>% as.data.frame() %>% print(row.names = FALSE)

pd <- res %>% mutate(survey = factor(survey, c("Pooled (3 surveys)", surveys$title)),
                     outcome = factor(OUTS[var], rev(OUTS)),
                     side_lab = ifelse(side == "A", "Posts about Israel / Azerbaijan / SAF suffering", "Posts about Palestine / Armenia / RSF suffering"),
                     lab = sprintf("%+.1f%s", est, stars))
ctrl_lab <- pd %>% distinct(survey, outcome, ctrl) %>% mutate(txt = sprintf("control %.0f%%", ctrl))
rng <- max(abs(c(pd$lo, pd$hi)))
g <- ggplot(pd, aes(est, outcome, colour = side_lab)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.35) +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.55), size = 0.35, linewidth = 0.7) +
  geom_text(aes(x = ifelse(est >= 0, hi, lo), label = lab, hjust = ifelse(est >= 0, -0.15, 1.15)),
            position = position_dodge(width = 0.55), size = 2.9, show.legend = FALSE) +
  geom_text(data = ctrl_lab, aes(x = -Inf, y = outcome, label = txt), inherit.aes = FALSE, hjust = -0.05, vjust = -4.6,
            size = 2.6, colour = "grey45", fontface = "italic") +
  facet_wrap(~survey, nrow = 1) +
  scale_x_continuous(limits = c(-rng, rng) * 1.35) +
  scale_colour_manual(values = c("Posts about Israel / Azerbaijan / SAF suffering" = "#2B6CB0",
                                 "Posts about Palestine / Armenia / RSF suffering" = "#DD6B20"), name = NULL) +
  labs(x = "Change in the share choosing this option, percentage points vs. control (95% CI)", y = NULL,
       title = "Q6 as three outcomes: \"Thinking about [A]'s military action, which statement best describes what you think?\"",
       subtitle = paste0("Each option coded 0/1 for everyone (the three sum to 100%). Pooled: survey fixed effects, SEs clustered by participant. ",
                         "A = Israel / Azerbaijan / SAF.\n** p<.01 * p<.05 + p<.10 (unadjusted)")) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 11.5), plot.title.position = "plot", panel.spacing.x = unit(1.2, "lines"))
ggsave("plots/Q6_binary_pooled.png", g, width = 15, height = 5.5, dpi = 200, bg = "white")
cat("saved plots/Q6_binary_pooled.png\n")
