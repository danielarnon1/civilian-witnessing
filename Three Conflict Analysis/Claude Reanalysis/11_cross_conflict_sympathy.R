# Cross-conflict sympathies.
# The only post-treatment sympathy item (Q5) is about each survey's own conflict; sympathies toward other conflicts
# (Pr5a NK, Pr5b Sudan, Pr6a Israel-Palestine, Pr6b / Pr6 Russia-Ukraine) are asked BEFORE the post.
#
# Part 1 — Cross-survey carry-over / spillover (causal, among the 855 who took both NK and Sudan surveys):
#   the randomly assigned condition in the FIRST survey is the treatment; the outcome is the sympathy item answered in
#   the SECOND survey's pre-treatment block (i.e., after the first survey's post).
#     Sudan first -> NK survey: Sudan sympathy (same conflict: persistence), NK, Israel-Palestine, Ukraine
#     NK first    -> Sudan survey: NK sympathy (persistence), Sudan, Israel-Palestine, Ukraine
# Part 2 — Within-survey placebo (all respondents of each survey): the same pre-treatment items vs the current
#   survey's condition. Answered before the post, so effects should be zero; this is the noise benchmark.
#
# Codings (A = Israel / Azerbaijan / SAF side):
#   two-sided items: +1 more with A, -1 more with B, 0 both / neither; "I don't know" excluded (dk share is its own outcome)
#   Israel-Palestine (7 options): 1 entirely Palestinians ... 5 entirely Israelis; neither / not sure excluded
#   Ukraine: 1 = more with Ukrainians, 0 = Russians / both / neither; "I don't know" excluded
# Model: outcome ~ side (A posts, B posts vs control), OLS HC2; BH across tests within each part.
# Outputs: cross_conflict_sympathy.csv, plots/CROSS_CONFLICT_sympathy.png

source("00_prepare.R")
suppressMessages({ library(estimatr); library(lubridate) })

code_two <- function(x, a_rx, b_rx) case_when(grepl(a_rx, x) ~ 1, grepl(b_rx, x) ~ -1,
                                              grepl("^Both|^Neither", x) ~ 0, TRUE ~ NA_real_)
is_dk <- function(x) ifelse(is.na(x), NA, as.numeric(grepl("know", x)))

d <- dd_all %>%
  mutate(
    s_nk    = code_two(`Pre-Treatment 5a`, "Azerbaijani", "Armenian"),
    dk_nk   = is_dk(`Pre-Treatment 5a`),
    s_sudan = code_two(`Pre-Treatment 5b`, "SAF", "RSF"),
    dk_sudan = is_dk(`Pre-Treatment 5b`),
    ip_raw  = ifelse(title == "Israel-Hamas", NA, `Pre-Treatment 6a`),
    s_ip    = case_when(grepl("^Entirely with the Israeli", ip_raw) ~ 5, grepl("^Mostly with the Israeli", ip_raw) ~ 4,
                        grepl("^Equally", ip_raw) ~ 3, grepl("^Mostly with the Palestinian", ip_raw) ~ 2,
                        grepl("^Entirely with the Palestinian", ip_raw) ~ 1),
    ukr_raw = ifelse(title == "Israel-Hamas", `Pre-Treatment 6`, `Pre-Treatment 6b`),
    s_ukr   = case_when(ukr_raw == "More with the Ukrainians" ~ 1, grepl("know", ukr_raw) | is.na(ukr_raw) ~ NA_real_, TRUE ~ 0),
    # Israel-Hamas survey: its own prior Israel-Palestine sympathy (4 options) as a placebo item
    s_ih_prior = ifelse(title == "Israel-Hamas", code_two(`Pre-Treatment 5`, "Israelis", "Palestinians"), NA),
    side_f = relevel(factor(side), "Control"),
    t_start = suppressWarnings(mdy_hm(StartDate)), t_end = suppressWarnings(mdy_hm(EndDate)))

ITEMS <- c(s_sudan = "Sudan: SAF (+1) vs RSF (-1)", dk_sudan = "Sudan: 'don't know' (share)",
           s_nk = "Nagorno-Karabakh: Azerbaijan (+1) vs Armenia (-1)", dk_nk = "Nagorno-Karabakh: 'don't know' (share)",
           s_ip = "Israel-Palestine: 1 Palestinians ... 5 Israelis", s_ukr = "Russia-Ukraine: more with Ukrainians (share)",
           s_ih_prior = "Israel-Palestine (IH survey item): Israelis (+1) vs Palestinians (-1)")

fit_side <- function(x, v) {
  x <- x %>% filter(!is.na(.data[[v]]))
  if (n_distinct(x$side_f) < 3 || nrow(x) < 30) return(tibble())
  m <- lm_robust(reformulate("side_f", v), data = x, se_type = "HC2"); ci <- confint(m)
  k <- grep("^side_f", names(coef(m)))
  tibble(var = v, side = sub("side_f", "", names(coef(m))[k]), est = coef(m)[k], lo = ci[k, 1], hi = ci[k, 2],
         p = m$p.value[k], n = nrow(x), n_ctrl = sum(x$side_f == "Control"), ctrl_mean = mean(x[[v]][x$side_f == "Control"]))
}

# ---------------------------------------------------------------- Part 1: cross-survey carry-over
nk <- d %>% filter(survey == "nk")    %>% group_by(participantId) %>% slice_min(t_start, n = 1, with_ties = FALSE) %>% ungroup()
sd <- d %>% filter(survey == "sudan") %>% group_by(participantId) %>% slice_min(t_start, n = 1, with_ties = FALSE) %>% ungroup()
pairs <- inner_join(nk %>% select(participantId, s_nk_t = t_start, e_nk_t = t_end),
                    sd %>% select(participantId, s_sd_t = t_start, e_sd_t = t_end), by = "participantId") %>%
  mutate(order = case_when(s_nk_t < s_sd_t ~ "nk_first", s_sd_t < s_nk_t ~ "sudan_first", TRUE ~ "ambiguous"))

out_cols <- c("s_sudan", "dk_sudan", "s_nk", "dk_nk", "s_ip", "s_ukr")
# first-survey condition joined to the second survey's pre-treatment answers
sudan_first <- pairs %>% filter(order == "sudan_first") %>%
  left_join(sd %>% select(participantId, side_f, fmt), by = "participantId") %>%
  left_join(nk %>% select(participantId, all_of(out_cols)), by = "participantId") %>%
  identity()
nk_first <- pairs %>% filter(order == "nk_first") %>%
  left_join(nk %>% select(participantId, side_f, fmt), by = "participantId") %>%
  left_join(sd %>% select(participantId, all_of(out_cols)), by = "participantId")
cat("Sudan first:", nrow(sudan_first), " NK first:", nrow(nk_first), "\n")
print(table(sudan_first$side_f)); print(table(nk_first$side_f))

carry <- bind_rows(
  map_dfr(out_cols, ~ fit_side(sudan_first, .x)) %>% mutate(design = "Sudan first", same_conflict = var %in% c("s_sudan", "dk_sudan")),
  map_dfr(out_cols, ~ fit_side(nk_first, .x))    %>% mutate(design = "NK first",    same_conflict = var %in% c("s_nk", "dk_nk"))) %>%
  mutate(part = "Cross-survey (after the first survey's post)") %>%
  group_by(same_conflict) %>% mutate(q_bh = p.adjust(p, "BH")) %>% ungroup()

# ---------------------------------------------------------------- Part 2: within-survey placebo
placebo <- map_dfr(surveys$title, function(ttl) {
  x <- d %>% filter(title == ttl)
  vars <- if (ttl == "Israel-Hamas") c("s_ih_prior", "s_ukr") else out_cols
  map_dfr(vars, ~ fit_side(x, .x)) %>% mutate(design = ttl)
}) %>% mutate(part = "Placebo: same survey, answered before the post", same_conflict = FALSE, q_bh = p.adjust(p, "BH"))

res <- bind_rows(carry, placebo) %>% mutate(label = ITEMS[var], stars = case_when(p < .01 ~ "**", p < .05 ~ "*", p < .10 ~ "+", TRUE ~ ""))
write.csv(res, "cross_conflict_sympathy.csv", row.names = FALSE)
cat("saved cross_conflict_sympathy.csv\n\n")
res %>% transmute(part = substr(part, 1, 12), design, item = var, side, eff = sprintf("%+.3f%s", est, stars),
                  p = round(p, 3), q = round(q_bh, 3), n, ctrl = round(ctrl_mean, 2)) %>%
  as.data.frame() %>% print(row.names = FALSE)

# ---------------------------------------------------------------- figure
pd <- res %>%
  mutate(panel = factor(case_when(part != "Placebo: same survey, answered before the post" & design == "Sudan first" ~
                                    "Saw a Sudan post first\nanswers in the later NK survey",
                                  part != "Placebo: same survey, answered before the post" ~ "Saw an NK post first\nanswers in the later Sudan survey",
                                  TRUE ~ paste0("Placebo: ", design, " survey\nanswered before that survey's post")),
                        c("Saw a Sudan post first\nanswers in the later NK survey", "Saw an NK post first\nanswers in the later Sudan survey",
                          paste0("Placebo: ", surveys$title, " survey\nanswered before that survey's post"))),
         item = factor(paste0(label, ifelse(same_conflict, "  [same conflict]", "")),
                       rev(unique(paste0(label, ifelse(same_conflict, "  [same conflict]", ""))[order(match(var, names(ITEMS)), !same_conflict)]))),
         side_lab = ifelse(side == "A", "Post about Azerbaijan / SAF / Israel suffering", "Post about Armenia / RSF / Palestine suffering"),
         lab = sprintf("%+.2f%s", est, stars))
g <- ggplot(pd, aes(est, item, colour = side_lab)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.35) +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.55), size = 0.3, linewidth = 0.6) +
  geom_text(aes(x = ifelse(est >= 0, hi, lo), label = lab, hjust = ifelse(est >= 0, -0.15, 1.15)),
            position = position_dodge(width = 0.55), size = 2.6, show.legend = FALSE) +
  facet_wrap(~panel, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = c("Post about Azerbaijan / SAF / Israel suffering" = "#2B6CB0",
                                 "Post about Armenia / RSF / Palestine suffering" = "#DD6B20"), name = NULL) +
  scale_x_continuous(expand = expansion(mult = 0.25)) +
  labs(x = "Difference from control (95% CI). Two-sided items: +1 = side A, -1 = side B; shares 0-1; Israel-Palestine 1-5", y = NULL,
       title = "Do posts about one conflict change sympathies in other conflicts?",
       subtitle = paste0("Top: respondents who took both surveys; the first survey's (random) post vs. control, sympathies measured at the start of the second survey.\n",
                         "Below: placebo, the same items vs. the current survey's post, answered before that post (should be ~0). ** p<.01 * p<.05 + p<.10")) +
  theme_minimal(base_size = 10.5) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), legend.position = "bottom",
        strip.text = element_text(face = "bold", hjust = 0), plot.title.position = "plot", panel.spacing = unit(1, "lines"))
ggsave("plots/CROSS_CONFLICT_sympathy.png", g, width = 13, height = 11, dpi = 200, bg = "white")
cat("saved plots/CROSS_CONFLICT_sympathy.png\n")
