# Do pre-treatment sympathies about OTHER conflicts moderate the effect of the post on sympathy in THIS conflict?
# Other-conflict items (asked before the post; the own-conflict item is excluded):
#   Israel-Hamas survey:      Russia-Ukraine (Pr6)
#   Nagorno-Karabakh survey:  Sudan (Pr5b), Israel-Palestine (Pr6a, 7 options), Russia-Ukraine (Pr6b)
#   Sudan survey:             Nagorno-Karabakh (Pr5a), Israel-Palestine (Pr6a, 7 options), Russia-Ukraine (Pr6b)
# All answer categories are kept (incl. both / neither / don't know).
# Outcome: Q5 sympathy toward side A of THIS conflict (1-5, non-committal = 3). Side A = Israel / Azerbaijan / SAF.
# (1) Interaction: symp_A ~ side * prior_category, joint robust Wald test of the side x category terms
#     (categories with fewer than 10 control respondents are left out of the test); BH across all tests.
# (2) Within each category: effects of A posts and B posts vs control (HC2), flagged if control n < 10.
# Outputs: other_conflict_priors.csv, other_conflict_interactions.csv, plots/OTHER_PRIORS_sympathy.png

source("00_prepare.R")
suppressMessages(library(estimatr))
MIN_CTRL <- 10

four <- function(x, a_rx, a_lab, b_rx, b_lab) case_when(
  is.na(x) ~ NA_character_, grepl(a_rx, x) ~ a_lab, grepl(b_rx, x) ~ b_lab,
  grepl("^Both", x) ~ "Both equally", grepl("^Neither", x) ~ "Neither", grepl("know", x) ~ "Don't know")
ip7 <- function(x) case_when(
  is.na(x) ~ NA_character_,
  grepl("^Entirely with the Israeli", x) ~ "Entirely Israelis", grepl("^Mostly with the Israeli", x) ~ "Mostly Israelis",
  grepl("^Equally", x) ~ "Equally", grepl("^Mostly with the Palestinian", x) ~ "Mostly Palestinians",
  grepl("^Entirely with the Palestinian", x) ~ "Entirely Palestinians", grepl("^With neither", x) ~ "Neither",
  grepl("^Not sure", x) ~ "Not sure")

d <- dd_all %>% mutate(
  side_f = relevel(factor(side), "Control"),
  o_ukr   = four(ifelse(title == "Israel-Hamas", `Pre-Treatment 6`, `Pre-Treatment 6b`),
                 "Ukrainians", "More with Ukrainians", "Russians", "More with Russians"),
  o_sudan = ifelse(title == "Nagorno-Karabakh", four(`Pre-Treatment 5b`, "SAF", "More with SAF", "RSF", "More with RSF"), NA),
  o_nk    = ifelse(title == "Sudan", four(`Pre-Treatment 5a`, "Azerbaijani", "More with Azerbaijanis", "Armenian", "More with Armenians"), NA),
  o_ip    = ifelse(title == "Israel-Hamas", NA, ip7(`Pre-Treatment 6a`)))

ITEMS <- tribble(
  ~survey,            ~var,      ~item,
  "Israel-Hamas",     "o_ukr",   "Prior: Russia-Ukraine",
  "Nagorno-Karabakh", "o_sudan", "Prior: Sudan (SAF vs RSF)",
  "Nagorno-Karabakh", "o_ip",    "Prior: Israel-Palestine",
  "Nagorno-Karabakh", "o_ukr",   "Prior: Russia-Ukraine",
  "Sudan",            "o_nk",    "Prior: Nagorno-Karabakh",
  "Sudan",            "o_ip",    "Prior: Israel-Palestine",
  "Sudan",            "o_ukr",   "Prior: Russia-Ukraine")
LEVELS <- list(
  o_ukr = c("More with Ukrainians", "More with Russians", "Both equally", "Neither", "Don't know"),
  o_sudan = c("More with SAF", "More with RSF", "Both equally", "Neither", "Don't know"),
  o_nk = c("More with Azerbaijanis", "More with Armenians", "Both equally", "Neither", "Don't know"),
  o_ip = c("Entirely Israelis", "Mostly Israelis", "Equally", "Mostly Palestinians", "Entirely Palestinians", "Neither", "Not sure"))

wald <- function(m) {
  k <- grep("^side_f.*:cat", names(coef(m))); k <- k[!is.na(coef(m)[k])]
  b <- coef(m)[k]; V <- vcov(m)[k, k, drop = FALSE]
  ok <- is.finite(diag(V)) & diag(V) > 0; b <- b[ok]; V <- V[ok, ok, drop = FALSE]
  s <- as.numeric(t(b) %*% MASS::ginv(V) %*% b); r <- qr(V)$rank
  c(stat = s, df = r, p = pchisq(s, r, lower.tail = FALSE))
}

within_eff <- list(); inter <- list()
for (i in seq_len(nrow(ITEMS))) {
  it <- ITEMS[i, ]
  x <- d %>% filter(title == it$survey, !is.na(.data[[it$var]]), !is.na(symp_A)) %>%
    mutate(cat = factor(.data[[it$var]], LEVELS[[it$var]]))
  cnt <- x %>% count(cat, side) %>% pivot_wider(names_from = side, values_from = n, values_fill = 0)
  # (2) within-category effects
  within_eff[[i]] <- map_dfr(levels(droplevels(x$cat)), function(l) {
    y <- x %>% filter(cat == l); nc <- sum(y$side == "Control")
    if (nc < 2 || n_distinct(y$side) < 3) return(tibble(cat = l, side = c("A", "B"), est = NA, lo = NA, hi = NA, p = NA, n = nrow(y), n_ctrl = nc))
    m <- lm_robust(symp_A ~ side_f, data = y, se_type = "HC2"); ci <- confint(m); k <- grep("^side_f", names(coef(m)))
    tibble(cat = l, side = sub("side_f", "", names(coef(m))[k]), est = coef(m)[k], lo = ci[k, 1], hi = ci[k, 2],
           p = m$p.value[k], n = nrow(y), n_ctrl = nc, ctrl_mean = mean(y$symp_A[y$side == "Control"]))
  }) %>% mutate(survey = it$survey, var = it$var, item = it$item, small = n_ctrl < MIN_CTRL)
  # (1) interaction test on categories with enough control respondents
  keep <- cnt$cat[cnt$Control >= MIN_CTRL]
  xt <- x %>% filter(cat %in% keep) %>% mutate(cat = droplevels(cat))
  w <- if (nlevels(xt$cat) >= 2) wald(lm_robust(symp_A ~ side_f * cat, data = xt, se_type = "HC2")) else c(stat = NA, df = 0, p = NA)
  inter[[i]] <- tibble(survey = it$survey, var = it$var, item = it$item, chi2 = w[["stat"]], df = w[["df"]], p = w[["p"]],
                       n = nrow(xt), categories_tested = paste(keep, collapse = " | "),
                       categories_dropped = paste(setdiff(levels(droplevels(x$cat)), keep), collapse = " | "))
}
within_eff <- bind_rows(within_eff) %>% mutate(stars = case_when(p < .01 ~ "**", p < .05 ~ "*", p < .10 ~ "+", TRUE ~ ""))
inter <- bind_rows(inter) %>% mutate(q_bh = p.adjust(p, "BH"))
write.csv(within_eff, "other_conflict_priors.csv", row.names = FALSE)
write.csv(inter, "other_conflict_interactions.csv", row.names = FALSE)
cat("saved other_conflict_priors.csv, other_conflict_interactions.csv\n\nInteraction tests (Q5 sympathy):\n")
print(inter %>% transmute(survey, item, chi2 = round(chi2, 1), df, p = signif(p, 3), q = signif(q_bh, 3), n, dropped = categories_dropped) %>%
        as.data.frame(), row.names = FALSE)
cat("\nWithin-category effects on Q5 (1-5):\n")
within_eff %>% transmute(survey, item = sub("Prior: ", "", item), cat, n, n_ctrl, side, eff = ifelse(is.na(est), "-", sprintf("%+.2f%s", est, stars))) %>%
  pivot_wider(names_from = side, values_from = eff) %>% as.data.frame() %>% print(row.names = FALSE)

# ---------------------------------------------------------------- figure
lab_int <- inter %>% transmute(survey, item, note = ifelse(is.na(p), "interaction: not testable",
                               sprintf("interaction p = %s, q = %s", format.pval(p, digits = 2, eps = .001), format.pval(q_bh, digits = 2, eps = .001))))
pd <- within_eff %>% filter(!is.na(est)) %>%
  left_join(lab_int, by = c("survey", "item")) %>%
  mutate(item_order = match(paste(survey, var), paste(ITEMS$survey, ITEMS$var)),
         cat_order = map2_int(var, cat, ~ match(.y, LEVELS[[.x]])),
         survey = factor(survey, surveys$title),
         survey_head = c("Israel-Hamas" = "ISRAEL-HAMAS SURVEY (A = Israel, B = Palestine)",
                         "Nagorno-Karabakh" = "NAGORNO-KARABAKH SURVEY (A = Azerbaijan, B = Armenia)",
                         "Sudan" = "SUDAN SURVEY (A = SAF, B = RSF)")[as.character(survey)],
         panel_txt = paste0(survey_head, "\n", item, "  |  ", note),
         panel = factor(panel_txt, unique(panel_txt[order(item_order)])),
         cat_lab = paste0(cat, " (n=", n, "; ctrl ", n_ctrl, ")"),
         key_txt = paste(panel_txt, cat_lab, sep = "||"),
         key = factor(key_txt, rev(unique(key_txt[order(item_order, cat_order)]))),
         side_lab = ifelse(side == "A", "Post about Israel / Azerbaijan / SAF suffering", "Post about Palestine / Armenia / RSF suffering"),
         alpha = ifelse(small, 0.35, 1))
g <- ggplot(pd, aes(est, key, colour = side_lab, alpha = alpha)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.35) +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.6), size = 0.25, linewidth = 0.55) +
  facet_wrap(~panel, ncol = 2, scales = "free_y") +
  scale_y_discrete(labels = function(x) sub("^.*\\|\\|", "", x)) +
  scale_alpha_identity() +
  coord_cartesian(xlim = c(-2, 2)) +
  scale_colour_manual(values = c("Post about Israel / Azerbaijan / SAF suffering" = "#2B6CB0",
                                 "Post about Palestine / Armenia / RSF suffering" = "#DD6B20"), name = NULL) +
  labs(x = "Effect on Q5 sympathy toward side A of THIS conflict (1-5 scale; vs. control within the category; 95% CI)", y = NULL,
       title = "Do sympathies in OTHER wars change how the post moves sympathy in this war?",
       subtitle = paste0("Each panel: one survey x one other-conflict prior item (asked before the post). Faded = fewer than ", MIN_CTRL,
                         " control respondents in that category (left out of the interaction test).\n",
                         "Panels in order: Israel-Hamas (Ukraine), Nagorno-Karabakh (Sudan, Israel-Palestine, Ukraine), Sudan (Nagorno-Karabakh, Israel-Palestine, Ukraine).")) +
  theme_minimal(base_size = 9.5) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), legend.position = "bottom",
        strip.text = element_text(face = "bold", hjust = 0, size = 9.5, lineheight = 1.15), plot.title.position = "plot", panel.spacing = unit(1.1, "lines"))
ggsave("plots/OTHER_PRIORS_sympathy.png", g, width = 14, height = 15, dpi = 200, bg = "white")
cat("saved plots/OTHER_PRIORS_sympathy.png\n")
