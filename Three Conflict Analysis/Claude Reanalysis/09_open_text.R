# Open-ended answers: Q3 "What are the 3 words that most came into your mind when you read this social media story?"
# Asked only after the post (treated respondents; no Control). Simple descriptive representations:
#   - word clouds by survey and by side of the post
#   - most common words by survey
#   - words that most distinguish posts about one side from the other
# Words are lower-cased, stop words removed, and stemmed for counting (sad / sadness / saddened count together);
# each stem is shown as its most common surface form. A word is counted once per respondent.
# Outputs: plots/TEXT_*.png, open_text_words.csv and genocide_mentions.csv (both read by 06_dashboard_data.R)

source("00_prepare.R")
suppressMessages({ library(tidytext); library(SnowballC); library(ggwordcloud) })
set.seed(20261120)

EXTRA_STOPS <- c("im", "ive", "id", "dont", "its", "it", "thats", "theyre", "lot", "really", "just", "like",
                 "feel", "feeling", "feels", "makes", "made", "post", "story", "words", "word", "three", "na", "n", "a")

docs <- dd_all %>%
  mutate(doc_id = paste0(survey, "_", row_number()), text = blank_na(`Post-Treatment 3`)) %>%
  filter(side != "Control", !is.na(text)) %>%
  select(doc_id, survey, title, side, fmt, arm, text)

tokens <- docs %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_replace_all(word, "[^a-z]", "")) %>%
  filter(nchar(word) > 1, !word %in% stop_words$word, !word %in% EXTRA_STOPS) %>%
  mutate(stem = wordStem(word, "en"))

stem_lab <- tokens %>% count(stem, word) %>% group_by(stem) %>% slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>% select(stem, label = word)

# ---------------------------------------------------------------- word counts (respondents using each word)
count_words <- function(grp_docs, grp_name) {
  tokens %>% semi_join(grp_docs, by = "doc_id") %>% distinct(doc_id, title, stem) %>%
    count(title, stem) %>%
    left_join(grp_docs %>% count(title, name = "N"), by = "title") %>%
    mutate(group = grp_name, share = n / N) %>% left_join(stem_lab, by = "stem")
}
words <- bind_rows(count_words(docs, "All posts"),
                   count_words(docs %>% filter(side == "A"), "A"),
                   count_words(docs %>% filter(side == "B"), "B"))
words_out <- words %>% group_by(title, group) %>% slice_max(n, n = 80, with_ties = FALSE) %>% ungroup() %>%
  left_join(surveys %>% select(title, survey), by = "title") %>%
  select(survey, title, group, word = label, n, N, share)
write.csv(words_out, "open_text_words.csv", row.names = FALSE)
cat("saved open_text_words.csv\n")

# ---------------------------------------------------------------- word clouds: survey x side
side_lab <- function(title, g) {
  s <- surveys[match(title, surveys$title), ]
  ifelse(g == "A", paste0("Posts about ", s$A_lab), paste0("Posts about ", s$B_lab))
}
wc <- words %>% filter(group != "All posts") %>% group_by(title, group) %>% slice_max(n, n = 45, with_ties = FALSE) %>% ungroup() %>%
  mutate(panel = factor(paste0(title, ": ", side_lab(title, group)),
                        unlist(lapply(surveys$title, function(t) paste0(t, ": ", side_lab(t, c("A", "B")))))))
g0 <- ggplot(wc, aes(label = label, size = share, colour = group)) +
  geom_text_wordcloud_area(rm_outside = TRUE, eccentricity = 0.8) +
  scale_size_area(max_size = 38) +
  scale_colour_manual(values = c(A = "#2B6CB0", B = "#DD6B20"), guide = "none") +
  facet_wrap(~panel, ncol = 2) +
  labs(title = "Q3: the three words that came to mind after the post",
       subtitle = "Word size = share of respondents using the word. Top 45 words per panel.") +
  theme_minimal(base_size = 11) + theme(strip.text = element_text(face = "bold", size = 11), plot.title.position = "plot")
ggsave("plots/TEXT_wordclouds.png", g0, width = 11, height = 10, dpi = 200, bg = "white")
cat("saved plots/TEXT_wordclouds.png\n")

# ---------------------------------------------------------------- most common words by survey
top <- words %>% filter(group == "All posts") %>%
  group_by(title) %>% slice_max(share, n = 20, with_ties = FALSE) %>% ungroup() %>%
  mutate(title = factor(title, surveys$title), label = reorder_within(label, share, title))
g1 <- ggplot(top, aes(share, label)) +
  geom_col(fill = "#4579b6", width = 0.75) +
  geom_text(aes(label = scales::percent(share, accuracy = 1)), hjust = -0.15, size = 3) +
  facet_wrap(~title, scales = "free_y") + scale_y_reordered() +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
  labs(x = "Share of respondents using the word (stemmed: sad / sadness / saddened count together)", y = NULL,
       title = "Q3: the three words respondents used, by survey",
       subtitle = sprintf("Top 20 words. Treated respondents only (N = %s answers).", scales::comma(nrow(docs)))) +
  theme_minimal(base_size = 11) + theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
                                        strip.text = element_text(face = "bold", size = 12), plot.title.position = "plot")
ggsave("plots/TEXT_top_words.png", g1, width = 13, height = 7, dpi = 200, bg = "white")
cat("saved plots/TEXT_top_words.png\n")

# ---------------------------------------------------------------- words that distinguish the two sides
dist_side <- words %>% filter(group != "All posts") %>% select(title, group, stem, label, share, n) %>%
  pivot_wider(names_from = group, values_from = c(share, n), values_fill = 0) %>%
  filter(n_A + n_B >= 15) %>% mutate(diff = share_A - share_B) %>%
  group_by(title) %>% arrange(desc(abs(diff))) %>% slice_head(n = 16) %>% ungroup() %>%
  mutate(lean = ifelse(diff > 0, "A", "B"), title = factor(title, surveys$title), label = reorder_within(label, diff, title))
g2 <- ggplot(dist_side, aes(100 * diff, label, fill = lean)) +
  geom_col(width = 0.7) + geom_vline(xintercept = 0, colour = "grey40") +
  facet_wrap(~title, scales = "free_y") + scale_y_reordered() +
  scale_fill_manual(values = c(A = "#2B6CB0", B = "#DD6B20"), name = NULL,
                    labels = c(A = "More common after Israel / Azerbaijan / SAF posts", B = "More common after Palestine / Armenia / RSF posts")) +
  labs(x = "Difference in share of respondents using the word (percentage points)", y = NULL,
       title = "Q3: words that most distinguish posts about one side from the other",
       subtitle = "The 16 words with the largest difference in each survey (words used by at least 15 respondents).") +
  theme_minimal(base_size = 11) + theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
                                        strip.text = element_text(face = "bold", size = 12), legend.position = "bottom",
                                        plot.title.position = "plot")
ggsave("plots/TEXT_distinctive_by_side.png", g2, width = 13, height = 7, dpi = 200, bg = "white")
cat("saved plots/TEXT_distinctive_by_side.png\n")

# ---------------------------------------------------------------- mentions of "genocide" (incl. "genocidal")
# Share of answers using the word, after posts about each side, by format and pooled groupings; B - A tested with
# HC2 SEs (pooled across surveys: survey fixed effects, SEs clustered by participant). No control: Q3 was asked only
# after the post. Outputs: genocide_mentions.csv (read by 06_dashboard_data.R), plots/TEXT_genocide_mentions.png
suppressMessages(library(estimatr))
gx <- dd_all %>% mutate(txt = blank_na(`Post-Treatment 3`)) %>%
  filter(side != "Control", !is.na(txt)) %>%
  mutate(gen = as.numeric(grepl("genocid", txt, ignore.case = TRUE)),
         cluster_id = ifelse(!is.na(participantId) & participantId != "", participantId, paste0("row", row_number())))
GROUPS <- list("Witness" = "Witness", "News story" = "News", "Statistics" = "Stats",
               "Personal (witness + news)" = c("Witness", "News"), "All posts" = c("Witness", "News", "Stats"))
gen_tab <- map_dfr(c(surveys$title, "Pooled (3 surveys)"), function(t) imap_dfr(GROUPS, function(fm, gname) {
  z <- gx %>% filter(fmt %in% fm); if (t != "Pooled (3 surveys)") z <- z %>% filter(title == t)
  s <- z %>% group_by(side) %>% summarise(n = n(), k = sum(gen), p = mean(gen), .groups = "drop") %>%
    mutate(se = sqrt(p * (1 - p) / n))
  m <- if (t == "Pooled (3 surveys)") lm_robust(gen ~ side + title, data = z, clusters = cluster_id, se_type = "stata")
       else lm_robust(gen ~ side, data = z, se_type = "HC2")
  tibble(survey = t, group = gname,
         n_A = s$n[s$side == "A"], k_A = s$k[s$side == "A"], share_A = s$p[s$side == "A"], se_A = s$se[s$side == "A"],
         n_B = s$n[s$side == "B"], k_B = s$k[s$side == "B"], share_B = s$p[s$side == "B"], se_B = s$se[s$side == "B"],
         diff = coef(m)[["sideB"]], diff_p = m$p.value[["sideB"]])
}))
write.csv(gen_tab, "genocide_mentions.csv", row.names = FALSE)
cat("saved genocide_mentions.csv\n")

gp <- gen_tab %>%
  pivot_longer(c(share_A, share_B), names_to = "side", values_to = "share") %>%
  mutate(side = sub("share_", "", side), se = ifelse(side == "A", se_A, se_B), k = ifelse(side == "A", k_A, k_B),
         n = ifelse(side == "A", n_A, n_B),
         lo = pmax(0, share - 1.96 * se), hi = share + 1.96 * se,
         survey = factor(survey, c(surveys$title, "Pooled (3 surveys)")),
         group = factor(group, rev(names(GROUPS))),
         side_lab = ifelse(side == "A", "After posts about Israel / Azerbaijan / SAF suffering", "After posts about Palestine / Armenia / RSF suffering"))
gd <- gen_tab %>% mutate(survey = factor(survey, levels(gp$survey)), group = factor(group, rev(names(GROUPS))),
                         lab = sprintf("B-A %+.1f%s", 100 * diff, case_when(diff_p < .01 ~ "**", diff_p < .05 ~ "*", diff_p < .10 ~ "+", TRUE ~ "")))
g5 <- ggplot(gp, aes(100 * share, group, colour = side_lab)) +
  geom_pointrange(aes(xmin = 100 * lo, xmax = 100 * hi), position = position_dodge(width = 0.55), size = 0.35, linewidth = 0.7) +
  geom_text(data = gd, aes(x = 16, y = group, label = lab), inherit.aes = FALSE, hjust = 1, size = 2.9, colour = "grey30") +
  facet_wrap(~survey, nrow = 1) +
  scale_x_continuous(limits = c(0, 16), breaks = seq(0, 12, 3)) +
  scale_colour_manual(values = c("After posts about Israel / Azerbaijan / SAF suffering" = "#2B6CB0",
                                 "After posts about Palestine / Armenia / RSF suffering" = "#DD6B20"), name = NULL) +
  labs(x = "% of answers mentioning \"genocide\" (95% CI)", y = NULL,
       title = "Q3 three words: how often respondents wrote \"genocide\", by post format and side",
       subtitle = "Asked only after the post (no control group). Right: difference B - A in percentage points; ** p<.01 * p<.05 + p<.10.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 11), plot.title.position = "plot", panel.spacing.x = unit(1.2, "lines"))
ggsave("plots/TEXT_genocide_mentions.png", g5, width = 15, height = 5.5, dpi = 200, bg = "white")
cat("saved plots/TEXT_genocide_mentions.png\n")
