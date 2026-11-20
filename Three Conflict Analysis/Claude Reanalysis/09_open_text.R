# Open-ended answers: Q3 "What are the 3 words that most came into your mind when you read this social media story?"
# Asked only after the post (treated respondents; no Control). Simple descriptive representations:
#   - word clouds by survey and by side of the post
#   - most common words by survey
#   - words that most distinguish posts about one side from the other
# Words are lower-cased, stop words removed, and stemmed for counting (sad / sadness / saddened count together);
# each stem is shown as its most common surface form. A word is counted once per respondent.
# Outputs: plots/TEXT_*.png, open_text_words.csv (read by 06_dashboard_data.R)

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
