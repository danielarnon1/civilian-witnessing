# Shared data preparation for all Claude Reanalysis scripts. Source it from this folder: source("00_prepare.R")
# Reads ../Data/{ih,nk,sudan}.csv, keeps Finished + consent ("Yes, I am"), and builds harmonized variables.
#
# Arms: side A = Israel / Azerbaijan / SAF ; side B = Palestine / Armenia / RSF (condition X = suffering OF side X).
# Format: Witness = civilian witness (relative's post, "_CW"); News = news-outlet personal story ("_Personal");
#         Stats = statistics ("_Statistics").
#
# Response-distribution codes (used by 03_plots_by_arm.R):
#   q1 1-5 never seen -> few times/day | q2 1-4 not likely -> very likely
#   q5 1 entirely A .. 5 entirely B, 6 neither, 7 not sure | q6 1 justified, 2 gone too far, 3 DK
#   q7 1 let A pursue, 2 diplomatic pressure, 3 reduce aid, 4 DK | q8 1 too little, 2 right, 3 too much
# Analysis outcomes (used by 04_treatment_effects.R); higher = more favorable to side A unless noted:
#   symp_A 1-5 (neither / not sure -> NA) | justified 0/1 (DK -> NA) | dk_justified 0/1
#   us_lenient 1 reduce aid, 2 pressure, 3 let A pursue (DK -> NA) | dk_us 0/1
#   attn_little 0/1 "too little attention" to this conflict
#   ceasefire, humanit, milaid_A 1-5 importance (Q10-Q12)

suppressMessages(library(tidyverse))

surveys <- tibble(
  survey = c("ih", "nk", "sudan"),
  title  = c("Israel-Hamas", "Nagorno-Karabakh", "Sudan"),
  A_cond = c("Israel", "Azerbaijan", "SAF"),     B_cond = c("Palestine", "Armenia", "RSF"),
  A_lab  = c("Israel", "Azerbaijan", "SAF"),     B_lab  = c("Palestine", "Armenia", "RSF")
)
fmt_key <- c(CW = "Witness", Personal = "News", Statistics = "Stats")

raw <- pmap_dfr(surveys, function(survey, title, A_cond, B_cond, A_lab, B_lab) {
  x <- read.csv(file.path("..", "Data", paste0(survey, ".csv")), check.names = FALSE)
  names(x) <- sub("^[*]", "", names(x))
  x <- x[x$Finished %in% c(TRUE, "TRUE", "True") & x[["Consent 2"]] == "Yes, I am", ]
  side <- case_when(x$condition == "Control" ~ "Control",
                    grepl(A_cond, x$condition) ~ "A", grepl(B_cond, x$condition) ~ "B")
  fmt  <- fmt_key[str_extract(x$condition, "(CW|Personal|Statistics)$")]
  lab  <- ifelse(side == "Control", "Control",
                 paste0(ifelse(side == "A", A_lab, B_lab), "\n", fmt))
  x %>% mutate(survey = survey, title = title, side = side, fmt = unname(fmt), arm = lab)
})

# Arm order within each survey: Control, A Witness/News/Stats, B Witness/News/Stats
arm_levels <- surveys %>%
  pmap(function(A_lab, B_lab, ...) c("Control",
       paste0(A_lab, "\n", c("Witness", "News", "Stats")),
       paste0(B_lab, "\n", c("Witness", "News", "Stats")))) %>% unlist() %>% unique()

A_word <- c("Israel-Hamas" = "Israeli", "Nagorno-Karabakh" = "Azerbaijani", "Sudan" = "Sudanese Armed Forces")
imp5 <- c("Not at all important" = 1, "Slightly important" = 2, "Moderately important" = 3,
          "Very important" = 4, "Extremely important" = 5)

dd_all <- raw %>%
  mutate(
    q1 = unname(c("I have never seen a post like this before" = 1, "Rarely, if ever" = 2,
                  "A few times per week" = 3, "At least once per day" = 4,
                  "At least a few times per day" = 5)[`Post-Treatment 1`]),
    q2 = unname(c("Not likely at all" = 1, "Not very likely" = 2, "Somewhat likely" = 3,
                  "Very likely" = 4)[`Post-Treatment 2`]),
    q5_raw = `Post-Treatment 5`, aw = unname(A_word[title]),
    q5 = case_when(str_starts(q5_raw, paste0("Entirely with the ", aw)) ~ 1,
                   str_starts(q5_raw, paste0("Mostly with the ", aw))   ~ 2,
                   str_starts(q5_raw, "Equally")                        ~ 3,
                   str_starts(q5_raw, "Mostly with the")                ~ 4,
                   str_starts(q5_raw, "Entirely with the")              ~ 5,
                   q5_raw == "With neither of them"                     ~ 6,
                   q5_raw == "Not sure"                                 ~ 7),
    q6 = case_when(grepl("defending", `Post-Treatment 6`)    ~ 1,
                   grepl("gone too far", `Post-Treatment 6`) ~ 2,
                   grepl("^Don", `Post-Treatment 6`)         ~ 3),
    q7 = case_when(grepl("^The U.S. should let", `Post-Treatment 7`) ~ 1,
                   grepl("diplomatic pressure", `Post-Treatment 7`)  ~ 2,
                   grepl("reduce military", `Post-Treatment 7`)      ~ 3,
                   grepl("^Don", `Post-Treatment 7`)                 ~ 4),
    q8_raw = case_when(title == "Israel-Hamas"     ~ `Post-Treatment 8_2`,
                       title == "Nagorno-Karabakh" ~ `Post-Treatment 8_5`,
                       title == "Sudan"            ~ `Post-Treatment 8_4`),
    q8 = unname(c("Too little attention" = 1, "Right amount of attention" = 2,
                  "Too much attention" = 3)[q8_raw]),
    # ---- analysis outcomes
    symp_A       = ifelse(q5 <= 5, 6 - q5, NA),
    justified    = case_when(q6 == 1 ~ 1, q6 == 2 ~ 0),
    dk_justified = as.numeric(q6 == 3),
    us_lenient   = case_when(q7 == 3 ~ 1, q7 == 2 ~ 2, q7 == 1 ~ 3),
    dk_us        = as.numeric(q7 == 4),
    attn_little  = as.numeric(q8 == 1),
    ceasefire    = unname(imp5[`Post-Treatment 10`]),
    humanit      = unname(imp5[`Post-Treatment 11`]),
    milaid_A     = unname(imp5[`Post-Treatment 12`])
  )

# ======================================================================================
# Demographic / pre-treatment moderators (factors) and numeric controls
# Moderator levels with fewer than MIN_LEVEL_N respondents in a survey are set to NA for that survey
# (after merging small race / religion groups into "Other").
MIN_LEVEL_N <- 60
own_sym_col    <- c("Israel-Hamas" = "Pre-Treatment 5",   "Nagorno-Karabakh" = "Pre-Treatment 5a", "Sudan" = "Pre-Treatment 5b")
own_follow_col <- c("Israel-Hamas" = "Pre-Treatment 2_2", "Nagorno-Karabakh" = "Pre-Treatment 2_6", "Sudan" = "Pre-Treatment 2_4")
pick_col <- function(df, map) {
  out <- rep(NA_character_, nrow(df))
  for (t in names(map)) { i <- df$title == t; out[i] <- df[[map[[t]]]][i] }
  out
}
ord <- function(x, lv) unname(setNames(seq_along(lv), lv)[x])
blank_na <- function(x) { x[!is.na(x) & trimws(x) == ""] <- NA; x }

dd_all <- dd_all %>%
  mutate(across(c(starts_with("Demographics"), starts_with("Pre-Treatment"), starts_with("Post-Treatment 22")), blank_na))
dd_all$own_sym    <- pick_col(dd_all, own_sym_col)
dd_all$own_follow <- pick_col(dd_all, own_follow_col)

dd_all <- dd_all %>%
  mutate(
    year_resp  = 2000 + as.numeric(sub("^\\d+/\\d+/(\\d+) .*$", "\\1", RecordedDate)),   # RecordedDate is m/d/yy hh:mm
    age        = year_resp - suppressWarnings(as.numeric(`Demographics 6`)),
    age        = ifelse(age >= 18 & age <= 100, age, NA),
    # ---- numeric controls
    c_age      = age,
    c_female   = case_when(`Demographics 7` == "Female" ~ 1, `Demographics 7` == "Male" ~ 0),
    c_white    = ifelse(is.na(`Demographics 8`), NA, as.numeric(`Demographics 8` == "White, Non-Hispanic")),
    c_college  = ifelse(is.na(`Demographics 9`), NA,
                        as.numeric(`Demographics 9` %in% c("4-year college degree", "Professional degree", "Doctoral degree"))),
    c_income   = ord(`Demographics 10`, c("$24,999 or less", "$25,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999",
                                          "$100,000 or $149,999", "$150,000 to $199,999", "$200,000 to $249,999", "$250,000 or more")),
    c_party    = ord(gsub("  ", " ", `Demographics 2`), c("Strong Democrat", "Democrat", "Independent, leaning to Democrat", "Independent",
                                                         "Independent, leaning to Republican", "Republican", "Strong Republican")),
    c_ideol    = ord(`Demographics 1`, c("Extremely Liberal", "Liberal", "Slightly Liberal", "Moderate, middle of the road",
                                         "Slightly Conservative", "Conservative", "Extremely Conservative")),
    c_attend   = ord(`Demographics 14`, c("Never", "A few times a year", "Once or twice a month", "Almost every week", "Every week")),
    c_socmed   = ord(`Pre-Treatment 7a`, c("I do not have social media", "Rarely on social media", "A few hours per week",
                                           "About an hour per day", "Multiple hours per day")),
    c_follow   = ord(own_follow, c("Not closely at all", "Not too closely", "Somewhat closely", "Very closely", "Extremely closely")),
    # ---- moderators (factors)
    m_party  = case_when(c_party <= 3 ~ "Democrat (incl. leaners)", c_party == 4 ~ "Independent", c_party >= 5 ~ "Republican (incl. leaners)"),
    m_ideol  = case_when(c_ideol <= 3 ~ "Liberal", c_ideol == 4 ~ "Moderate", c_ideol >= 5 ~ "Conservative"),
    m_vote   = case_when(`Post-Treatment 22a` == "Kamala Harris" ~ "Voted Harris", `Post-Treatment 22a` == "Donald Trump" ~ "Voted Trump",
                         `Post-Treatment 22` == "No" | `Post-Treatment 22a` == "Other" ~ "Other / did not vote"),
    m_age    = case_when(age < 35 ~ "18-34", age < 55 ~ "35-54", age >= 55 ~ "55+"),
    m_gender = case_when(c_female == 1 ~ "Female", c_female == 0 ~ "Male"),
    m_race   = case_when(`Demographics 8` == "White, Non-Hispanic" ~ "White", `Demographics 8` == "Black" ~ "Black",
                         `Demographics 8` == "Hispanic/Latino" ~ "Hispanic", `Demographics 8` == "Asian" ~ "Asian",
                         !is.na(`Demographics 8`) ~ "Other"),
    m_educ   = case_when(c_college == 1 ~ "4-year degree or more", c_college == 0 ~ "No 4-year degree"),
    m_income = case_when(c_income <= 2 ~ "Under $50k", c_income <= 4 ~ "$50k-$100k", c_income >= 5 ~ "$100k+"),
    m_relig  = case_when(`Demographics 12` == "Evangelical Protestant Christian" ~ "Evangelical",
                         `Demographics 12` %in% c("Protestant Christian (Non-Evangelical)", "Other Christian") ~ "Other Christian",
                         `Demographics 12` == "Catholic" ~ "Catholic", `Demographics 12` == "Jewish" ~ "Jewish",
                         `Demographics 12` %in% c("Athiest", "No formal religious affiliation") ~ "None / atheist",
                         !is.na(`Demographics 12`) ~ "Other religion"),
    m_attend = case_when(c_attend >= 4 ~ "Weekly or almost", c_attend >= 2 ~ "Monthly to yearly", c_attend == 1 ~ "Never"),
    m_mil    = case_when(`Demographics 11` == "Yes" ~ "Served", `Demographics 11` == "No" ~ "Never served"),
    m_socmed = case_when(c_socmed == 5 ~ "Multiple hours a day", c_socmed == 4 ~ "About an hour a day", c_socmed <= 3 ~ "A few hours a week or less"),
    # two levels: few NK / Sudan respondents followed their conflict "very" or "extremely" closely
    m_follow = case_when(c_follow >= 3 ~ "At least somewhat closely", c_follow <= 2 ~ "Not too / not at all"),
    m_prior  = case_when(own_sym %in% c("More with the Israelis", "More with the Azerbaijanis/Azeris", "Sudanese Armed Forces/SAF (government)") ~ "Prior side A",
                         own_sym %in% c("More with the Palestinians", "More with the Armenians", "Rapid Support Forces/RSF (rebels)") ~ "Prior side B",
                         own_sym == "Both equally" ~ "Prior both", own_sym == "Neither" ~ "Prior neither",
                         own_sym == "I don’t know" | own_sym == "I don't know" ~ "Prior unsure")
  )

# Small Jewish / Asian groups -> "Other", then drop any remaining level under MIN_LEVEL_N (per survey)
dd_all <- dd_all %>% group_by(title) %>%
  mutate(m_relig = ifelse(m_relig == "Jewish" & sum(m_relig == "Jewish", na.rm = TRUE) < MIN_LEVEL_N, "Other religion", m_relig),
         m_race  = ifelse(m_race == "Asian" & sum(m_race == "Asian", na.rm = TRUE) < MIN_LEVEL_N, "Other", m_race)) %>%
  mutate(across(starts_with("m_"), function(x) { tb <- table(x); ifelse(x %in% names(tb)[tb >= MIN_LEVEL_N], x, NA) })) %>%
  ungroup()

MODERATORS <- tribble(
  ~mod,       ~label,                                   ~levels,
  "m_party",  "Party ID",                               list(c("Democrat (incl. leaners)", "Independent", "Republican (incl. leaners)")),
  "m_ideol",  "Ideology",                               list(c("Liberal", "Moderate", "Conservative")),
  "m_vote",   "2024 presidential vote",                 list(c("Voted Harris", "Voted Trump", "Other / did not vote")),
  "m_age",    "Age",                                    list(c("18-34", "35-54", "55+")),
  "m_gender", "Gender",                                 list(c("Female", "Male")),
  "m_race",   "Race / ethnicity",                       list(c("White", "Black", "Hispanic", "Asian", "Other")),
  "m_educ",   "Education",                              list(c("No 4-year degree", "4-year degree or more")),
  "m_income", "Household income",                       list(c("Under $50k", "$50k-$100k", "$100k+")),
  "m_relig",  "Religion",                               list(c("Evangelical", "Other Christian", "Catholic", "Jewish", "None / atheist", "Other religion")),
  "m_attend", "Religious attendance",                   list(c("Weekly or almost", "Monthly to yearly", "Never")),
  "m_mil",    "Military service",                       list(c("Served", "Never served")),
  "m_socmed", "Social media use",                       list(c("Multiple hours a day", "About an hour a day", "A few hours a week or less")),
  "m_follow", "Followed this conflict (pre-treatment)", list(c("At least somewhat closely", "Not too / not at all")),
  "m_prior",  "Prior sympathy in this conflict",        list(c("Prior side A", "Prior side B", "Prior both", "Prior neither", "Prior unsure"))
)
MODERATORS$levels <- lapply(MODERATORS$levels, unlist)   # tribble wraps each cell in a list
# Controls for the "with controls" models (prior sympathy enters as factor m_prior, NA as its own level)
CONTROLS <- c("c_age", "c_female", "c_white", "c_college", "c_income", "c_party", "c_ideol", "c_attend", "c_socmed", "c_follow")

# ======================================================================================
# Pre-treatment moderators (Pr1-Pr7). Pr2 own-conflict attention (m_follow), Pr5/6 own-conflict prior
# sympathy (m_prior) and Pr7a social media time (m_socmed) are defined above.
# Pr2 item order (from the cleaning scripts): 2_1 election, 2_2 Israel-Hamas, 2_3 Ukraine, 2_4 Sudan,
# 2_5 Myanmar, 2_6 Nagorno-Karabakh (NK / Sudan surveys only).
foreign_rx <- regex(paste0("foreign|war\\b|wars\\b|israel|gaza|palestin|hamas|ukrain|russia|putin|china|iran|terror|",
                           "military|genocide|sudan|armenia|azerbaij|international|world|global|nato|middle east|overseas|abroad"),
                    ignore_case = TRUE)
follow5 <- c("Not closely at all", "Not too closely", "Somewhat closely", "Very closely", "Extremely closely")
agree3 <- function(x) case_when(grepl("Agree$", x) & !grepl("Neither|Disagree", x) ~ "Agree",
                                grepl("Disagree$", x) & !grepl("Neither", x) ~ "Disagree",
                                grepl("^Neither", x) ~ "Neither agree nor disagree")
platform <- function(p) ifelse(is.na(dd_all[["Pre-Treatment 7b"]]), NA,
                               ifelse(grepl(p, dd_all[["Pre-Treatment 7b"]], fixed = TRUE), "Uses it", "Does not use it"))

foreign_items <- c("Pre-Treatment 2_2", "Pre-Treatment 2_3", "Pre-Treatment 2_4", "Pre-Treatment 2_5", "Pre-Treatment 2_6")
fol_mat <- sapply(foreign_items, function(cc) if (cc %in% names(dd_all)) ord(blank_na(dd_all[[cc]]), follow5) else rep(NA, nrow(dd_all)))
dd_all$c_news_foreign <- rowMeans(fol_mat, na.rm = TRUE)
dd_all$c_news_foreign[is.nan(dd_all$c_news_foreign)] <- NA

ukr_sym <- ifelse(dd_all$title == "Israel-Hamas", dd_all[["Pre-Treatment 6"]], dd_all[["Pre-Treatment 6b"]])
ihw_sym <- ifelse(dd_all$title == "Israel-Hamas", NA, dd_all[["Pre-Treatment 6a"]])

dd_all <- dd_all %>%
  mutate(
    m_pr1_foreign = case_when(is.na(`Pre-Treatment 1`) ~ NA_character_,
                              str_detect(`Pre-Treatment 1`, foreign_rx) ~ "Mentions foreign affairs / war",
                              TRUE ~ "Does not mention"),
    m_news_foreign = NA_character_,
    m_follow_elec = case_when(ord(`Pre-Treatment 2_1`, follow5) >= 4 ~ "Very / extremely closely",
                              ord(`Pre-Treatment 2_1`, follow5) <= 3 ~ "Somewhat or less"),
    m_greatpow = agree3(`Pre-Treatment 3`),
    m_milforce = agree3(`Pre-Treatment 4`),
    m_prior_ukr = case_when(ukr_sym == "More with the Ukrainians" ~ "More with Ukrainians",
                            !is.na(ukr_sym) ~ "Russians / both / neither / unsure"),
    m_prior_ihw = case_when(grepl("with the Israeli people but", ihw_sym) | grepl("^Entirely with the Israeli", ihw_sym) ~ "Israelis (mostly / entirely)",
                            grepl("with the Palestinian people but", ihw_sym) | grepl("^Entirely with the Palestinian", ihw_sym) ~ "Palestinians (mostly / entirely)",
                            grepl("^Equally", ihw_sym) ~ "Equally with both",
                            !is.na(ihw_sym) ~ "Neither / not sure"),
    m_tiktok    = platform("TikTok"),
    m_instagram = platform("Instagram"),
    m_x         = platform("Twitter/X"),
    m_facebook  = platform("Facebook")
  ) %>%
  group_by(title) %>%
  # foreign-news attention: above vs at-or-below the survey median of the average across foreign-conflict items
  mutate(m_news_foreign = case_when(c_news_foreign > median(c_news_foreign, na.rm = TRUE) ~ "Above survey median",
                                    !is.na(c_news_foreign) ~ "At or below median")) %>%
  mutate(across(c(m_pr1_foreign, m_news_foreign, m_follow_elec, m_greatpow, m_milforce, m_prior_ukr, m_prior_ihw,
                  m_tiktok, m_instagram, m_x, m_facebook),
                function(x) { tb <- table(x); ifelse(x %in% names(tb)[tb >= MIN_LEVEL_N], x, NA) })) %>%
  ungroup()

MODERATORS$group <- ifelse(MODERATORS$mod %in% c("m_socmed", "m_follow", "m_prior"), "Pre-treatment", "Demographics")
MODERATORS$item  <- c(m_party = "D2", m_ideol = "D1", m_vote = "Q22", m_age = "D6", m_gender = "D7", m_race = "D8", m_educ = "D9",
                      m_income = "D10", m_relig = "D12", m_attend = "D14", m_mil = "D11", m_socmed = "Pr7a",
                      m_follow = "Pr2", m_prior = "Pr5/6")[MODERATORS$mod]
PRE_MODS <- tribble(
  ~mod,             ~label,                                        ~levels,                                                                                  ~item,
  "m_pr1_foreign",  "Most important problem mentions foreign affairs", list(c("Mentions foreign affairs / war", "Does not mention")),                         "Pr1",
  "m_news_foreign", "Attention to foreign conflicts (index)",       list(c("Above survey median", "At or below median")),                                   "Pr2",
  "m_follow_elec",  "Followed the 2024 election",                   list(c("Very / extremely closely", "Somewhat or less")),                                "Pr2",
  "m_greatpow",     "Great powers should play an active role",      list(c("Agree", "Neither agree nor disagree", "Disagree")),                             "Pr3",
  "m_milforce",     "Military force has a role in world affairs",   list(c("Agree", "Neither agree nor disagree", "Disagree")),                             "Pr4",
  "m_prior_ukr",    "Prior sympathy: Russia-Ukraine",               list(c("More with Ukrainians", "Russians / both / neither / unsure")),                  "Pr6",
  "m_prior_ihw",    "Prior sympathy: Israel-Palestine",             list(c("Israelis (mostly / entirely)", "Palestinians (mostly / entirely)", "Equally with both", "Neither / not sure")), "Pr6a",
  "m_tiktok",       "Uses TikTok",                                   list(c("Uses it", "Does not use it")),                                                  "Pr7b",
  "m_instagram",    "Uses Instagram",                                list(c("Uses it", "Does not use it")),                                                  "Pr7b",
  "m_x",            "Uses Twitter/X",                                list(c("Uses it", "Does not use it")),                                                  "Pr7b",
  "m_facebook",     "Uses Facebook",                                 list(c("Uses it", "Does not use it")),                                                  "Pr7b"
) %>% mutate(levels = lapply(levels, unlist), group = "Pre-treatment")
MODERATORS <- bind_rows(MODERATORS, PRE_MODS)
# display order: demographics first, then pre-treatment items in questionnaire order
MODERATORS <- MODERATORS %>%
  arrange(group, factor(item, c("D1", "D2", "Q22", "D6", "D7", "D8", "D9", "D10", "D12", "D14", "D11",
                                "Pr1", "Pr2", "Pr3", "Pr4", "Pr5/6", "Pr6", "Pr6a", "Pr7a", "Pr7b")))
