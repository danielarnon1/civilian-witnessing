
# REQUIRES: Data/sudan.csv
# REQUIRES: Load Packages.R

################# Dependencies #################
source("Load Packages.R")

################# Import Data #################
scw_raw <- c("Data/sudan.csv")
scw_raw <- lapply(scw_raw, read.csv) %>% bind_rows()

################# Data Cleaning #################
scw <- scw_raw %>% dplyr::mutate(scw_raw, id = row_number())
scw <- filter(scw, scw$Finished == TRUE)
scw <- filter(scw, scw$Consent.2 == "Yes, I am")


# Remove Variables
scw_full <- scw
scw <- subset(scw, select = -c(StartDate, EndDate, Status, IPAddress, Progress, Finished, ResponseId, RecipientLastName, 
                             RecipientFirstName, RecipientEmail, ExternalReference, LocationLatitude, LocationLongitude,
                             DistributionChannel, UserLanguage, Q_RecaptchaScore, Consent.1, Consent.2,
                             ASC, DSC, DSCa, ASP, DSP, DSPa, ASS, DSS, DSSa, ARC, DRC, DRCa, ARP, DRP, DRPa, ARS, DRS, DRSa))

# Rename Variables
names(scw)[names(scw) == 'Duration..in.seconds.'] <- 'time'
names(scw)[names(scw) == 'RecordedDate'] <- 'date'

# Pre-Treatment (1-7)
names(scw)[names(scw) == 'Pre.Treatment.1'] <- 'pr1_importantprob'
names(scw)[names(scw) == 'Pre.Treatment.2_1'] <- 'pr2_followtopics_election'
names(scw)[names(scw) == 'Pre.Treatment.2_2'] <- 'pr2_followtopics_ihw'
names(scw)[names(scw) == 'Pre.Treatment.2_3'] <- 'pr2_followtopics_ukraine'
names(scw)[names(scw) == 'Pre.Treatment.2_4'] <- 'pr2_followtopics_sudan'
names(scw)[names(scw) == 'Pre.Treatment.2_5'] <- 'pr2_followtopics_myanmar'
names(scw)[names(scw) == 'Pre.Treatment.2_6'] <- 'pr2_followtopics_nk'
names(scw)[names(scw) == 'Pre.Treatment.3'] <- 'pr3_greatpowers'
names(scw)[names(scw) == 'Pre.Treatment.4'] <- 'pr4_milforce'
names(scw)[names(scw) == 'Pre.Treatment.5a'] <- 'pr5_sympathize_nk'
names(scw)[names(scw) == 'Pre.Treatment.5b'] <- 'pr5_sympathize_sudan'
names(scw)[names(scw) == 'Pre.Treatment.6a'] <- 'pr6_sympathize_ihw'
names(scw)[names(scw) == 'Pre.Treatment.6b'] <- 'pr6_sympathize_ukraine'
names(scw)[names(scw) == 'Pre.Treatment.7a'] <- 'pr7_media_time'
names(scw)[names(scw) == 'Pre.Treatment.7b'] <- 'pr7_media_platform'
names(scw)[names(scw) == 'Pre.Treatment.7b_11_TEXT'] <- 'pr7_media_platform_text'
names(scw)[names(scw) == 'Pre.Treatment.7c'] <- 'pr7_media_mostactive'
names(scw)[names(scw) == 'Pre.Treatment.7c_11_TEXT'] <- 'pr7_media_mostactive_text'

# Immediate Post Treatment (1-4)
names(scw)[names(scw) == 'Post.Treatment.1'] <- 'po1_postfreq'
names(scw)[names(scw) == 'Post.Treatment.2'] <- 'po2_sharepost'
names(scw)[names(scw) == 'Post.Treatment.3'] <- 'po3_3words'
names(scw)[names(scw) == 'Post.Treatment.4_1'] <- 'po4_feeling_afraid'
names(scw)[names(scw) == 'Post.Treatment.4_2'] <- 'po4_feeling_exhausted'
names(scw)[names(scw) == 'Post.Treatment.4_3'] <- 'po4_feeling_angry'
names(scw)[names(scw) == 'Post.Treatment.4_4'] <- 'po4_feeling_sad'

# Post-Treatment - Foreign Policy - Attitudes (5-8)
names(scw)[names(scw) == 'Post.Treatment.5'] <- 'po5_sympathies'
names(scw)[names(scw) == 'X.Post.Treatment.6'] <- 'po6_think'
names(scw)[names(scw) == 'Post.Treatment.7'] <- 'po7_usaction'
names(scw)[names(scw) == 'Post.Treatment.8_1'] <- 'po8_leaderattention_ukraine'
names(scw)[names(scw) == 'Post.Treatment.8_2'] <- 'po8_leaderattention_ihw'
names(scw)[names(scw) == 'Post.Treatment.8_3'] <- 'po8_leaderattention_myanmar'
names(scw)[names(scw) == 'Post.Treatment.8_4'] <- 'po8_leaderattention_sudan'
names(scw)[names(scw) == 'Post.Treatment.8_5'] <- 'po8_leaderattention_nk'

# Post-Treatment - Foreign Policy - Goals (10-12) - skipped 9 for parallelism
names(scw)[names(scw) == 'Post.Treatment.10'] <- 'po10_important_ceasefire'
names(scw)[names(scw) == 'Post.Treatment.11'] <- 'po11_important_humanitarian'
names(scw)[names(scw) == 'Post.Treatment.12'] <- 'po12_important_aidsaf'

# Skipped polarization and distance questions in IHW context

# Post-Treatment - Domestic - Voting (22)
names(scw)[names(scw) == 'Post.Treatment.22'] <- 'po22_vote_yn'
names(scw)[names(scw) == 'Post.Treatment.22a'] <- 'po22_vote_candid'
names(scw)[names(scw) == 'Post.Treatment.22a_3_TEXT'] <- 'po22_vote_candid_text'

# Demographics (1-15)
names(scw)[names(scw) == 'Demographics.1'] <- 'd1_ideology'
names(scw)[names(scw) == 'Demographics.2'] <- 'd2_party'
names(scw)[names(scw) == 'Demographics.3'] <- 'd3_leadersudan'
names(scw)[names(scw) == 'X.Demographics.4'] <- 'd4_capitalsudan'
names(scw)[names(scw) == 'X.Demographics.5'] <- 'd5_sudanesedeaths'
names(scw)[names(scw) == 'Demographics.6'] <- 'd6_age'
names(scw)[names(scw) == 'Demographics.7'] <- 'd7_gender'
names(scw)[names(scw) == 'Demographics.7_4_TEXT'] <- 'd7_gender_text'
names(scw)[names(scw) == 'Demographics.8'] <- 'd8_race'
names(scw)[names(scw) == 'Demographics.8_7_TEXT'] <- 'd8_race_text'
names(scw)[names(scw) == 'Demographics.9'] <- 'd9_educ'
names(scw)[names(scw) == 'Demographics.10'] <- 'd10_income'
names(scw)[names(scw) == 'Demographics.11'] <- 'd11_mil'
names(scw)[names(scw) == 'Demographics.12'] <- 'd12_religion'
names(scw)[names(scw) == 'Demographics.12_10_TEXT'] <- 'd12_religion_text'
names(scw)[names(scw) == 'Demographics.13'] <- 'd13_religion_important'
names(scw)[names(scw) == 'Demographics.14'] <- 'd14_religion_service'

# Reorder Variables
scw <- scw %>% dplyr::select(id, date, time, condition, attentioncheck, donate, donate_amt, everything())

# Categorical Variables
## Condition
scw$personal <- ifelse(scw$condition == "Condition_SAF_CW" |
                        scw$condition == "Condition_SAF_Personal" |
                        scw$condition == "Condition_RSF_CW" |
                        scw$condition == "Condition_RSF_Personal", 1, 0)
scw$personal <- as.numeric(as.character(scw$personal))
scw$stats <- ifelse(scw$condition == "Condition_SAF_Statistics" |
                     scw$condition == "Condition_RSF_Statistics", 1, 0)
scw$stats <- as.numeric(as.character(scw$stats))
scw$control <- ifelse(scw$condition == "Control", 1, 0)
scw$control <- as.numeric(as.character(scw$control))

scw$condition_c <- ifelse(scw$personal == 1, "personal", "")
scw$condition_c <- ifelse(scw$stats == 1, "stats", scw$condition_c)
scw$condition_c <- ifelse(scw$control == 1, "control", scw$condition_c)
scw$condition_c <- relevel(scw$condition_c, ref = "control")

scw$rsf <- ifelse(scw$condition == "Condition_RSF_CW" |
                     scw$condition == "Condition_RSF_Personal" |
                     scw$condition == "Condition_RSF_Statistics", 1, 0)
scw$rsf <- as.numeric(as.character(scw$pales))
scw$saf <- ifelse(scw$condition == "Condition_SAF_CW" |
                  scw$condition == "Condition_SAF_Personal" |
                  scw$condition == "Condition_SAF_Statistics", 1, 0)
scw$saf <- as.numeric(as.character(scw$saf))
scw$condition_ipc <- ifelse(scw$rsf == 1, "rsf", "")
scw$condition_ipc <- ifelse(scw$saf == 1, "saf", scw$condition_ipc)
scw$condition_ipc <- ifelse(scw$control == 1, "control", scw$condition_ipc)
scw$condition_ipc <- as.factor(scw$condition_ipc)

scw$condition_scw <- ifelse(scw$condition == "Condition_SAF_CW" |
                            scw$condition == "Condition_RSF_CW", 1, 0)
scw$condition_scw <- as.numeric(as.character(scw$condition_scw))
scw$condition_personal <- ifelse(scw$condition == "Condition_SAF_Personal" |
                                  scw$condition == "Condition_RSF_Personal", 1, 0)
scw$condition_personal <- as.numeric(as.character(scw$condition_personal))
scw$stats <- ifelse(scw$condition == "Condition_SAF_Statistics" |
                     scw$condition == "Condition_RSF_Statistics", 1, 0)
scw$stats <- as.numeric(as.character(scw$stats))
scw$control <- ifelse(scw$condition == "Control", 1, 0)
scw$control <- as.numeric(as.character(scw$control))

scw$condition_vers <- ifelse(scw$condition_scw == 1, "CW", "")
scw$condition_vers <- ifelse(scw$condition_personal == 1, "Personal", scw$condition_vers)
scw$condition_vers <- ifelse(scw$stats == 1, "Statistics", scw$condition_vers)
scw$condition_vers <- ifelse(scw$control == 1, "Control", scw$condition_vers)
scw$condition_vers <- relevel(scw$condition_vers, ref = "Control")

scw$condition_sep <- ifelse(scw$condition == "Condition_SAF_CW", "SAF_CW", "")
scw$condition_sep <- ifelse(scw$condition == "Condition_RSF_CW", "RSF_CW", scw$condition_sep)
scw$condition_sep <- ifelse(scw$condition == "Condition_SAF_Personal", "SAF_News", scw$condition_sep)
scw$condition_sep <- ifelse(scw$condition == "Condition_RSF_Personal", "RSF_News", scw$condition_sep)
scw$condition_sep <- ifelse(scw$condition == "Condition_SAF_Statistics", "SAF_News", scw$condition_sep)
scw$condition_sep <- ifelse(scw$condition == "Condition_RSF_Statistics", "RSF_News", scw$condition_sep)
scw$condition_sep <- ifelse(scw$condition == "Control", "Control", scw$condition_sep)
scw$condition_sep <- relevel(scw$condition_sep, ref = "Control")

scw$condition_sep2 <- ifelse(scw$condition == "Condition_SAF_CW", "SAF_Personal", "")
scw$condition_sep2 <- ifelse(scw$condition == "Condition_RSF_CW", "RSF_Personal", scw$condition_sep2)
scw$condition_sep2 <- ifelse(scw$condition == "Condition_SAF_Personal", "SAF_Personal", scw$condition_sep2)
scw$condition_sep2 <- ifelse(scw$condition == "Condition_RSF_Personal", "RSF_Personal", scw$condition_sep2)
scw$condition_sep2 <- ifelse(scw$condition == "Condition_SAF_Statistics", "SAF_Statistics", scw$condition_sep2)
scw$condition_sep2 <- ifelse(scw$condition == "Condition_RSF_Statistics", "RSF_Statistics", scw$condition_sep2)
scw$condition_sep2 <- ifelse(scw$condition == "Control", "Control", scw$condition_sep2)
scw$condition_sep2 <- relevel(scw$condition_sep2, ref = "Control")

scw$condition_sep3 <- ifelse(scw$condition == "Condition_SAF_CW", "SAF_Personal", "")
scw$condition_sep3 <- ifelse(scw$condition == "Condition_RSF_CW", "RSF_Personal", scw$condition_sep3)
scw$condition_sep3 <- ifelse(scw$condition == "Condition_SAF_Personal", "SAF_Personal", scw$condition_sep3)
scw$condition_sep3 <- ifelse(scw$condition == "Condition_RSF_Personal", "RSF_Personal", scw$condition_sep3)
scw$condition_sep3 <- ifelse(scw$condition == "Condition_SAF_Statistics", "SAF_Statistics", scw$condition_sep3)
scw$condition_sep3 <- ifelse(scw$condition == "Condition_RSF_Statistics", "RSF_Statistics", scw$condition_sep3)
scw$condition_sep3 <- ifelse(scw$condition == "Control", "Control", scw$condition_sep3)
scw$condition_sep3 <- relevel(scw$condition_sep3, ref = "Control")

## Pre-Treatment
scw$pr2_followtopics_election_n <- recode_factor(scw$pr2_followtopics_election,
                                                "Not closely at all" = 1,
                                                "Not too closely" = 2,
                                                "Somewhat closely" = 3,
                                                "Very closely" = 4,
                                                "Extremely closely" = 5)
scw$pr2_followtopics_election_n <- as.numeric(as.character(scw$pr2_followtopics_election_n))
scw$pr2_followtopics_ihw_n <- recode_factor(scw$pr2_followtopics_ihw,
                                           "Not closely at all" = 1,
                                           "Not too closely" = 2,
                                           "Somewhat closely" = 3,
                                           "Very closely" = 4,
                                           "Extremely closely" = 5)
scw$pr2_followtopics_ihw_n <- as.numeric(as.character(scw$pr2_followtopics_ihw_n))
scw$att <- ifelse(scw$pr2_followtopics_ihw == "Not closely at all" | scw$pr2_followtopics_ihw == "Not too closely", "low_att", "")
scw$att <- ifelse(scw$pr2_followtopics_ihw == "Somewhat closely" | scw$pr2_followtopics_ihw == "Very closely" | scw$pr2_followtopics_ihw == "Extremely closely", "high_att", scw$att)
scw$att_n <- ifelse(scw$att == "high_att", 1, 0)

scw$pr2_followtopics_ukraine_n <- recode_factor(scw$pr2_followtopics_ukraine,
                                               "Not closely at all" = 1,
                                               "Not too closely" = 2,
                                               "Somewhat closely" = 3,
                                               "Very closely" = 4,
                                               "Extremely closely" = 5)
scw$pr2_followtopics_ukraine_n <- as.numeric(as.character(scw$pr2_followtopics_ukraine_n))
scw$pr2_followtopics_sudan_n <- recode_factor(scw$pr2_followtopics_sudan,
                                             "Not closely at all" = 1,
                                             "Not too closely" = 2,
                                             "Somewhat closely" = 3,
                                             "Very closely" = 4,
                                             "Extremely closely" = 5)
scw$pr2_followtopics_sudan_n <- as.numeric(as.character(scw$pr2_followtopics_sudan_n))
scw$pr2_followtopics_myanmar_n <- recode_factor(scw$pr2_followtopics_myanmar,
                                               "Not closely at all" = 1,
                                               "Not too closely" = 2,
                                               "Somewhat closely" = 3,
                                               "Very closely" = 4,
                                               "Extremely closely" = 5)
scw$pr2_followtopics_myanmar_n <- as.numeric(as.character(scw$pr2_followtopics_myanmar_n))

scw$pr3_greatpowers <- as.character(scw$pr3_greatpowers)
scw$pr3_greatpowers <- ifelse(scw$pr3_greatpowers == "Neither Agree Nor DIsagree", "Neither Agree Nor Disagree", scw$pr3_greatpowers)
scw$pr3_greatpowers <- as.factor(scw$pr3_greatpowers)
scw$pr3_greatpowers_n <- recode_factor(scw$pr3_greatpowers,
                                      "Strongly Disagree" = 0,
                                      "Disagree" = 1,
                                      "Neither Agree Nor Disagree" = 2,
                                      "Agree" = 3,
                                      "Strongly Agree" = 4)
scw$pr3_greatpowers_n <- as.numeric(as.character(scw$pr3_greatpowers_n))
scw$pr3_greatpowers <- factor(scw$pr3_greatpowers, levels=c("Strongly Disagree", "Disagree", "Neither Agree Nor Disagree", "Agree", "Strongly Agree", NA))
scw$pr4_milforce <- as.character(scw$pr4_milforce)
scw$pr4_milforce <- ifelse(scw$pr4_milforce == "Neither Agree Nor DIsagree", "Neither Agree Nor Disagree", scw$pr4_milforce)
scw$pr4_milforce <- as.factor(scw$pr4_milforce)
scw$pr4_milforce_n <- recode_factor(scw$pr4_milforce,
                                   "Strongly Disagree" = 0,
                                   "Disagree" = 1,
                                   "Neither Agree Nor Disagree" = 2,
                                   "Agree" = 3,
                                   "Strongly Agree" = 4)
scw$pr4_milforce_n <- as.numeric(as.character(scw$pr4_milforce_n))
scw$pr4_milforce <- factor(scw$pr4_milforce, levels=c("Strongly Disagree", "Disagree", "Neither Agree Nor Disagree", "Agree", "Strongly Agree", NA))

# scw$pr5_sympathize_nk_n <- recode_factor(scw$pr5_sympathize_nk,
#                                          "Neither" = 0,
#                                          "More with the Palestinians" = 1,
#                                          "Both equally" = 2,
#                                          "More with the Israelis" = 3)
# scw$pr5_sympathize_nk_n <- as.numeric(as.character(scw$pr5_sympathize_nk_n))
# scw$pr5_sympathize_nk2 <- recode_factor(scw$pr5_sympathize_nk,
#                                         "More with the Palestinians" = "Pales",
#                                         "Both equally" = "Both",
#                                         "More with the Israelis" = "Is")
# scw$pr5_sympathize_nk2 <- relevel(scw$pr5_sympathize_nk2, ref = "Both")
# scw$pr5_sympathize_nk_n2 <- recode_factor(scw$pr5_sympathize_nk,
#                                           "More with the Palestinians" = 0,
#                                           "Both equally" = 1,
#                                           "More with the Israelis" = 2)
# scw$pr5_sympathize_nk_n2 <- as.numeric(as.character(scw$pr5_sympathize_nk_n2))
# scw$pr5_sympathize_nk_bnt <- as.character(scw$pr5_sympathize_nk)
# scw$pr5_sympathize_nk_bnt <- ifelse(scw$pr5_sympathize_nk == "Neither", "Both_Neither", scw$pr5_sympathize_nk_bnt)
# scw$pr5_sympathize_nk_bnt <- ifelse(scw$pr5_sympathize_nk_bn == "Both equally", "Both_Neither", scw$pr5_sympathize_nk_bnt)
# scw$pr5_sympathize_nk_bnt <- as.factor(scw$pr5_sympathize_nk_bnt)
# 
# scw$pr5_sympathize_sudan_n <- recode_factor(scw$pr5_sympathize_sudan,
#                                          "Neither" = 0,
#                                          "More with the Palestinians" = 1,
#                                          "Both equally" = 2,
#                                          "More with the Israelis" = 3)
# scw$pr5_sympathize_sudan_n <- as.numeric(as.character(scw$pr5_sympathize_sudan_n))
# scw$pr5_sympathize_sudan2 <- recode_factor(scw$pr5_sympathize_sudan,
#                                         "More with the Palestinians" = "Pales",
#                                         "Both equally" = "Both",
#                                         "More with the Israelis" = "Is")
# scw$pr5_sympathize_sudan2 <- relevel(scw$pr5_sympathize_sudan2, ref = "Both")
# scw$pr5_sympathize_sudan_n2 <- recode_factor(scw$pr5_sympathize_sudan,
#                                           "More with the Palestinians" = 0,
#                                           "Both equally" = 1,
#                                           "More with the Israelis" = 2)
# scw$pr5_sympathize_sudan_n2 <- as.numeric(as.character(scw$pr5_sympathize_sudan_n2))
# scw$pr5_sympathize_sudan_bnt <- as.character(scw$pr5_sympathize_sudan)
# scw$pr5_sympathize_sudan_bnt <- ifelse(scw$pr5_sympathize_sudan == "Neither", "Both_Neither", scw$pr5_sympathize_sudan_bnt)
# scw$pr5_sympathize_sudan_bnt <- ifelse(scw$pr5_sympathize_sudan_bn == "Both equally", "Both_Neither", scw$pr5_sympathize_sudan_bnt)
# scw$pr5_sympathize_sudan_bnt <- as.factor(scw$pr5_sympathize_sudan_bnt)


# scw$pr5_sympathize_ihw_otherside_n <- ifelse(scw$condition == "Condition_Palestine_CW" & scw$pr5_sympathize_ihw == "More with the Israelis", 1, 0)
# scw$pr5_sympathize_ihw_otherside_n <- ifelse(scw$condition == "Condition_Palestine_Personal" & scw$pr5_sympathize_ihw == "More with the Israelis", 1, scw$pr5_sympathize_ihw_otherside_n)
# scw$pr5_sympathize_ihw_otherside_n <- ifelse(scw$condition == "Condition_Palestine_Statistics" & scw$pr5_sympathize_ihw == "More with the Israelis", 1, scw$pr5_sympathize_ihw_otherside_n)
# scw$pr5_sympathize_ihw_otherside_n <- ifelse(scw$condition == "Condition_Israel_CW" & scw$pr5_sympathize_ihw == "More with the Palestinians", 1, scw$pr5_sympathize_ihw_otherside_n)
# scw$pr5_sympathize_ihw_otherside_n <- ifelse(scw$condition == "Condition_Israel_Personal" & scw$pr5_sympathize_ihw == "More with the Palestinians", 1, scw$pr5_sympathize_ihw_otherside_n)
# scw$pr5_sympathize_ihw_otherside_n <- ifelse(scw$condition == "Condition_Israel_Statistics" & scw$pr5_sympathize_ihw == "More with the Palestinians", 1, scw$pr5_sympathize_ihw_otherside_n)
# scw$pr5_sympathize_ihw_otherside_n <- as.numeric(as.character(scw$pr5_sympathize_ihw_otherside_n))
# scw$pr5_sympathize_ihw_pales <- as.numeric(as.character(ifelse(scw$pr5_sympathize_ihw == "More with the Palestinians", 1, 0)))
# scw$pr5_sympathize_ihw_pales_f <- as.factor(scw$pr5_sympathize_ihw_pales)
# scw$pr5_sympathize_ihw_is <- as.numeric(as.character(ifelse(scw$pr5_sympathize_ihw == "More with the Israelis", 1, 0)))
# scw$pr5_sympathize_ihw_both <- as.numeric(as.character(ifelse(scw$pr5_sympathize_ihw == "Both equally", 1, 0)))
# scw$pr5_sympathize_ihw_neither <- as.numeric(as.character(ifelse(scw$pr5_sympathize_ihw == "Neither", 1, 0)))
# scw$pr5_sympathize_ihw_bn <- as.numeric(as.character(ifelse(scw$pr5_sympathize_ihw == "Both equally", 1, 0)))
# scw$pr5_sympathize_ihw_bn <- as.numeric(as.character(ifelse(scw$pr5_sympathize_ihw == "Neither", 1, scw$pr5_sympathize_ihw_bn)))
# scw$pr6_sympathize_ukraine_n <- recode_factor(scw$pr6_sympathize_ukraine,
#                                              "Neither" = 0,
#                                              "More with the Russians" = 1,
#                                              "Both equally" = 2,
#                                              "More with the Ukrainians" = 3)
# scw$pr6_sympathize_ukraine_n <- as.numeric(as.character(scw$pr6_sympathize_ukraine_n))

scw$pr7_media_time_n <- recode_factor(scw$pr7_media_time,
                                     "I do not have social media" = 0,
                                     "Rarely on social media" = 1,
                                     "A few hours per week" = 2,
                                     "About an hour per day" = 3,
                                     "Multiple hours per day" = 4)
scw$pr7_media_time_n <- as.numeric(as.character(scw$pr7_media_time_n))
scw$pr7_media_time <- factor(scw$pr7_media_time, levels=c("I do not have social media", "Rarely on social media", 
                                                        "A few hours per week", "About an hour per day", "Multiple hours per day", NA))
scw$soc <- ifelse(scw$pr7_media_time == "I do not have social media" | scw$pr7_media_time == "Rarely on social media" | scw$pr7_media_time == "A few hours per week", "low_soc", "")
scw$soc <- ifelse(scw$pr7_media_time == "About an hour per day" | scw$pr7_media_time == "Multiple hours per day", "high_soc", scw$soc)
scw$soc <- factor(scw$soc, levels=c("low_soc", "high_soc"))
scw$soc <- relevel(scw$soc, ref = "high_soc")
scw$soc_n <- ifelse(scw$soc == "high_soc", 1, 0)

scw$socatt <- ifelse(scw$soc == "low_soc" & scw$att == "low_att", "lowlow", "")
scw$socatt <- ifelse(scw$soc == "high_soc" & scw$att == "low_att", "highlow", scw$socatt)
scw$socatt <- ifelse(scw$soc == "low_soc" & scw$att == "high_att", "lowhigh", scw$socatt)
scw$socatt <- ifelse(scw$soc == "high_soc" & scw$att == "high_att", "highhigh", scw$socatt)
scw$socatt_n <- ifelse(scw$socatt == "lowlow", 0, 99)
scw$socatt_n <- ifelse(scw$socatt == "highlow", 1, scw$socatt_n)
scw$socatt_n <- ifelse(scw$socatt == "lowhigh", 2, scw$socatt_n)
scw$socatt_n <- ifelse(scw$socatt == "highhigh", 3, scw$socatt_n)
scw$socatt_n <- as.numeric(as.character(scw$socatt_n))

scw$pr7_media_platform <- as.character(scw$pr7_media_platform)
scw$pr7_media_platform <- ifelse(scw$pr7_media_time == "I do not have social media", "I do not have social media", scw$pr7_media_platform)
socialmedia <- data.frame(matrix(0, nrow = 12, ncol = 0))
socialmedia$platform <- c("TikTok", "Instagram", "Twitter/X", "Facebook", "Threads", "Bluesky", "Linkedin", "Reddit", "Gab",
                          "Truth Social", "Other", "None of the Above")
socialmedia$count <- c(sum(scw$pr7_media_platform_tiktok), sum(scw$pr7_media_platform_instagram), sum(scw$pr7_media_platform_twitter), 
                       sum(scw$pr7_media_platform_facebook), sum(scw$pr7_media_platform_threads), sum(scw$pr7_media_platform_bluesky),
                       sum(scw$pr7_media_platform_linkedin), sum(scw$pr7_media_platform_reddit), sum(scw$pr7_media_platform_gab), 
                       sum(scw$pr7_media_platform_truthsocial), sum(scw$pr7_media_platform_other), sum(scw$pr7_media_platform_noneofabove))

scw$pr7_media_platform_tiktok <- as.integer(as.logical(str_detect(scw$pr7_media_platform, "TikTok")))
scw$pr7_media_platform_instagram <- as.integer(as.logical(str_detect(scw$pr7_media_platform, "Instagram")))
scw$pr7_media_platform_twitter <- as.integer(as.logical(str_detect(scw$pr7_media_platform, "Twitter/X")))
scw$pr7_media_platform_facebook <- as.integer(as.logical(str_detect(scw$pr7_media_platform, "Facebook")))
scw$pr7_media_platform_threads <- as.integer(as.logical(str_detect(scw$pr7_media_platform, "Threads")))
scw$pr7_media_platform_bluesky <- as.integer(as.logical(str_detect(scw$pr7_media_platform, "Bluesky")))
scw$pr7_media_platform_linkedin <- as.integer(as.logical(str_detect(scw$pr7_media_platform, "LinkedIn")))
scw$pr7_media_platform_reddit <- as.integer(as.logical(str_detect(scw$pr7_media_platform, "Reddit")))
scw$pr7_media_platform_gab <- as.integer(as.logical(str_detect(scw$pr7_media_platform, "Gab")))
scw$pr7_media_platform_truthsocial <- as.integer(as.logical(str_detect(scw$pr7_media_platform, "Truth Social")))
scw$pr7_media_platform_other <- as.integer(as.logical(str_detect(scw$pr7_media_platform, "Other")))
scw$pr7_media_platform_noneofabove <- as.integer(as.logical(str_detect(scw$pr7_media_platform, "None of the above")))

scw$pr7_media_mostactive <- as.character(scw$pr7_media_mostactive)
scw$pr7_media_mostactive <- ifelse(scw$pr7_media_time == "I do not have social media", "I do not have social media", scw$pr7_media_mostactive)
scw$pr7_media_mostactive <- as.factor(scw$pr7_media_mostactive)
## other social media variables

## Post-Treatment - Foreign Policy
scw$po1_postfreq_n <- recode_factor(scw$po1_postfreq,
                                   "I have never seen a post like this before" = 0,
                                   "Rarely, if ever" = 1,
                                   "A few times per week" = 2,
                                   "At least once per day" = 3,
                                   "At least a few times per day" = 4)
scw$po1_postfreq_n <- as.numeric(as.character(scw$po1_postfreq_n))
scw$po2_sharepost_n <- recode_factor(scw$po2_sharepost,
                                    "Not likely at all" = 0,
                                    "Not very likely" = 1,
                                    "Somewhat likely" = 2,
                                    "Likely" = 3,
                                    "Very likely" = 4)
scw$po2_sharepost_n <- as.numeric(as.character(scw$po2_sharepost_n))

scw$po4_feeling_afraid_n <- recode_factor(scw$po4_feeling_afraid,
                                         "Not at all" = 0,
                                         "Somewhat" = 1,
                                         "Strongly" = 2,
                                         "Very strongly" = 3)
scw$po4_feeling_afraid_n <- as.numeric(as.character(scw$po4_feeling_afraid_n))
scw$po4_feeling_exhausted_n <- recode_factor(scw$po4_feeling_exhausted,
                                            "Not at all" = 0,
                                            "Somewhat" = 1,
                                            "Strongly" = 2,
                                            "Very strongly" = 3)
scw$po4_feeling_exhausted_n <- as.numeric(as.character(scw$po4_feeling_exhausted_n))
scw$po4_feeling_angry_n <- recode_factor(scw$po4_feeling_angry,
                                        "Not at all" = 0,
                                        "Somewhat" = 1,
                                        "Strongly" = 2,
                                        "Very strongly" = 3)
scw$po4_feeling_angry_n <- as.numeric(as.character(scw$po4_feeling_angry_n))
scw$po4_feeling_sad_n <- recode_factor(scw$po4_feeling_sad,
                                      "Not at all" = 0,
                                      "Somewhat" = 1,
                                      "Strongly" = 2,
                                      "Very strongly" = 3)
scw$po4_feeling_sad_n <- as.numeric(as.character(scw$po4_feeling_sad_n))

scw$po5_sympathies_n <- recode_factor(scw$po5_sympathies,
                                     "Not sure" = 0,
                                     "With neither of them" = 0,
                                     "Entirely with the Rapid Support Forces (rebel) supporters" = 1,
                                     "Mostly with the Rapid Support Forces (rebel) supporters but also somewhat with the Sudanese Armed Forces (government) supporters" = 2,
                                     "Equally with both of them" = 3,
                                     "Mostly with the Sudanese Armed Forces (government) supporters but also somewhat with the Rapid Support Forces (rebel) supporters" = 4,
                                     "Entirely with the Sudanese Armed Forces (government) supporters" = 5)
scw$po5_sympathies_n <- as.numeric(as.character(scw$po5_sympathies_n))
scw$po5_sympathies_nr <- recode_factor(scw$po5_sympathies,
                                      "Not sure" = 0,
                                      "With neither of them" = 0,
                                      "Entirely with the Rapid Support Forces (rebel) supporters" = 1,
                                      "Mostly with the Rapid Support Forces (rebel) supporters but also somewhat with the Sudanese Armed Forces (government) supporters" = 1,
                                      "Equally with both of them" = 2,
                                      "Mostly with the Sudanese Armed Forces (government) supporters but also somewhat with the Rapid Support Forces (rebel) supporters" = 3,
                                      "Entirely with the Sudanese Armed Forces (government) supporters" = 3)
scw$po5_sympathies_nr <- as.numeric(as.character(scw$po5_sympathies_nr))
scw$po5_sympathies_nr2 <- recode_factor(scw$po5_sympathies,
                                       "Entirely with the Rapid Support Forces (rebel) supporters" = 0,
                                       "Mostly with the Rapid Support Forces (rebel) supporters but also somewhat with the Sudanese Armed Forces (government) supporters" = 0,
                                       "Equally with both of them" = 1,
                                       "Mostly with the Sudanese Armed Forces (government) supporters but also somewhat with the Rapid Support Forces (rebel) supporters" = 2,
                                       "Entirely with the Sudanese Armed Forces (government) supporters" = 2)
scw$po5_sympathies_nr2 <- as.numeric(as.character(scw$po5_sympathies_nr2))
scw$p5_diff <- scw$po5_sympathies_nr2-scw$pr5_sympathize_ihw_n2
scw$po6_think_n <- recode_factor(scw$po6_think,
                                "Don’t know enough to have an opinion" = 0,
                                "The Sudanese Armed Forces have gone too far, and their military actions aren’t justified" = 1,
                                "The Sudanese Armed Forces are defending their interests, and their military actions are justified" = 2)
scw$po6_think_n <- as.numeric(as.character(scw$po6_think_n))
scw$po7_usaction_n <- recode_factor(scw$po7_usaction,
                                   "Don’t know enough to have an opinion" = 0,
                                   "The U.S. should reduce military and economic aid to the Sudanese Armed Forces until it ends its war with the Rapid Support Forces" = 1,
                                   "The U.S. should exert diplomatic pressure on the Sudanese Armed Forces but not reduce military and economic aid" = 2,
                                   "The U.S. should let the Sudanese Armed Forces pursue whatever policy it thinks best without any U.S. pressure" = 3)
scw$po7_usaction_n <- as.numeric(as.character(scw$po7_usaction_n))
scw$po8_leaderattention_ukraine_n <- recode_factor(scw$po8_leaderattention_ukraine,
                                                  "Too little attention" = 1,
                                                  "Right amount of attention" = 2,
                                                  "Too much attention" = 3)
scw$po8_leaderattention_ukraine_n <- as.numeric(as.character(scw$po8_leaderattention_ukraine_n))
scw$po8_leaderattention_ihw_n <- recode_factor(scw$po8_leaderattention_ihw,
                                              "Too little attention" = 1,
                                              "Right amount of attention" = 2,
                                              "Too much attention" = 3)
scw$po8_leaderattention_ihw_n <- as.numeric(as.character(scw$po8_leaderattention_ihw_n))
scw$po8_leaderattention_myanmar_n <- recode_factor(scw$po8_leaderattention_myanmar,
                                                  "Too little attention" = 1,
                                                  "Right amount of attention" = 2,
                                                  "Too much attention" = 3)
scw$po8_leaderattention_myanmar_n <- as.numeric(as.character(scw$po8_leaderattention_myanmar_n))
scw$po8_leaderattention_sudan_n <- recode_factor(scw$po8_leaderattention_sudan,
                                                "Too little attention" = 1,
                                                "Right amount of attention" = 2,
                                                "Too much attention" = 3)
scw$po8_leaderattention_sudan_n <- as.numeric(as.character(scw$po8_leaderattention_sudan_n))
scw$po8_leaderattention_nk_n <- recode_factor(scw$po8_leaderattention_nk,
                                                 "Too little attention" = 1,
                                                 "Right amount of attention" = 2,
                                                 "Too much attention" = 3)
scw$po8_leaderattention_nk_n <- as.numeric(as.character(scw$po8_leaderattention_nk_n))

scw$po10_important_ceasefire_n <- recode_factor(scw$po10_important_ceasefire,
                                               "Not at all important" = 0,
                                               "Slightly important" = 1,
                                               "Moderately important" = 2,
                                               "Very important" = 3,
                                               "Extremely important" = 4)
scw$po10_important_ceasefire_n <- as.numeric(as.character(scw$po10_important_ceasefire_n))
scw$po11_important_humanitarian_n <- recode_factor(scw$po11_important_humanitarian,
                                                  "Not at all important" = 0,
                                                  "Slightly important" = 1,
                                                  "Moderately important" = 2,
                                                  "Very important" = 3,
                                                  "Extremely important" = 4)
scw$po11_important_humanitarian_n <- as.numeric(as.character(scw$po11_important_humanitarian_n))
scw$po12_important_aidsaf_n <- recode_factor(scw$po12_important_aidsaf,
                                                  "Not at all important" = 0,
                                                  "Slightly important" = 1,
                                                  "Moderately important" = 2,
                                                  "Very important" = 3,
                                                  "Extremely important" = 4)
scw$po12_important_aidsaf_n <- as.numeric(as.character(scw$po12_important_aidsaf_n))

## Post-Treatment - Domestic
scw$po22_vote_yn_n <- recode_factor(scw$po22_vote_yn,
                                   "Prefer not to say" = 0,
                                   "No" = 1,
                                   "Yes" = 2)
scw$po22_vote_yn_n <- as.numeric(as.character(scw$po22_vote_yn_n))
scw$po22_vote_candid_n <- recode_factor(scw$po22_vote_candid,
                                       "Other" = 0,
                                       "Prefer not to say" = 0,
                                       "Kamala Harris" = 1,
                                       "Donald Trump" = 2)
scw$po22_vote_candid_n <- as.numeric(as.character(scw$po22_vote_candid_n))

# Demographics
scw$d1_ideology_n <- recode_factor(scw$d1_ideology,
                                  "Extremely Conservative" = 7,
                                  "Conservative" = 6,
                                  "Slightly Conservative" = 5,
                                  "Moderate, middle of the road" = 4,
                                  "Slightly Liberal" = 3,
                                  "Liberal" = 2,
                                  "Extremely Liberal" = 1)
scw$d1_ideology_n <- as.numeric(as.character(scw$d1_ideology_n))
scw$d2_party_n <- recode_factor(scw$d2_party,
                               "Strong Republican" = 7,
                               "Republican" = 6,
                               "Independent, leaning  to Republican" = 5,
                               "Independent" = 4,
                               "Independent, leaning to Democrat" = 3,
                               "Democrat" = 2,
                               "Strong Democrat" = 1)
scw$d2_party_n <- as.numeric(as.character(scw$d2_party_n))
scw$d2_party <- factor(scw$d2_party, levels=c("Strong Democrat", "Democrat", "Independent, leaning to Democrat",
                                            "Independent", "Independent, leaning  to Republican", "Republican", "Strong Republican", NA))
scw$d2_party_3 <- ifelse(scw$d2_party == "Strong Republican" | scw$d2_party == "Republican" | scw$d2_party == "Independent, leaning  to Republican", "Republican", "")
scw$d2_party_3 <- ifelse(scw$d2_party == "Independent", "Independent", scw$d2_party_3)
scw$d2_party_3 <- ifelse(scw$d2_party == "Strong Democrat" | scw$d2_party == "Democrat" | scw$d2_party == "Independent, leaning to Democrat", "Democrat", scw$d2_party_3)
scw$d2_party_3 <- factor(scw$d2_party_3, levels=c("Republican", "Independent", "Democrat"))
scw$d2_party_3_n <- recode_factor(scw$d2_party_3,
                                 "Republican" = 3,
                                 "Independent" = 2,
                                 "Democrat" = 1)
scw$d2_party_3_n <- as.numeric(as.character(scw$d2_party_3_n))


# scw$d3_pmisrael_n <- recode_factor(scw$d3_pmisrael,
#                                   "Sunak" = 0,
#                                   "Gantz" = 0,
#                                   "Fattah el-Sisi" = 0,
#                                   "Not Sure" = 0,
#                                   "Netanyahu" = 1)
# scw$d3_pmisrael_n <- as.numeric(as.character(scw$d3_pmisrael_n))
# scw$d4_groupattack_n <- recode_factor(scw$d4_groupattack,
#                                      "Hezbollah" = 0,
#                                      "Al Qaeda" = 0,
#                                      "Taliban" = 0,
#                                      "Not Sure" = 0,
#                                      "Hamas" = 1)
# scw$d4_groupattack_n <- as.numeric(as.character(scw$d4_groupattack_n))
scw$d5_sudanesedeaths_n <- recode_factor(scw$d5_sudanesedeaths,
                                   "Higher among SAF supporters" = 0,
                                   "About the Same" = 0,
                                   "Not Sure" = 0,
                                   "Higher among RSF supporters" = 1)
scw$d5_sudanesedeaths_n <- as.numeric(as.character(scw$d5_sudanesedeaths_n))

scw$knowledge <- ifelse(scw$d3_leadersudan == "Abdel Fattah al-Burhan", 1, 0)
scw$knowledge <- ifelse(scw$d4_capitalsudan == "Khartoum", scw$knowledge+1, scw$knowledge)
scw$knowledge_n <- scw$knowledge
scw$knowledge <- as.factor(scw$knowledge)

scw$d6_age_n <- 2024-scw$d6_age
scw$d6_age_gen[scw$d6_age >= 1928 & scw$d6_age <= 1945] <- 'Silent Gen'
scw$d6_age_gen[scw$d6_age >= 1946 & scw$d6_age <= 1964] <- 'Baby Boomer'
scw$d6_age_gen[scw$d6_age >= 1965 & scw$d6_age <= 1980] <- 'Gen X'
scw$d6_age_gen[scw$d6_age >= 1981 & scw$d6_age <= 1996] <- 'Millennial'
scw$d6_age_gen[scw$d6_age >= 1997 & scw$d6_age <= 2010] <- 'Gen Z'
scw$d6_age_gen <- as.factor(scw$d6_age_gen)
scw$d6_age_gen <- factor(scw$d6_age_gen, levels=c("Silent Gen", "Baby Boomer", "Gen X", "Millennial", "Gen Z", NA))
scw$d6_age_gen_n <- recode_factor(scw$d6_age_gen,
                                 "Silent Gen" = 0,
                                 "Baby Boomer" = 1,
                                 "Gen X" = 2,
                                 "Millennial" = 2,
                                 "Gen Z" = 3)
scw$d6_age_gen_n <- as.numeric(as.character(scw$d6_age_gen_n))

scw$d7_gender_n <- recode_factor(scw$d7_gender,
                                "Female" = 0,
                                "Male" = 1,
                                "Non-binary / third gender" = 2,
                                "Prefer to specify:" = 2)
scw$d7_gender_n <- as.numeric(as.character(scw$d7_gender_n))
scw$d8_race_n <- recode_factor(scw$d8_race,
                              "American Indian and Alaskan Native" = 0,
                              "Asian" = 1,
                              "Black" = 2,
                              "Hispanic/Latino" = 3,
                              "Native Hawaiian and Other Pacific Islander" = 4,
                              "White, Non-Hispanic" = 5,
                              "Prefer to specify:" = 6)
scw$d8_race_n <- as.numeric(as.character(scw$d8_race_n))
scw$d9_educ_n <- recode_factor(scw$d9_educ,
                              "Less than high school" = 0,
                              "High school graduate" = 1,
                              "Some college, but no degree" = 2,
                              "2-year college degree" = 3,
                              "4-year college degree" = 4,
                              "Professional degree" = 5,
                              "Doctoral degree" = 6)
scw$d9_educ_n <- as.numeric(as.character(scw$d9_educ_n))
scw$d9_educ <- factor(scw$d9_educ, levels=c("Less than high school", "High school graduate", "Some college, but no degree",
                                          "2-year college degree", "4-year college degree", "Professional degree", "Doctoral degree", NA))
scw$d10_income_n <- recode_factor(scw$d10_income,
                                 "$24,999 or less" = 0,
                                 "$25,000 to $49,999" = 1,
                                 "$50,000 to $74,999" = 2,
                                 "$75,000 to $99,999" = 3,
                                 "$100,000 or $149,999" = 4,
                                 "$150,000 to $199,999" = 5,
                                 "$200,000 to $249,999" = 6,
                                 "$250,000 or more" = 7)
scw$d10_income_n <- as.numeric(as.character(scw$d10_income_n))
scw$d10_income <- factor(scw$d10_income, levels=c("$24,999 or less", "$25,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999", 
                                                "$100,000 or $149,999", "$150,000 to $199,999", "$200,000 to $249,999", "$250,000 or more", NA))
scw$d11_mil_n <- recode_factor(scw$d11_mil,
                              "No" = 0,
                              "Yes" = 1)
scw$d11_mil_n <- as.numeric(as.character(scw$d11_mil_n))

scw$d12_religion_n <- recode_factor(scw$d12_religion,
                                   "Protestant Christian (Non-Evangelical)" = 0,
                                   "Evangelical Protestant Christian" = 1,
                                   "Catholic" = 2,
                                   "Other Christian" = 3,
                                   "Jewish" = 4,
                                   "Muslim" = 5,
                                   "Buddhist" = 6,
                                   "Hindu" = 7,
                                   "Athiest" = 8,
                                   "No formal religious affiliation" = 9,
                                   "Other" = 10)
scw$d12_religion_n <- as.numeric(as.character(scw$d12_religion_n))
scw$d13_religion_important_n <- recode_factor(scw$d13_religion_important,
                                             "Not important at all" = 0,
                                             "A little important" = 1,
                                             "Moderately important" = 2,
                                             "Very important" = 3,
                                             "Extremely important" = 4)
scw$d13_religion_important_n <- as.numeric(as.character(scw$d13_religion_important_n))
scw$d13_religion_important <- factor(scw$d13_religion_important, levels=c('Not important at all', 'A little important', 'Moderately important',
                                                                        'Very important', 'Extremely important'))
scw$d14_religion_service_n <- recode_factor(scw$d14_religion_service,
                                           "Never" = 0,
                                           "A few times a year" = 1,
                                           "Once or twice a month" = 2,
                                           "Almost every week" = 3,
                                           "Every week" = 4)
scw$d14_religion_service_n <- as.numeric(as.character(scw$d14_religion_service_n))
scw$d14_religion_service <- factor(scw$d14_religion_service, levels=c('Never', 'A few times a year', 'Once or twice a month',
                                                                    'Almost every week', 'Every week'))

#ANES

write.csv(scw, "Data/sudan_data_clean.csv")

