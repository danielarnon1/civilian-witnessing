
# REQUIRES: Data/nk.csv
# REQUIRES: Load Packages.R

################# Dependencies #################
source("Load Packages.R")

################# Import Data #################
ncw_raw <- c("Data/nk.csv")
ncw_raw <- lapply(ncw_raw, read.csv) %>% bind_rows()

################# Data Cleaning #################
ncw <- ncw_raw %>% dplyr::mutate(ncw_raw, id = row_number())
ncw <- filter(ncw, ncw$Finished == TRUE)
ncw <- filter(ncw, ncw$Consent.2 == "Yes, I am")


# Remove Variables
ncw_full <- ncw
ncw <- subset(ncw, select = -c(StartDate, EndDate, Status, IPAddress, Progress, Finished, ResponseId, RecipientLastName, 
                             RecipientFirstName, RecipientEmail, ExternalReference, LocationLatitude, LocationLongitude,
                             DistributionChannel, UserLanguage, Q_RecaptchaScore, Consent.1, Consent.2,
                             AZC, DZC, DZCa, AZP, DZP, DZPa, AZS, DZS, DZSa, AMC, DMC, DMCa, AMP, DMP, DMPa, AMS, DMS, DMSa))

# Rename Variables
names(ncw)[names(ncw) == 'Duration..in.seconds.'] <- 'time'
names(ncw)[names(ncw) == 'RecordedDate'] <- 'date'

# Pre-Treatment (1-7)
names(ncw)[names(ncw) == 'Pre.Treatment.1'] <- 'pr1_importantprob'
names(ncw)[names(ncw) == 'Pre.Treatment.2_1'] <- 'pr2_followtopics_election'
names(ncw)[names(ncw) == 'Pre.Treatment.2_2'] <- 'pr2_followtopics_ihw'
names(ncw)[names(ncw) == 'Pre.Treatment.2_3'] <- 'pr2_followtopics_ukraine'
names(ncw)[names(ncw) == 'Pre.Treatment.2_4'] <- 'pr2_followtopics_sudan'
names(ncw)[names(ncw) == 'Pre.Treatment.2_5'] <- 'pr2_followtopics_myanmar'
names(ncw)[names(ncw) == 'Pre.Treatment.2_6'] <- 'pr2_followtopics_nk'
names(ncw)[names(ncw) == 'Pre.Treatment.3'] <- 'pr3_greatpowers'
names(ncw)[names(ncw) == 'Pre.Treatment.4'] <- 'pr4_milforce'
names(ncw)[names(ncw) == 'Pre.Treatment.5a'] <- 'pr5_sympathize_nk'
names(ncw)[names(ncw) == 'Pre.Treatment.5b'] <- 'pr5_sympathize_sudan'
names(ncw)[names(ncw) == 'Pre.Treatment.6a'] <- 'pr6_sympathize_ihw'
names(ncw)[names(ncw) == 'Pre.Treatment.6b'] <- 'pr6_sympathize_ukraine'
names(ncw)[names(ncw) == 'Pre.Treatment.7a'] <- 'pr7_media_time'
names(ncw)[names(ncw) == 'Pre.Treatment.7b'] <- 'pr7_media_platform'
names(ncw)[names(ncw) == 'Pre.Treatment.7b_11_TEXT'] <- 'pr7_media_platform_text'
names(ncw)[names(ncw) == 'Pre.Treatment.7c'] <- 'pr7_media_mostactive'
names(ncw)[names(ncw) == 'Pre.Treatment.7c_11_TEXT'] <- 'pr7_media_mostactive_text'

# Immediate Post Treatment (1-4)
names(ncw)[names(ncw) == 'Post.Treatment.1'] <- 'po1_postfreq'
names(ncw)[names(ncw) == 'Post.Treatment.2'] <- 'po2_sharepost'
names(ncw)[names(ncw) == 'Post.Treatment.3'] <- 'po3_3words'
names(ncw)[names(ncw) == 'Post.Treatment.4_1'] <- 'po4_feeling_afraid'
names(ncw)[names(ncw) == 'Post.Treatment.4_2'] <- 'po4_feeling_exhausted'
names(ncw)[names(ncw) == 'Post.Treatment.4_3'] <- 'po4_feeling_angry'
names(ncw)[names(ncw) == 'Post.Treatment.4_4'] <- 'po4_feeling_sad'

# Post-Treatment - Foreign Policy - Attitudes (5-8)
names(ncw)[names(ncw) == 'Post.Treatment.5'] <- 'po5_sympathies'
names(ncw)[names(ncw) == 'Post.Treatment.6'] <- 'po6_think'
names(ncw)[names(ncw) == 'Post.Treatment.7'] <- 'po7_usaction'
names(ncw)[names(ncw) == 'Post.Treatment.8_1'] <- 'po8_leaderattention_ukraine'
names(ncw)[names(ncw) == 'Post.Treatment.8_2'] <- 'po8_leaderattention_ihw'
names(ncw)[names(ncw) == 'Post.Treatment.8_3'] <- 'po8_leaderattention_myanmar'
names(ncw)[names(ncw) == 'Post.Treatment.8_4'] <- 'po8_leaderattention_sudan'
names(ncw)[names(ncw) == 'Post.Treatment.8_5'] <- 'po8_leaderattention_nk'

# Post-Treatment - Foreign Policy - Goals (10-12) - skipped 9 for parallelism
names(ncw)[names(ncw) == 'Post.Treatment.10'] <- 'po10_important_ceasefire'
names(ncw)[names(ncw) == 'Post.Treatment.11'] <- 'po11_important_humanitarian'
names(ncw)[names(ncw) == 'Post.Treatment.12'] <- 'po12_important_aidaz'

# Skipped polarization and distance questions in IHW context

# Post-Treatment - Domestic - Voting (22)
names(ncw)[names(ncw) == 'Post.Treatment.22'] <- 'po22_vote_yn'
names(ncw)[names(ncw) == 'Post.Treatment.22a'] <- 'po22_vote_candid'
names(ncw)[names(ncw) == 'Post.Treatment.22a_3_TEXT'] <- 'po22_vote_candid_text'

# Demographics (1-15)
names(ncw)[names(ncw) == 'Demographics.1'] <- 'd1_ideology'
names(ncw)[names(ncw) == 'Demographics.2'] <- 'd2_party'
names(ncw)[names(ncw) == 'Demographics.3'] <- 'd3_leaderaz'
names(ncw)[names(ncw) == 'Demographics.4'] <- 'd4_countrychristian'
names(ncw)[names(ncw) == 'Demographics.5'] <- 'd5_nkdeaths'
names(ncw)[names(ncw) == 'Demographics.6'] <- 'd6_age'
names(ncw)[names(ncw) == 'Demographics.7'] <- 'd7_gender'
names(ncw)[names(ncw) == 'Demographics.7_4_TEXT'] <- 'd7_gender_text'
names(ncw)[names(ncw) == 'Demographics.8'] <- 'd8_race'
names(ncw)[names(ncw) == 'Demographics.8_7_TEXT'] <- 'd8_race_text'
names(ncw)[names(ncw) == 'Demographics.9'] <- 'd9_educ'
names(ncw)[names(ncw) == 'Demographics.10'] <- 'd10_income'
names(ncw)[names(ncw) == 'Demographics.11'] <- 'd11_mil'
names(ncw)[names(ncw) == 'Demographics.12'] <- 'd12_religion'
names(ncw)[names(ncw) == 'Demographics.12_10_TEXT'] <- 'd12_religion_text'
names(ncw)[names(ncw) == 'Demographics.13'] <- 'd13_religion_important'
names(ncw)[names(ncw) == 'Demographics.14'] <- 'd14_religion_service'

# Reorder Variables
ncw <- ncw %>% dplyr::select(id, date, time, condition, attentioncheck, donate, donate_amt, everything())

# Categorical Variables
## Condition
ncw$personal <- ifelse(ncw$condition == "Condition_Azerbaijan_CW" |
                        ncw$condition == "Condition_Azerbaijan_Personal" |
                        ncw$condition == "Condition_Armenia_CW" |
                        ncw$condition == "Condition_Armenia_Personal", 1, 0)
ncw$personal <- as.numeric(as.character(ncw$personal))
ncw$stats <- ifelse(ncw$condition == "Condition_Azerbaijan_Statistics" |
                     ncw$condition == "Condition_Armenia_Statistics", 1, 0)
ncw$stats <- as.numeric(as.character(ncw$stats))
ncw$control <- ifelse(ncw$condition == "Control", 1, 0)
ncw$control <- as.numeric(as.character(ncw$control))

ncw$condition_c <- ifelse(ncw$personal == 1, "personal", "")
ncw$condition_c <- ifelse(ncw$stats == 1, "stats", ncw$condition_c)
ncw$condition_c <- ifelse(ncw$control == 1, "control", ncw$condition_c)
ncw$condition_c <- relevel(ncw$condition_c, ref = "control")

ncw$armenia <- ifelse(ncw$condition == "Condition_Armenia_CW" |
                     ncw$condition == "Condition_Armenia_Personal" |
                     ncw$condition == "Condition_Armenia_Statistics", 1, 0)
ncw$armenia <- as.numeric(as.character(ncw$armenia))
ncw$azerbaijan <- ifelse(ncw$condition == "Condition_Azerbaijan_CW" |
                  ncw$condition == "Condition_Azerbaijan_Personal" |
                  ncw$condition == "Condition_Azerbaijan_Statistics", 1, 0)
ncw$azerbaijan <- as.numeric(as.character(ncw$azerbaijan))
ncw$condition_ipc <- ifelse(ncw$armenia == 1, "armenia", "")
ncw$condition_ipc <- ifelse(ncw$azerbaijan == 1, "azerbaijan", ncw$condition_ipc)
ncw$condition_ipc <- ifelse(ncw$control == 1, "control", ncw$condition_ipc)
ncw$condition_ipc <- as.factor(ncw$condition_ipc)

ncw$condition_ncw <- ifelse(ncw$condition == "Condition_Azerbaijan_CW" |
                            ncw$condition == "Condition_Armenia_CW", 1, 0)
ncw$condition_ncw <- as.numeric(as.character(ncw$condition_ncw))
ncw$condition_personal <- ifelse(ncw$condition == "Condition_Azerbaijan_Personal" |
                                  ncw$condition == "Condition_Armenia_Personal", 1, 0)
ncw$condition_personal <- as.numeric(as.character(ncw$condition_personal))
ncw$stats <- ifelse(ncw$condition == "Condition_Azerbaijan_Statistics" |
                     ncw$condition == "Condition_Armenia_Statistics", 1, 0)
ncw$stats <- as.numeric(as.character(ncw$stats))
ncw$control <- ifelse(ncw$condition == "Control", 1, 0)
ncw$control <- as.numeric(as.character(ncw$control))

ncw$condition_vers <- ifelse(ncw$condition_ncw == 1, "CW", "")
ncw$condition_vers <- ifelse(ncw$condition_personal == 1, "Personal", ncw$condition_vers)
ncw$condition_vers <- ifelse(ncw$stats == 1, "Statistics", ncw$condition_vers)
ncw$condition_vers <- ifelse(ncw$control == 1, "Control", ncw$condition_vers)
ncw$condition_vers <- relevel(ncw$condition_vers, ref = "Control")

ncw$condition_sep <- ifelse(ncw$condition == "Condition_Azerbaijan_CW", "Azerbaijan_CW", "")
ncw$condition_sep <- ifelse(ncw$condition == "Condition_Armenia_CW", "Armenia_CW", ncw$condition_sep)
ncw$condition_sep <- ifelse(ncw$condition == "Condition_Azerbaijan_Personal", "Azerbaijan_News", ncw$condition_sep)
ncw$condition_sep <- ifelse(ncw$condition == "Condition_Armenia_Personal", "Armenia_News", ncw$condition_sep)
ncw$condition_sep <- ifelse(ncw$condition == "Condition_Azerbaijan_Statistics", "Azerbaijan_News", ncw$condition_sep)
ncw$condition_sep <- ifelse(ncw$condition == "Condition_Armenia_Statistics", "Armenia_News", ncw$condition_sep)
ncw$condition_sep <- ifelse(ncw$condition == "Control", "Control", ncw$condition_sep)
ncw$condition_sep <- relevel(ncw$condition_sep, ref = "Control")

ncw$condition_sep2 <- ifelse(ncw$condition == "Condition_Azerbaijan_CW", "Azerbaijan_Personal", "")
ncw$condition_sep2 <- ifelse(ncw$condition == "Condition_Armenia_CW", "Armenia_Personal", ncw$condition_sep2)
ncw$condition_sep2 <- ifelse(ncw$condition == "Condition_Azerbaijan_Personal", "Azerbaijan_Personal", ncw$condition_sep2)
ncw$condition_sep2 <- ifelse(ncw$condition == "Condition_Armenia_Personal", "Armenia_Personal", ncw$condition_sep2)
ncw$condition_sep2 <- ifelse(ncw$condition == "Condition_Azerbaijan_Statistics", "Azerbaijan_Statistics", ncw$condition_sep2)
ncw$condition_sep2 <- ifelse(ncw$condition == "Condition_Armenia_Statistics", "Armenia_Statistics", ncw$condition_sep2)
ncw$condition_sep2 <- ifelse(ncw$condition == "Control", "Control", ncw$condition_sep2)
ncw$condition_sep2 <- relevel(ncw$condition_sep2, ref = "Control")

ncw$condition_sep3 <- ifelse(ncw$condition == "Condition_Azerbaijan_CW", "Azerbaijan_Personal", "")
ncw$condition_sep3 <- ifelse(ncw$condition == "Condition_Armenia_CW", "Armenia_Personal", ncw$condition_sep3)
ncw$condition_sep3 <- ifelse(ncw$condition == "Condition_Azerbaijan_Personal", "Azerbaijan_Personal", ncw$condition_sep3)
ncw$condition_sep3 <- ifelse(ncw$condition == "Condition_Armenia_Personal", "Armenia_Personal", ncw$condition_sep3)
ncw$condition_sep3 <- ifelse(ncw$condition == "Condition_Azerbaijan_Statistics", "Azerbaijan_Statistics", ncw$condition_sep3)
ncw$condition_sep3 <- ifelse(ncw$condition == "Condition_Armenia_Statistics", "Armenia_Statistics", ncw$condition_sep3)
ncw$condition_sep3 <- ifelse(ncw$condition == "Control", "Control", ncw$condition_sep3)
ncw$condition_sep3 <- relevel(ncw$condition_sep3, ref = "Control")

## Pre-Treatment
ncw$pr2_followtopics_election_n <- recode_factor(ncw$pr2_followtopics_election,
                                                "Not closely at all" = 1,
                                                "Not too closely" = 2,
                                                "Somewhat closely" = 3,
                                                "Very closely" = 4,
                                                "Extremely closely" = 5)
ncw$pr2_followtopics_election_n <- as.numeric(as.character(ncw$pr2_followtopics_election_n))
ncw$pr2_followtopics_ihw_n <- recode_factor(ncw$pr2_followtopics_ihw,
                                           "Not closely at all" = 1,
                                           "Not too closely" = 2,
                                           "Somewhat closely" = 3,
                                           "Very closely" = 4,
                                           "Extremely closely" = 5)
ncw$pr2_followtopics_ihw_n <- as.numeric(as.character(ncw$pr2_followtopics_ihw_n))
ncw$att <- ifelse(ncw$pr2_followtopics_ihw == "Not closely at all" | ncw$pr2_followtopics_ihw == "Not too closely", "low_att", "")
ncw$att <- ifelse(ncw$pr2_followtopics_ihw == "Somewhat closely" | ncw$pr2_followtopics_ihw == "Very closely" | ncw$pr2_followtopics_ihw == "Extremely closely", "high_att", ncw$att)
ncw$att_n <- ifelse(ncw$att == "high_att", 1, 0)

ncw$pr2_followtopics_ukraine_n <- recode_factor(ncw$pr2_followtopics_ukraine,
                                               "Not closely at all" = 1,
                                               "Not too closely" = 2,
                                               "Somewhat closely" = 3,
                                               "Very closely" = 4,
                                               "Extremely closely" = 5)
ncw$pr2_followtopics_ukraine_n <- as.numeric(as.character(ncw$pr2_followtopics_ukraine_n))
ncw$pr2_followtopics_sudan_n <- recode_factor(ncw$pr2_followtopics_sudan,
                                             "Not closely at all" = 1,
                                             "Not too closely" = 2,
                                             "Somewhat closely" = 3,
                                             "Very closely" = 4,
                                             "Extremely closely" = 5)
ncw$pr2_followtopics_sudan_n <- as.numeric(as.character(ncw$pr2_followtopics_sudan_n))
ncw$pr2_followtopics_myanmar_n <- recode_factor(ncw$pr2_followtopics_myanmar,
                                               "Not closely at all" = 1,
                                               "Not too closely" = 2,
                                               "Somewhat closely" = 3,
                                               "Very closely" = 4,
                                               "Extremely closely" = 5)
ncw$pr2_followtopics_myanmar_n <- as.numeric(as.character(ncw$pr2_followtopics_myanmar_n))

ncw$pr3_greatpowers <- as.character(ncw$pr3_greatpowers)
ncw$pr3_greatpowers <- ifelse(ncw$pr3_greatpowers == "Neither Agree Nor DIsagree", "Neither Agree Nor Disagree", ncw$pr3_greatpowers)
ncw$pr3_greatpowers <- as.factor(ncw$pr3_greatpowers)
ncw$pr3_greatpowers_n <- recode_factor(ncw$pr3_greatpowers,
                                      "Strongly Disagree" = 0,
                                      "Disagree" = 1,
                                      "Neither Agree Nor Disagree" = 2,
                                      "Agree" = 3,
                                      "Strongly Agree" = 4)
ncw$pr3_greatpowers_n <- as.numeric(as.character(ncw$pr3_greatpowers_n))
ncw$pr3_greatpowers <- factor(ncw$pr3_greatpowers, levels=c("Strongly Disagree", "Disagree", "Neither Agree Nor Disagree", "Agree", "Strongly Agree", NA))
ncw$pr4_milforce <- as.character(ncw$pr4_milforce)
ncw$pr4_milforce <- ifelse(ncw$pr4_milforce == "Neither Agree Nor DIsagree", "Neither Agree Nor Disagree", ncw$pr4_milforce)
ncw$pr4_milforce <- as.factor(ncw$pr4_milforce)
ncw$pr4_milforce_n <- recode_factor(ncw$pr4_milforce,
                                   "Strongly Disagree" = 0,
                                   "Disagree" = 1,
                                   "Neither Agree Nor Disagree" = 2,
                                   "Agree" = 3,
                                   "Strongly Agree" = 4)
ncw$pr4_milforce_n <- as.numeric(as.character(ncw$pr4_milforce_n))
ncw$pr4_milforce <- factor(ncw$pr4_milforce, levels=c("Strongly Disagree", "Disagree", "Neither Agree Nor Disagree", "Agree", "Strongly Agree", NA))

# ncw$pr5_sympathize_nk_n <- recode_factor(ncw$pr5_sympathize_nk,
#                                          "Neither" = 0,
#                                          "More with the Palestinians" = 1,
#                                          "Both equally" = 2,
#                                          "More with the Israelis" = 3)
# ncw$pr5_sympathize_nk_n <- as.numeric(as.character(ncw$pr5_sympathize_nk_n))
# ncw$pr5_sympathize_nk2 <- recode_factor(ncw$pr5_sympathize_nk,
#                                         "More with the Palestinians" = "Pales",
#                                         "Both equally" = "Both",
#                                         "More with the Israelis" = "Is")
# ncw$pr5_sympathize_nk2 <- relevel(ncw$pr5_sympathize_nk2, ref = "Both")
# ncw$pr5_sympathize_nk_n2 <- recode_factor(ncw$pr5_sympathize_nk,
#                                           "More with the Palestinians" = 0,
#                                           "Both equally" = 1,
#                                           "More with the Israelis" = 2)
# ncw$pr5_sympathize_nk_n2 <- as.numeric(as.character(ncw$pr5_sympathize_nk_n2))
# ncw$pr5_sympathize_nk_bnt <- as.character(ncw$pr5_sympathize_nk)
# ncw$pr5_sympathize_nk_bnt <- ifelse(ncw$pr5_sympathize_nk == "Neither", "Both_Neither", ncw$pr5_sympathize_nk_bnt)
# ncw$pr5_sympathize_nk_bnt <- ifelse(ncw$pr5_sympathize_nk_bn == "Both equally", "Both_Neither", ncw$pr5_sympathize_nk_bnt)
# ncw$pr5_sympathize_nk_bnt <- as.factor(ncw$pr5_sympathize_nk_bnt)
# 
# ncw$pr5_sympathize_sudan_n <- recode_factor(ncw$pr5_sympathize_sudan,
#                                          "Neither" = 0,
#                                          "More with the Palestinians" = 1,
#                                          "Both equally" = 2,
#                                          "More with the Israelis" = 3)
# ncw$pr5_sympathize_sudan_n <- as.numeric(as.character(ncw$pr5_sympathize_sudan_n))
# ncw$pr5_sympathize_sudan2 <- recode_factor(ncw$pr5_sympathize_sudan,
#                                         "More with the Palestinians" = "Pales",
#                                         "Both equally" = "Both",
#                                         "More with the Israelis" = "Is")
# ncw$pr5_sympathize_sudan2 <- relevel(ncw$pr5_sympathize_sudan2, ref = "Both")
# ncw$pr5_sympathize_sudan_n2 <- recode_factor(ncw$pr5_sympathize_sudan,
#                                           "More with the Palestinians" = 0,
#                                           "Both equally" = 1,
#                                           "More with the Israelis" = 2)
# ncw$pr5_sympathize_sudan_n2 <- as.numeric(as.character(ncw$pr5_sympathize_sudan_n2))
# ncw$pr5_sympathize_sudan_bnt <- as.character(ncw$pr5_sympathize_sudan)
# ncw$pr5_sympathize_sudan_bnt <- ifelse(ncw$pr5_sympathize_sudan == "Neither", "Both_Neither", ncw$pr5_sympathize_sudan_bnt)
# ncw$pr5_sympathize_sudan_bnt <- ifelse(ncw$pr5_sympathize_sudan_bn == "Both equally", "Both_Neither", ncw$pr5_sympathize_sudan_bnt)
# ncw$pr5_sympathize_sudan_bnt <- as.factor(ncw$pr5_sympathize_sudan_bnt)


# ncw$pr5_sympathize_ihw_otherside_n <- ifelse(ncw$condition == "Condition_Palestine_CW" & ncw$pr5_sympathize_ihw == "More with the Israelis", 1, 0)
# ncw$pr5_sympathize_ihw_otherside_n <- ifelse(ncw$condition == "Condition_Palestine_Personal" & ncw$pr5_sympathize_ihw == "More with the Israelis", 1, ncw$pr5_sympathize_ihw_otherside_n)
# ncw$pr5_sympathize_ihw_otherside_n <- ifelse(ncw$condition == "Condition_Palestine_Statistics" & ncw$pr5_sympathize_ihw == "More with the Israelis", 1, ncw$pr5_sympathize_ihw_otherside_n)
# ncw$pr5_sympathize_ihw_otherside_n <- ifelse(ncw$condition == "Condition_Israel_CW" & ncw$pr5_sympathize_ihw == "More with the Palestinians", 1, ncw$pr5_sympathize_ihw_otherside_n)
# ncw$pr5_sympathize_ihw_otherside_n <- ifelse(ncw$condition == "Condition_Israel_Personal" & ncw$pr5_sympathize_ihw == "More with the Palestinians", 1, ncw$pr5_sympathize_ihw_otherside_n)
# ncw$pr5_sympathize_ihw_otherside_n <- ifelse(ncw$condition == "Condition_Israel_Statistics" & ncw$pr5_sympathize_ihw == "More with the Palestinians", 1, ncw$pr5_sympathize_ihw_otherside_n)
# ncw$pr5_sympathize_ihw_otherside_n <- as.numeric(as.character(ncw$pr5_sympathize_ihw_otherside_n))
# ncw$pr5_sympathize_ihw_pales <- as.numeric(as.character(ifelse(ncw$pr5_sympathize_ihw == "More with the Palestinians", 1, 0)))
# ncw$pr5_sympathize_ihw_pales_f <- as.factor(ncw$pr5_sympathize_ihw_pales)
# ncw$pr5_sympathize_ihw_is <- as.numeric(as.character(ifelse(ncw$pr5_sympathize_ihw == "More with the Israelis", 1, 0)))
# ncw$pr5_sympathize_ihw_both <- as.numeric(as.character(ifelse(ncw$pr5_sympathize_ihw == "Both equally", 1, 0)))
# ncw$pr5_sympathize_ihw_neither <- as.numeric(as.character(ifelse(ncw$pr5_sympathize_ihw == "Neither", 1, 0)))
# ncw$pr5_sympathize_ihw_bn <- as.numeric(as.character(ifelse(ncw$pr5_sympathize_ihw == "Both equally", 1, 0)))
# ncw$pr5_sympathize_ihw_bn <- as.numeric(as.character(ifelse(ncw$pr5_sympathize_ihw == "Neither", 1, ncw$pr5_sympathize_ihw_bn)))
# ncw$pr6_sympathize_ukraine_n <- recode_factor(ncw$pr6_sympathize_ukraine,
#                                              "Neither" = 0,
#                                              "More with the Russians" = 1,
#                                              "Both equally" = 2,
#                                              "More with the Ukrainians" = 3)
# ncw$pr6_sympathize_ukraine_n <- as.numeric(as.character(ncw$pr6_sympathize_ukraine_n))

ncw$pr7_media_time_n <- recode_factor(ncw$pr7_media_time,
                                     "I do not have social media" = 0,
                                     "Rarely on social media" = 1,
                                     "A few hours per week" = 2,
                                     "About an hour per day" = 3,
                                     "Multiple hours per day" = 4)
ncw$pr7_media_time_n <- as.numeric(as.character(ncw$pr7_media_time_n))
ncw$pr7_media_time <- factor(ncw$pr7_media_time, levels=c("I do not have social media", "Rarely on social media", 
                                                        "A few hours per week", "About an hour per day", "Multiple hours per day", NA))
ncw$soc <- ifelse(ncw$pr7_media_time == "I do not have social media" | ncw$pr7_media_time == "Rarely on social media" | ncw$pr7_media_time == "A few hours per week", "low_soc", "")
ncw$soc <- ifelse(ncw$pr7_media_time == "About an hour per day" | ncw$pr7_media_time == "Multiple hours per day", "high_soc", ncw$soc)
ncw$soc <- factor(ncw$soc, levels=c("low_soc", "high_soc"))
ncw$soc <- relevel(ncw$soc, ref = "high_soc")
ncw$soc_n <- ifelse(ncw$soc == "high_soc", 1, 0)

ncw$socatt <- ifelse(ncw$soc == "low_soc" & ncw$att == "low_att", "lowlow", "")
ncw$socatt <- ifelse(ncw$soc == "high_soc" & ncw$att == "low_att", "highlow", ncw$socatt)
ncw$socatt <- ifelse(ncw$soc == "low_soc" & ncw$att == "high_att", "lowhigh", ncw$socatt)
ncw$socatt <- ifelse(ncw$soc == "high_soc" & ncw$att == "high_att", "highhigh", ncw$socatt)
ncw$socatt_n <- ifelse(ncw$socatt == "lowlow", 0, 99)
ncw$socatt_n <- ifelse(ncw$socatt == "highlow", 1, ncw$socatt_n)
ncw$socatt_n <- ifelse(ncw$socatt == "lowhigh", 2, ncw$socatt_n)
ncw$socatt_n <- ifelse(ncw$socatt == "highhigh", 3, ncw$socatt_n)
ncw$socatt_n <- as.numeric(as.character(ncw$socatt_n))

ncw$pr7_media_platform <- as.character(ncw$pr7_media_platform)
ncw$pr7_media_platform <- ifelse(ncw$pr7_media_time == "I do not have social media", "I do not have social media", ncw$pr7_media_platform)
socialmedia <- data.frame(matrix(0, nrow = 12, ncol = 0))
socialmedia$platform <- c("TikTok", "Instagram", "Twitter/X", "Facebook", "Threads", "Bluesky", "Linkedin", "Reddit", "Gab",
                          "Truth Social", "Other", "None of the Above")
socialmedia$count <- c(sum(ncw$pr7_media_platform_tiktok), sum(ncw$pr7_media_platform_instagram), sum(ncw$pr7_media_platform_twitter), 
                       sum(ncw$pr7_media_platform_facebook), sum(ncw$pr7_media_platform_threads), sum(ncw$pr7_media_platform_bluesky),
                       sum(ncw$pr7_media_platform_linkedin), sum(ncw$pr7_media_platform_reddit), sum(ncw$pr7_media_platform_gab), 
                       sum(ncw$pr7_media_platform_truthsocial), sum(ncw$pr7_media_platform_other), sum(ncw$pr7_media_platform_noneofabove))

ncw$pr7_media_platform_tiktok <- as.integer(as.logical(str_detect(ncw$pr7_media_platform, "TikTok")))
ncw$pr7_media_platform_instagram <- as.integer(as.logical(str_detect(ncw$pr7_media_platform, "Instagram")))
ncw$pr7_media_platform_twitter <- as.integer(as.logical(str_detect(ncw$pr7_media_platform, "Twitter/X")))
ncw$pr7_media_platform_facebook <- as.integer(as.logical(str_detect(ncw$pr7_media_platform, "Facebook")))
ncw$pr7_media_platform_threads <- as.integer(as.logical(str_detect(ncw$pr7_media_platform, "Threads")))
ncw$pr7_media_platform_bluesky <- as.integer(as.logical(str_detect(ncw$pr7_media_platform, "Bluesky")))
ncw$pr7_media_platform_linkedin <- as.integer(as.logical(str_detect(ncw$pr7_media_platform, "LinkedIn")))
ncw$pr7_media_platform_reddit <- as.integer(as.logical(str_detect(ncw$pr7_media_platform, "Reddit")))
ncw$pr7_media_platform_gab <- as.integer(as.logical(str_detect(ncw$pr7_media_platform, "Gab")))
ncw$pr7_media_platform_truthsocial <- as.integer(as.logical(str_detect(ncw$pr7_media_platform, "Truth Social")))
ncw$pr7_media_platform_other <- as.integer(as.logical(str_detect(ncw$pr7_media_platform, "Other")))
ncw$pr7_media_platform_noneofabove <- as.integer(as.logical(str_detect(ncw$pr7_media_platform, "None of the above")))

ncw$pr7_media_mostactive <- as.character(ncw$pr7_media_mostactive)
ncw$pr7_media_mostactive <- ifelse(ncw$pr7_media_time == "I do not have social media", "I do not have social media", ncw$pr7_media_mostactive)
ncw$pr7_media_mostactive <- as.factor(ncw$pr7_media_mostactive)
## other social media variables

## Post-Treatment - Foreign Policy
ncw$po1_postfreq_n <- recode_factor(ncw$po1_postfreq,
                                   "I have never seen a post like this before" = 0,
                                   "Rarely, if ever" = 1,
                                   "A few times per week" = 2,
                                   "At least once per day" = 3,
                                   "At least a few times per day" = 4)
ncw$po1_postfreq_n <- as.numeric(as.character(ncw$po1_postfreq_n))
ncw$po2_sharepost_n <- recode_factor(ncw$po2_sharepost,
                                    "Not likely at all" = 0,
                                    "Not very likely" = 1,
                                    "Somewhat likely" = 2,
                                    "Likely" = 3,
                                    "Very likely" = 4)
ncw$po2_sharepost_n <- as.numeric(as.character(ncw$po2_sharepost_n))

ncw$po4_feeling_afraid_n <- recode_factor(ncw$po4_feeling_afraid,
                                         "Not at all" = 0,
                                         "Somewhat" = 1,
                                         "Strongly" = 2,
                                         "Very strongly" = 3)
ncw$po4_feeling_afraid_n <- as.numeric(as.character(ncw$po4_feeling_afraid_n))
ncw$po4_feeling_exhausted_n <- recode_factor(ncw$po4_feeling_exhausted,
                                            "Not at all" = 0,
                                            "Somewhat" = 1,
                                            "Strongly" = 2,
                                            "Very strongly" = 3)
ncw$po4_feeling_exhausted_n <- as.numeric(as.character(ncw$po4_feeling_exhausted_n))
ncw$po4_feeling_angry_n <- recode_factor(ncw$po4_feeling_angry,
                                        "Not at all" = 0,
                                        "Somewhat" = 1,
                                        "Strongly" = 2,
                                        "Very strongly" = 3)
ncw$po4_feeling_angry_n <- as.numeric(as.character(ncw$po4_feeling_angry_n))
ncw$po4_feeling_sad_n <- recode_factor(ncw$po4_feeling_sad,
                                      "Not at all" = 0,
                                      "Somewhat" = 1,
                                      "Strongly" = 2,
                                      "Very strongly" = 3)
ncw$po4_feeling_sad_n <- as.numeric(as.character(ncw$po4_feeling_sad_n))

ncw$po5_sympathies_n <- recode_factor(ncw$po5_sympathies,
                                     "Not sure" = 0,
                                     "With neither of them" = 0,
                                     "Entirely with the Armenian people" = 1,
                                     "Mostly with the Armenian people but also somewhat with the Azerbaijani people" = 2,
                                     "Equally with both of them" = 3,
                                     "Mostly with the Azerbaijani people but also somewhat with the Armenian people" = 4,
                                     "Entirely with the Azerbaijani people" = 5)
ncw$po5_sympathies_n <- as.numeric(as.character(ncw$po5_sympathies_n))
ncw$po5_sympathies_nr <- recode_factor(ncw$po5_sympathies,
                                      "Not sure" = 0,
                                      "With neither of them" = 0,
                                      "Entirely with the Armenian people" = 1,
                                      "Mostly with the Armenian people but also somewhat with the Azerbaijani people" = 1,
                                      "Equally with both of them" = 2,
                                      "Mostly with the Azerbaijani people but also somewhat with the Armenian people" = 3,
                                      "Entirely with the Azerbaijani people" = 3)
ncw$po5_sympathies_nr <- as.numeric(as.character(ncw$po5_sympathies_nr))
ncw$po5_sympathies_nr2 <- recode_factor(ncw$po5_sympathies,
                                       "Entirely with the Armenian people" = 0,
                                       "Mostly with the Armenian people but also somewhat with the Azerbaijani people" = 0,
                                       "Equally with both of them" = 1,
                                       "Mostly with the Azerbaijani people but also somewhat with the Armenian people" = 2,
                                       "Entirely with the Azerbaijani people" = 2)
ncw$po5_sympathies_nr2 <- as.numeric(as.character(ncw$po5_sympathies_nr2))
ncw$p5_diff <- ncw$po5_sympathies_nr2-ncw$pr5_sympathize_ihw_n2
ncw$po6_think_n <- recode_factor(ncw$po6_think,
                                "Don’t know enough to have an opinion" = 0,
                                "Azerbaijan is defending its interests, and its military actions are justified" = 1,
                                "Azerbaijan has gone too far, and its military actions aren’t justified" = 2)
ncw$po6_think_n <- as.numeric(as.character(ncw$po6_think_n))
ncw$po7_usaction_n <- recode_factor(ncw$po7_usaction,
                                   "Don’t know enough to have an opinion" = 0,
                                   "The U.S. should reduce military and economic aid to Azerbaijan until it ends its war with Armenia" = 1,
                                   "The U.S. should exert diplomatic pressure on Azerbaijan but not reduce military and economic aid" = 2,
                                   "The U.S. should let Azerbaijan pursue whatever policy it thinks best without any U.S. pressure" = 3)
ncw$po7_usaction_n <- as.numeric(as.character(ncw$po7_usaction_n))
ncw$po8_leaderattention_ukraine_n <- recode_factor(ncw$po8_leaderattention_ukraine,
                                                  "Too little attention" = 1,
                                                  "Right amount of attention" = 2,
                                                  "Too much attention" = 3)
ncw$po8_leaderattention_ukraine_n <- as.numeric(as.character(ncw$po8_leaderattention_ukraine_n))
ncw$po8_leaderattention_ihw_n <- recode_factor(ncw$po8_leaderattention_ihw,
                                              "Too little attention" = 1,
                                              "Right amount of attention" = 2,
                                              "Too much attention" = 3)
ncw$po8_leaderattention_ihw_n <- as.numeric(as.character(ncw$po8_leaderattention_ihw_n))
ncw$po8_leaderattention_myanmar_n <- recode_factor(ncw$po8_leaderattention_myanmar,
                                                  "Too little attention" = 1,
                                                  "Right amount of attention" = 2,
                                                  "Too much attention" = 3)
ncw$po8_leaderattention_myanmar_n <- as.numeric(as.character(ncw$po8_leaderattention_myanmar_n))
ncw$po8_leaderattention_sudan_n <- recode_factor(ncw$po8_leaderattention_sudan,
                                                "Too little attention" = 1,
                                                "Right amount of attention" = 2,
                                                "Too much attention" = 3)
ncw$po8_leaderattention_sudan_n <- as.numeric(as.character(ncw$po8_leaderattention_sudan_n))
ncw$po8_leaderattention_nk_n <- recode_factor(ncw$po8_leaderattention_nk,
                                                 "Too little attention" = 1,
                                                 "Right amount of attention" = 2,
                                                 "Too much attention" = 3)
ncw$po8_leaderattention_nk_n <- as.numeric(as.character(ncw$po8_leaderattention_nk_n))

ncw$po10_important_ceasefire_n <- recode_factor(ncw$po10_important_ceasefire,
                                               "Not at all important" = 0,
                                               "Slightly important" = 1,
                                               "Moderately important" = 2,
                                               "Very important" = 3,
                                               "Extremely important" = 4)
ncw$po10_important_ceasefire_n <- as.numeric(as.character(ncw$po10_important_ceasefire_n))
ncw$po11_important_humanitarian_n <- recode_factor(ncw$po11_important_humanitarian,
                                                  "Not at all important" = 0,
                                                  "Slightly important" = 1,
                                                  "Moderately important" = 2,
                                                  "Very important" = 3,
                                                  "Extremely important" = 4)
ncw$po11_important_humanitarian_n <- as.numeric(as.character(ncw$po11_important_humanitarian_n))
ncw$po12_important_aidaz_n <- recode_factor(ncw$po12_important_aidaz,
                                                  "Not at all important" = 0,
                                                  "Slightly important" = 1,
                                                  "Moderately important" = 2,
                                                  "Very important" = 3,
                                                  "Extremely important" = 4)
ncw$po12_important_aidaz_n <- as.numeric(as.character(ncw$po12_important_aidaz_n))

## Post-Treatment - Domestic
ncw$po22_vote_yn_n <- recode_factor(ncw$po22_vote_yn,
                                   "Prefer not to say" = 0,
                                   "No" = 1,
                                   "Yes" = 2)
ncw$po22_vote_yn_n <- as.numeric(as.character(ncw$po22_vote_yn_n))
ncw$po22_vote_candid_n <- recode_factor(ncw$po22_vote_candid,
                                       "Other" = 0,
                                       "Prefer not to say" = 0,
                                       "Kamala Harris" = 1,
                                       "Donald Trump" = 2)
ncw$po22_vote_candid_n <- as.numeric(as.character(ncw$po22_vote_candid_n))

# Demographics
ncw$d1_ideology_n <- recode_factor(ncw$d1_ideology,
                                  "Extremely Conservative" = 7,
                                  "Conservative" = 6,
                                  "Slightly Conservative" = 5,
                                  "Moderate, middle of the road" = 4,
                                  "Slightly Liberal" = 3,
                                  "Liberal" = 2,
                                  "Extremely Liberal" = 1)
ncw$d1_ideology_n <- as.numeric(as.character(ncw$d1_ideology_n))
ncw$d2_party_n <- recode_factor(ncw$d2_party,
                               "Strong Republican" = 7,
                               "Republican" = 6,
                               "Independent, leaning  to Republican" = 5,
                               "Independent" = 4,
                               "Independent, leaning to Democrat" = 3,
                               "Democrat" = 2,
                               "Strong Democrat" = 1)
ncw$d2_party_n <- as.numeric(as.character(ncw$d2_party_n))
ncw$d2_party <- factor(ncw$d2_party, levels=c("Strong Democrat", "Democrat", "Independent, leaning to Democrat",
                                            "Independent", "Independent, leaning  to Republican", "Republican", "Strong Republican", NA))
ncw$d2_party_3 <- ifelse(ncw$d2_party == "Strong Republican" | ncw$d2_party == "Republican" | ncw$d2_party == "Independent, leaning  to Republican", "Republican", "")
ncw$d2_party_3 <- ifelse(ncw$d2_party == "Independent", "Independent", ncw$d2_party_3)
ncw$d2_party_3 <- ifelse(ncw$d2_party == "Strong Democrat" | ncw$d2_party == "Democrat" | ncw$d2_party == "Independent, leaning to Democrat", "Democrat", ncw$d2_party_3)
ncw$d2_party_3 <- factor(ncw$d2_party_3, levels=c("Republican", "Independent", "Democrat"))
ncw$d2_party_3_n <- recode_factor(ncw$d2_party_3,
                                 "Republican" = 3,
                                 "Independent" = 2,
                                 "Democrat" = 1)
ncw$d2_party_3_n <- as.numeric(as.character(ncw$d2_party_3_n))


# ncw$d3_pmisrael_n <- recode_factor(ncw$d3_pmisrael,
#                                   "Sunak" = 0,
#                                   "Gantz" = 0,
#                                   "Fattah el-Sisi" = 0,
#                                   "Not Sure" = 0,
#                                   "Netanyahu" = 1)
# ncw$d3_pmisrael_n <- as.numeric(as.character(ncw$d3_pmisrael_n))
# ncw$d4_groupattack_n <- recode_factor(ncw$d4_groupattack,
#                                      "Hezbollah" = 0,
#                                      "Al Qaeda" = 0,
#                                      "Taliban" = 0,
#                                      "Not Sure" = 0,
#                                      "Hamas" = 1)
# ncw$d4_groupattack_n <- as.numeric(as.character(ncw$d4_groupattack_n))
ncw$d5_nkdeaths_n <- recode_factor(ncw$d5_sudanesedeaths,
                                   "Higher among Azerbaijanis" = 0,
                                   "About the Same" = 0,
                                   "Not Sure" = 0,
                                   "Higher among Armenians" = 1)
ncw$d5_nkdeaths_n <- as.numeric(as.character(ncw$d5_nkdeaths_n))

ncw$knowledge <- ifelse(ncw$d3_leaderazerbaijan == "Ilham Aliyev", 1, 0)
ncw$knowledge <- ifelse(ncw$d4_countrychristian == "Armenia", ncw$knowledge+1, ncw$knowledge)
ncw$knowledge_n <- ncw$knowledge
ncw$knowledge <- as.factor(ncw$knowledge)

ncw$d6_age_n <- 2024-ncw$d6_age
ncw$d6_age_gen[ncw$d6_age >= 1928 & ncw$d6_age <= 1945] <- 'Silent Gen'
ncw$d6_age_gen[ncw$d6_age >= 1946 & ncw$d6_age <= 1964] <- 'Baby Boomer'
ncw$d6_age_gen[ncw$d6_age >= 1965 & ncw$d6_age <= 1980] <- 'Gen X'
ncw$d6_age_gen[ncw$d6_age >= 1981 & ncw$d6_age <= 1996] <- 'Millennial'
ncw$d6_age_gen[ncw$d6_age >= 1997 & ncw$d6_age <= 2010] <- 'Gen Z'
ncw$d6_age_gen <- as.factor(ncw$d6_age_gen)
ncw$d6_age_gen <- factor(ncw$d6_age_gen, levels=c("Silent Gen", "Baby Boomer", "Gen X", "Millennial", "Gen Z", NA))
ncw$d6_age_gen_n <- recode_factor(ncw$d6_age_gen,
                                 "Silent Gen" = 0,
                                 "Baby Boomer" = 1,
                                 "Gen X" = 2,
                                 "Millennial" = 2,
                                 "Gen Z" = 3)
ncw$d6_age_gen_n <- as.numeric(as.character(ncw$d6_age_gen_n))

ncw$d7_gender_n <- recode_factor(ncw$d7_gender,
                                "Female" = 0,
                                "Male" = 1,
                                "Non-binary / third gender" = 2,
                                "Prefer to specify:" = 2)
ncw$d7_gender_n <- as.numeric(as.character(ncw$d7_gender_n))
ncw$d8_race_n <- recode_factor(ncw$d8_race,
                              "American Indian and Alaskan Native" = 0,
                              "Asian" = 1,
                              "Black" = 2,
                              "Hispanic/Latino" = 3,
                              "Native Hawaiian and Other Pacific Islander" = 4,
                              "White, Non-Hispanic" = 5,
                              "Prefer to specify:" = 6)
ncw$d8_race_n <- as.numeric(as.character(ncw$d8_race_n))
ncw$d9_educ_n <- recode_factor(ncw$d9_educ,
                              "Less than high school" = 0,
                              "High school graduate" = 1,
                              "Some college, but no degree" = 2,
                              "2-year college degree" = 3,
                              "4-year college degree" = 4,
                              "Professional degree" = 5,
                              "Doctoral degree" = 6)
ncw$d9_educ_n <- as.numeric(as.character(ncw$d9_educ_n))
ncw$d9_educ <- factor(ncw$d9_educ, levels=c("Less than high school", "High school graduate", "Some college, but no degree",
                                          "2-year college degree", "4-year college degree", "Professional degree", "Doctoral degree", NA))
ncw$d10_income_n <- recode_factor(ncw$d10_income,
                                 "$24,999 or less" = 0,
                                 "$25,000 to $49,999" = 1,
                                 "$50,000 to $74,999" = 2,
                                 "$75,000 to $99,999" = 3,
                                 "$100,000 or $149,999" = 4,
                                 "$150,000 to $199,999" = 5,
                                 "$200,000 to $249,999" = 6,
                                 "$250,000 or more" = 7)
ncw$d10_income_n <- as.numeric(as.character(ncw$d10_income_n))
ncw$d10_income <- factor(ncw$d10_income, levels=c("$24,999 or less", "$25,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999", 
                                                "$100,000 or $149,999", "$150,000 to $199,999", "$200,000 to $249,999", "$250,000 or more", NA))
ncw$d11_mil_n <- recode_factor(ncw$d11_mil,
                              "No" = 0,
                              "Yes" = 1)
ncw$d11_mil_n <- as.numeric(as.character(ncw$d11_mil_n))

ncw$d12_religion_n <- recode_factor(ncw$d12_religion,
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
ncw$d12_religion_n <- as.numeric(as.character(ncw$d12_religion_n))
ncw$d13_religion_important_n <- recode_factor(ncw$d13_religion_important,
                                             "Not important at all" = 0,
                                             "A little important" = 1,
                                             "Moderately important" = 2,
                                             "Very important" = 3,
                                             "Extremely important" = 4)
ncw$d13_religion_important_n <- as.numeric(as.character(ncw$d13_religion_important_n))
ncw$d13_religion_important <- factor(ncw$d13_religion_important, levels=c('Not important at all', 'A little important', 'Moderately important',
                                                                        'Very important', 'Extremely important'))
ncw$d14_religion_service_n <- recode_factor(ncw$d14_religion_service,
                                           "Never" = 0,
                                           "A few times a year" = 1,
                                           "Once or twice a month" = 2,
                                           "Almost every week" = 3,
                                           "Every week" = 4)
ncw$d14_religion_service_n <- as.numeric(as.character(ncw$d14_religion_service_n))
ncw$d14_religion_service <- factor(ncw$d14_religion_service, levels=c('Never', 'A few times a year', 'Once or twice a month',
                                                                    'Almost every week', 'Every week'))

#ANES

write.csv(ncw, "Data/nk_data_clean.csv")

