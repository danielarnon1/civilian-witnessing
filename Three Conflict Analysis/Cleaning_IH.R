
# REQUIRES: Data/ih.csv
# REQUIRES: Load Packages.R

################# Dependencies #################
source("Load Packages.R")

################# Import Data #################
cw_raw <- c("Data/ih.csv")
cw_raw <- lapply(cw_raw, read.csv) %>% bind_rows()

################# Data Cleaning #################
cw <- cw_raw %>% dplyr::mutate(cw_raw, id = row_number())
cw <- filter(cw, cw$Finished == TRUE)
cw <- filter(cw, cw$Consent.2 == "Yes, I am")

# Attention Check
cw$attentioncheck <- paste0(cw$AIC, cw$AIP, cw$AIS, cw$APC, cw$APP, cw$APS)
cw$attentioncheck <- ifelse(cw$condition == "Control", "control", cw$attentioncheck)

# Donation y/n
cw$donate <- paste0(cw$DIC, cw$DIP, cw$DIS, cw$DPC, cw$DPP, cw$DPS)
cw$donate <- ifelse(cw$condition == "Control", "Control", cw$donate)
cw$donate <- as.factor(cw$donate)

# Donation Amount
cw$DICa[is.na(cw$DICa)] <- ""
cw$DIPa[is.na(cw$DIPa)] <- ""
cw$DISa[is.na(cw$DISa)] <- ""
cw$DPCa[is.na(cw$DPCa)] <- ""
cw$DPPa[is.na(cw$DPPa)] <- ""
cw$DPSa[is.na(cw$DPSa)] <- ""
cw$donate_amt <- paste0(cw$DICa, cw$DIPa, cw$DISa, cw$DPCa, cw$DPPa, cw$DPSa)
cw$donate_amt <- ifelse(cw$condition == "Control", "control", cw$donate_amt)
cw$donate_amt <- ifelse(cw$donate == "No", 0, cw$donate_amt)
cw$donate_amt_n <- as.numeric(cw$donate_amt)

# Remove Variables
cw_full <- cw
cw <- subset(cw, select = -c(StartDate, EndDate, Status, IPAddress, Progress, Finished, ResponseId, RecipientLastName, 
                                   RecipientFirstName, RecipientEmail, ExternalReference, LocationLatitude, LocationLongitude,
                                   DistributionChannel, UserLanguage, Q_RecaptchaScore, Consent.1, Consent.2,
                                   AIC, DIC, DICa, AIP, DIP, DIPa, AIS, DIS, DISa, APC, DPC, DPCa, APP, DPP, DPPa, APS, DPS, DPSa))

# Rename Variables
names(cw)[names(cw) == 'Duration..in.seconds.'] <- 'time'
names(cw)[names(cw) == 'RecordedDate'] <- 'date'

# Pre-Treatment (1-7)
names(cw)[names(cw) == 'Pre.Treatment.1'] <- 'pr1_importantprob'
names(cw)[names(cw) == 'Pre.Treatment.2_1'] <- 'pr2_followtopics_election'
names(cw)[names(cw) == 'Pre.Treatment.2_2'] <- 'pr2_followtopics_ihw'
names(cw)[names(cw) == 'Pre.Treatment.2_3'] <- 'pr2_followtopics_ukraine'
names(cw)[names(cw) == 'Pre.Treatment.2_4'] <- 'pr2_followtopics_sudan'
names(cw)[names(cw) == 'Pre.Treatment.2_5'] <- 'pr2_followtopics_myanmar'
names(cw)[names(cw) == 'Pre.Treatment.3'] <- 'pr3_greatpowers'
names(cw)[names(cw) == 'Pre.Treatment.4'] <- 'pr4_milforce'
names(cw)[names(cw) == 'Pre.Treatment.5'] <- 'pr5_sympathize_ihw'
names(cw)[names(cw) == 'Pre.Treatment.6'] <- 'pr6_sympathize_ukraine'
names(cw)[names(cw) == 'Pre.Treatment.7a'] <- 'pr7_media_time'
names(cw)[names(cw) == 'Pre.Treatment.7b'] <- 'pr7_media_platform'
names(cw)[names(cw) == 'Pre.Treatment.7b_11_TEXT'] <- 'pr7_media_platform_text'
names(cw)[names(cw) == 'Pre.Treatment.7c'] <- 'pr7_media_mostactive'
names(cw)[names(cw) == 'Pre.Treatment.7c_11_TEXT'] <- 'pr7_media_mostactive_text'

# Immediate Post Treatment (1-4)
names(cw)[names(cw) == 'Post.Treatment.1'] <- 'po1_postfreq'
names(cw)[names(cw) == 'Post.Treatment.2'] <- 'po2_sharepost'
names(cw)[names(cw) == 'Post.Treatment.3'] <- 'po3_3words'
names(cw)[names(cw) == 'Post.Treatment.4_1'] <- 'po4_feeling_afraid'
names(cw)[names(cw) == 'Post.Treatment.4_2'] <- 'po4_feeling_exhausted'
names(cw)[names(cw) == 'Post.Treatment.4_3'] <- 'po4_feeling_angry'
names(cw)[names(cw) == 'Post.Treatment.4_4'] <- 'po4_feeling_sad'

# Post-Treatment - Foreign Policy - Attitudes (5-8)
names(cw)[names(cw) == 'Post.Treatment.5'] <- 'po5_sympathies'
names(cw)[names(cw) == 'Post.Treatment.6'] <- 'po6_think'
names(cw)[names(cw) == 'Post.Treatment.7'] <- 'po7_usaction'
names(cw)[names(cw) == 'Post.Treatment.8_1'] <- 'po8_leaderattention_ukraine'
names(cw)[names(cw) == 'Post.Treatment.8_2'] <- 'po8_leaderattention_ihw'
names(cw)[names(cw) == 'Post.Treatment.8_3'] <- 'po8_leaderattention_myanmar'
names(cw)[names(cw) == 'Post.Treatment.8_4'] <- 'po8_leaderattention_sudan'

# Post-Treatment - Foreign Policy - Goals (9-12)
names(cw)[names(cw) == 'Post.Treatment.9'] <- 'po9_important_recoverhostages'
names(cw)[names(cw) == 'Post.Treatment.10'] <- 'po10_important_ceasefire'
names(cw)[names(cw) == 'Post.Treatment.11'] <- 'po11_important_humanitarian'
names(cw)[names(cw) == 'Post.Treatment.12'] <- 'po12_important_aidisraelmil'

# Post-Treatment - Domestic - Polarization (13-14)
names(cw)[names(cw) == 'Post.Treatment.13_1'] <- 'po13_favor_dems'
names(cw)[names(cw) == 'Post.Treatment.13_2'] <- 'po13_favor_repubs'
names(cw)[names(cw) == 'Post.Treatment.13_3'] <- 'po13_favor_harris'
names(cw)[names(cw) == 'Post.Treatment.13_4'] <- 'po13_favor_trump'
names(cw)[names(cw) == 'Post.Treatment.14_5'] <- 'po14_favor_israelsupport'
names(cw)[names(cw) == 'Post.Treatment.14_6'] <- 'po14_favor_palessupport'
names(cw)[names(cw) == 'Post.Treatment.14_7'] <- 'po14_favor_jews'
names(cw)[names(cw) == 'Post.Treatment.14_8'] <- 'po14_favor_muslims'

# Post-Treatment - Domestic - Distance (15-21)
## Post-Treatment 15 is text
names(cw)[names(cw) == 'Post.Treatment.16'] <- 'po16_comfort_pales_friends'
names(cw)[names(cw) == 'Post.Treatment.17'] <- 'po17_comfort_pales_neighbors'
names(cw)[names(cw) == 'Post.Treatment.18'] <- 'po18_comfort_pales_childmarry'
names(cw)[names(cw) == 'Post.Treatment.19'] <- 'po19_comfort_israel_friends'
names(cw)[names(cw) == 'Post.Treatment.20'] <- 'po20_comfort_israel_neighbors'
names(cw)[names(cw) == 'Post.Treatment.21'] <- 'po21_comfort_israel_childmarry'

# Post-Treatment - Domestic - Voting (22)
names(cw)[names(cw) == 'Post.Treatment.22'] <- 'po22_vote_yn'
names(cw)[names(cw) == 'Post.Treatment.22a'] <- 'po22_vote_candid'
names(cw)[names(cw) == 'Post.Treatment.22a_3_TEXT'] <- 'po22_vote_candid_text'

# Demographics (1-15)
names(cw)[names(cw) == 'Demographics.1'] <- 'd1_ideology'
names(cw)[names(cw) == 'Demographics.2'] <- 'd2_party'
names(cw)[names(cw) == 'Demographics.3'] <- 'd3_pmisrael'
names(cw)[names(cw) == 'Demographics.4'] <- 'd4_groupattack'
names(cw)[names(cw) == 'Demographics.5'] <- 'd5_ihwdeaths'
names(cw)[names(cw) == 'Demographics.6'] <- 'd6_age'
names(cw)[names(cw) == 'Demographics.7'] <- 'd7_gender'
names(cw)[names(cw) == 'Demographics.7_4_TEXT'] <- 'd7_gender_text'
names(cw)[names(cw) == 'Demographics.8'] <- 'd8_race'
names(cw)[names(cw) == 'Demographics.8_7_TEXT'] <- 'd8_race_text'
names(cw)[names(cw) == 'Demographics.9'] <- 'd9_educ'
names(cw)[names(cw) == 'Demographics.10'] <- 'd10_income'
names(cw)[names(cw) == 'Demographics.11'] <- 'd11_mil'
names(cw)[names(cw) == 'Demographics.12'] <- 'd12_religion'
names(cw)[names(cw) == 'Demographics.12_10_TEXT'] <- 'd12_religion_text'
names(cw)[names(cw) == 'Demographics.13'] <- 'd13_religion_important'
names(cw)[names(cw) == 'Demographics.14'] <- 'd14_religion_service'
names(cw)[names(cw) == 'Demographics.15'] <- 'd15_protest'

# Reorder Variables
cw <- cw %>% dplyr::select(id, date, time, condition, attentioncheck, donate, donate_amt, everything())

# Categorical Variables
## Condition
cw$personal <- ifelse(cw$condition == "Condition_Israel_CW" |
                        cw$condition == "Condition_Israel_Personal" |
                        cw$condition == "Condition_Palestine_CW" |
                        cw$condition == "Condition_Palestine_Personal", 1, 0)
cw$personal <- as.numeric(as.character(cw$personal))
cw$stats <- ifelse(cw$condition == "Condition_Israel_Statistics" |
                        cw$condition == "Condition_Palestine_Statistics", 1, 0)
cw$stats <- as.numeric(as.character(cw$stats))
cw$control <- ifelse(cw$condition == "Control", 1, 0)
cw$control <- as.numeric(as.character(cw$control))

cw$condition_c <- ifelse(cw$personal == 1, "personal", "")
cw$condition_c <- ifelse(cw$stats == 1, "stats", cw$condition_c)
cw$condition_c <- ifelse(cw$control == 1, "control", cw$condition_c)
cw$condition_c <- relevel(cw$condition_c, ref = "control")

cw$pales <- ifelse(cw$condition == "Condition_Palestine_CW" |
                     cw$condition == "Condition_Palestine_Personal" |
                     cw$condition == "Condition_Palestine_Statistics", 1, 0)
cw$pales <- as.numeric(as.character(cw$pales))
cw$is <- ifelse(cw$condition == "Condition_Israel_CW" |
                     cw$condition == "Condition_Israel_Personal" |
                     cw$condition == "Condition_Israel_Statistics", 1, 0)
cw$is <- as.numeric(as.character(cw$is))
cw$condition_ipc <- ifelse(cw$pales == 1, "palestinian", "")
cw$condition_ipc <- ifelse(cw$is == 1, "israeli", cw$condition_ipc)
cw$condition_ipc <- ifelse(cw$control == 1, "control", cw$condition_ipc)
cw$condition_ipc <- as.factor(cw$condition_ipc)

cw$condition_cw <- ifelse(cw$condition == "Condition_Israel_CW" |
                        cw$condition == "Condition_Palestine_CW", 1, 0)
cw$condition_cw <- as.numeric(as.character(cw$condition_cw))
cw$condition_personal <- ifelse(cw$condition == "Condition_Israel_Personal" |
                            cw$condition == "Condition_Palestine_Personal", 1, 0)
cw$condition_personal <- as.numeric(as.character(cw$condition_personal))
cw$stats <- ifelse(cw$condition == "Condition_Israel_Statistics" |
                     cw$condition == "Condition_Palestine_Statistics", 1, 0)
cw$stats <- as.numeric(as.character(cw$stats))
cw$control <- ifelse(cw$condition == "Control", 1, 0)
cw$control <- as.numeric(as.character(cw$control))

cw$condition_vers <- ifelse(cw$condition_cw == 1, "CW", "")
cw$condition_vers <- ifelse(cw$condition_personal == 1, "Personal", cw$condition_vers)
cw$condition_vers <- ifelse(cw$stats == 1, "Statistics", cw$condition_vers)
cw$condition_vers <- ifelse(cw$control == 1, "Control", cw$condition_vers)
cw$condition_vers <- relevel(cw$condition_vers, ref = "Control")

cw$condition_sep <- ifelse(cw$condition == "Condition_Israel_CW", "Israel_CW", "")
cw$condition_sep <- ifelse(cw$condition == "Condition_Palestine_CW", "Palestine_CW", cw$condition_sep)
cw$condition_sep <- ifelse(cw$condition == "Condition_Israel_Personal", "Israel_News", cw$condition_sep)
cw$condition_sep <- ifelse(cw$condition == "Condition_Palestine_Personal", "Palestine_News", cw$condition_sep)
cw$condition_sep <- ifelse(cw$condition == "Condition_Israel_Statistics", "Israel_News", cw$condition_sep)
cw$condition_sep <- ifelse(cw$condition == "Condition_Palestine_Statistics", "Palestine_News", cw$condition_sep)
cw$condition_sep <- ifelse(cw$condition == "Control", "Control", cw$condition_sep)
cw$condition_sep <- relevel(cw$condition_sep, ref = "Control")

cw$condition_sep2 <- ifelse(cw$condition == "Condition_Israel_CW", "Israel_Personal", "")
cw$condition_sep2 <- ifelse(cw$condition == "Condition_Palestine_CW", "Palestine_Personal", cw$condition_sep2)
cw$condition_sep2 <- ifelse(cw$condition == "Condition_Israel_Personal", "Israel_Personal", cw$condition_sep2)
cw$condition_sep2 <- ifelse(cw$condition == "Condition_Palestine_Personal", "Palestine_Personal", cw$condition_sep2)
cw$condition_sep2 <- ifelse(cw$condition == "Condition_Israel_Statistics", "Israel_Statistics", cw$condition_sep2)
cw$condition_sep2 <- ifelse(cw$condition == "Condition_Palestine_Statistics", "Palestine_Statistics", cw$condition_sep2)
cw$condition_sep2 <- ifelse(cw$condition == "Control", "Control", cw$condition_sep2)
cw$condition_sep2 <- relevel(cw$condition_sep2, ref = "Control")

cw$condition_sep3 <- ifelse(cw$condition == "Condition_Israel_CW", "Israel_Personal", "")
cw$condition_sep3 <- ifelse(cw$condition == "Condition_Palestine_CW", "Palestine_Personal", cw$condition_sep3)
cw$condition_sep3 <- ifelse(cw$condition == "Condition_Israel_Personal", "Israel_Personal", cw$condition_sep3)
cw$condition_sep3 <- ifelse(cw$condition == "Condition_Palestine_Personal", "Palestine_Personal", cw$condition_sep3)
cw$condition_sep3 <- ifelse(cw$condition == "Condition_Israel_Statistics", "Israel_Statistics", cw$condition_sep3)
cw$condition_sep3 <- ifelse(cw$condition == "Condition_Palestine_Statistics", "Palestine_Statistics", cw$condition_sep3)
cw$condition_sep3 <- ifelse(cw$condition == "Control", "Control", cw$condition_sep3)
cw$condition_sep3 <- relevel(cw$condition_sep3, ref = "Control")

## Pre-Treatment
cw$pr2_followtopics_election_n <- recode_factor(cw$pr2_followtopics_election,
                                                "Not closely at all" = 1,
                                                "Not too closely" = 2,
                                                "Somewhat closely" = 3,
                                                "Very closely" = 4,
                                                "Extremely closely" = 5)
cw$pr2_followtopics_election_n <- as.numeric(as.character(cw$pr2_followtopics_election_n))
cw$pr2_followtopics_ihw_n <- recode_factor(cw$pr2_followtopics_ihw,
                                                "Not closely at all" = 1,
                                                "Not too closely" = 2,
                                                "Somewhat closely" = 3,
                                                "Very closely" = 4,
                                                "Extremely closely" = 5)
cw$pr2_followtopics_ihw_n <- as.numeric(as.character(cw$pr2_followtopics_ihw_n))
cw$att <- ifelse(cw$pr2_followtopics_ihw == "Not closely at all" | cw$pr2_followtopics_ihw == "Not too closely", "low_att", "")
cw$att <- ifelse(cw$pr2_followtopics_ihw == "Somewhat closely" | cw$pr2_followtopics_ihw == "Very closely" | cw$pr2_followtopics_ihw == "Extremely closely", "high_att", cw$att)
cw$att_n <- ifelse(cw$att == "high_att", 1, 0)

cw$pr2_followtopics_ukraine_n <- recode_factor(cw$pr2_followtopics_ukraine,
                                                "Not closely at all" = 1,
                                                "Not too closely" = 2,
                                                "Somewhat closely" = 3,
                                                "Very closely" = 4,
                                                "Extremely closely" = 5)
cw$pr2_followtopics_ukraine_n <- as.numeric(as.character(cw$pr2_followtopics_ukraine_n))
cw$pr2_followtopics_sudan_n <- recode_factor(cw$pr2_followtopics_sudan,
                                                "Not closely at all" = 1,
                                                "Not too closely" = 2,
                                                "Somewhat closely" = 3,
                                                "Very closely" = 4,
                                                "Extremely closely" = 5)
cw$pr2_followtopics_sudan_n <- as.numeric(as.character(cw$pr2_followtopics_sudan_n))
cw$pr2_followtopics_myanmar_n <- recode_factor(cw$pr2_followtopics_myanmar,
                                                "Not closely at all" = 1,
                                                "Not too closely" = 2,
                                                "Somewhat closely" = 3,
                                                "Very closely" = 4,
                                                "Extremely closely" = 5)
cw$pr2_followtopics_myanmar_n <- as.numeric(as.character(cw$pr2_followtopics_myanmar_n))

cw$pr3_greatpowers <- as.character(cw$pr3_greatpowers)
cw$pr3_greatpowers <- ifelse(cw$pr3_greatpowers == "Neither Agree Nor DIsagree", "Neither Agree Nor Disagree", cw$pr3_greatpowers)
cw$pr3_greatpowers <- as.factor(cw$pr3_greatpowers)
cw$pr3_greatpowers_n <- recode_factor(cw$pr3_greatpowers,
                                   "Strongly Disagree" = 0,
                                   "Disagree" = 1,
                                   "Neither Agree Nor Disagree" = 2,
                                   "Agree" = 3,
                                   "Strongly Agree" = 4)
cw$pr3_greatpowers_n <- as.numeric(as.character(cw$pr3_greatpowers_n))
cw$pr3_greatpowers <- factor(cw$pr3_greatpowers, levels=c("Strongly Disagree", "Disagree", "Neither Agree Nor Disagree", "Agree", "Strongly Agree", NA))
cw$pr4_milforce <- as.character(cw$pr4_milforce)
cw$pr4_milforce <- ifelse(cw$pr4_milforce == "Neither Agree Nor DIsagree", "Neither Agree Nor Disagree", cw$pr4_milforce)
cw$pr4_milforce <- as.factor(cw$pr4_milforce)
cw$pr4_milforce_n <- recode_factor(cw$pr4_milforce,
                                               "Strongly Disagree" = 0,
                                               "Disagree" = 1,
                                               "Neither Agree Nor Disagree" = 2,
                                               "Agree" = 3,
                                               "Strongly Agree" = 4)
cw$pr4_milforce_n <- as.numeric(as.character(cw$pr4_milforce_n))
cw$pr4_milforce <- factor(cw$pr4_milforce, levels=c("Strongly Disagree", "Disagree", "Neither Agree Nor Disagree", "Agree", "Strongly Agree", NA))

cw$pr5_sympathize_ihw_n <- recode_factor(cw$pr5_sympathize_ihw,
                                         "Neither" = 0,
                                         "More with the Palestinians" = 1,
                                         "Both equally" = 2,
                                         "More with the Israelis" = 3)
cw$pr5_sympathize_ihw_n <- as.numeric(as.character(cw$pr5_sympathize_ihw_n))
cw$pr5_sympathize_ihw2 <- recode_factor(cw$pr5_sympathize_ihw,
                                          "More with the Palestinians" = "Pales",
                                          "Both equally" = "Both",
                                          "More with the Israelis" = "Is")
cw$pr5_sympathize_ihw2 <- relevel(cw$pr5_sympathize_ihw2, ref = "Both")
cw$pr5_sympathize_ihw_n2 <- recode_factor(cw$pr5_sympathize_ihw,
                                         "More with the Palestinians" = 0,
                                         "Both equally" = 1,
                                         "More with the Israelis" = 2)
cw$pr5_sympathize_ihw_n2 <- as.numeric(as.character(cw$pr5_sympathize_ihw_n2))
cw$pr5_sympathize_ihw_bnt <- as.character(cw$pr5_sympathize_ihw)
cw$pr5_sympathize_ihw_bnt <- ifelse(cw$pr5_sympathize_ihw == "Neither", "Both_Neither", cw$pr5_sympathize_ihw_bnt)
cw$pr5_sympathize_ihw_bnt <- ifelse(cw$pr5_sympathize_ihw_bn == "Both equally", "Both_Neither", cw$pr5_sympathize_ihw_bnt)
cw$pr5_sympathize_ihw_bnt <- as.factor(cw$pr5_sympathize_ihw_bnt)


cw$pr5_sympathize_ihw_otherside_n <- ifelse(cw$condition == "Condition_Palestine_CW" & cw$pr5_sympathize_ihw == "More with the Israelis", 1, 0)
cw$pr5_sympathize_ihw_otherside_n <- ifelse(cw$condition == "Condition_Palestine_Personal" & cw$pr5_sympathize_ihw == "More with the Israelis", 1, cw$pr5_sympathize_ihw_otherside_n)
cw$pr5_sympathize_ihw_otherside_n <- ifelse(cw$condition == "Condition_Palestine_Statistics" & cw$pr5_sympathize_ihw == "More with the Israelis", 1, cw$pr5_sympathize_ihw_otherside_n)
cw$pr5_sympathize_ihw_otherside_n <- ifelse(cw$condition == "Condition_Israel_CW" & cw$pr5_sympathize_ihw == "More with the Palestinians", 1, cw$pr5_sympathize_ihw_otherside_n)
cw$pr5_sympathize_ihw_otherside_n <- ifelse(cw$condition == "Condition_Israel_Personal" & cw$pr5_sympathize_ihw == "More with the Palestinians", 1, cw$pr5_sympathize_ihw_otherside_n)
cw$pr5_sympathize_ihw_otherside_n <- ifelse(cw$condition == "Condition_Israel_Statistics" & cw$pr5_sympathize_ihw == "More with the Palestinians", 1, cw$pr5_sympathize_ihw_otherside_n)
cw$pr5_sympathize_ihw_otherside_n <- as.numeric(as.character(cw$pr5_sympathize_ihw_otherside_n))
cw$pr5_sympathize_ihw_pales <- as.numeric(as.character(ifelse(cw$pr5_sympathize_ihw == "More with the Palestinians", 1, 0)))
cw$pr5_sympathize_ihw_pales_f <- as.factor(cw$pr5_sympathize_ihw_pales)
cw$pr5_sympathize_ihw_is <- as.numeric(as.character(ifelse(cw$pr5_sympathize_ihw == "More with the Israelis", 1, 0)))
cw$pr5_sympathize_ihw_both <- as.numeric(as.character(ifelse(cw$pr5_sympathize_ihw == "Both equally", 1, 0)))
cw$pr5_sympathize_ihw_neither <- as.numeric(as.character(ifelse(cw$pr5_sympathize_ihw == "Neither", 1, 0)))
cw$pr5_sympathize_ihw_bn <- as.numeric(as.character(ifelse(cw$pr5_sympathize_ihw == "Both equally", 1, 0)))
cw$pr5_sympathize_ihw_bn <- as.numeric(as.character(ifelse(cw$pr5_sympathize_ihw == "Neither", 1, cw$pr5_sympathize_ihw_bn)))
cw$pr6_sympathize_ukraine_n <- recode_factor(cw$pr6_sympathize_ukraine,
                                         "Neither" = 0,
                                         "More with the Russians" = 1,
                                         "Both equally" = 2,
                                         "More with the Ukrainians" = 3)
cw$pr6_sympathize_ukraine_n <- as.numeric(as.character(cw$pr6_sympathize_ukraine_n))

cw$pr7_media_time_n <- recode_factor(cw$pr7_media_time,
                                     "I do not have social media" = 0,
                                     "Rarely on social media" = 1,
                                     "A few hours per week" = 2,
                                     "About an hour per day" = 3,
                                     "Multiple hours per day" = 4)
cw$pr7_media_time_n <- as.numeric(as.character(cw$pr7_media_time_n))
cw$pr7_media_time <- factor(cw$pr7_media_time, levels=c("I do not have social media", "Rarely on social media", 
                                                        "A few hours per week", "About an hour per day", "Multiple hours per day", NA))
cw$soc <- ifelse(cw$pr7_media_time == "I do not have social media" | cw$pr7_media_time == "Rarely on social media" | cw$pr7_media_time == "A few hours per week", "low_soc", "")
cw$soc <- ifelse(cw$pr7_media_time == "About an hour per day" | cw$pr7_media_time == "Multiple hours per day", "high_soc", cw$soc)
cw$soc <- factor(cw$soc, levels=c("low_soc", "high_soc"))
cw$soc <- relevel(cw$soc, ref = "high_soc")
cw$soc_n <- ifelse(cw$soc == "high_soc", 1, 0)

cw$socatt <- ifelse(cw$soc == "low_soc" & cw$att == "low_att", "lowlow", "")
cw$socatt <- ifelse(cw$soc == "high_soc" & cw$att == "low_att", "highlow", cw$socatt)
cw$socatt <- ifelse(cw$soc == "low_soc" & cw$att == "high_att", "lowhigh", cw$socatt)
cw$socatt <- ifelse(cw$soc == "high_soc" & cw$att == "high_att", "highhigh", cw$socatt)
cw$socatt_n <- ifelse(cw$socatt == "lowlow", 0, 99)
cw$socatt_n <- ifelse(cw$socatt == "highlow", 1, cw$socatt_n)
cw$socatt_n <- ifelse(cw$socatt == "lowhigh", 2, cw$socatt_n)
cw$socatt_n <- ifelse(cw$socatt == "highhigh", 3, cw$socatt_n)
cw$socatt_n <- as.numeric(as.character(cw$socatt_n))

cw$pr7_media_platform <- as.character(cw$pr7_media_platform)
cw$pr7_media_platform <- ifelse(cw$pr7_media_time == "I do not have social media", "I do not have social media", cw$pr7_media_platform)
socialmedia <- data.frame(matrix(0, nrow = 12, ncol = 0))
socialmedia$platform <- c("TikTok", "Instagram", "Twitter/X", "Facebook", "Threads", "Bluesky", "Linkedin", "Reddit", "Gab",
                          "Truth Social", "Other", "None of the Above")
socialmedia$count <- c(sum(cw$pr7_media_platform_tiktok), sum(cw$pr7_media_platform_instagram), sum(cw$pr7_media_platform_twitter), 
                       sum(cw$pr7_media_platform_facebook), sum(cw$pr7_media_platform_threads), sum(cw$pr7_media_platform_bluesky),
                       sum(cw$pr7_media_platform_linkedin), sum(cw$pr7_media_platform_reddit), sum(cw$pr7_media_platform_gab), 
                       sum(cw$pr7_media_platform_truthsocial), sum(cw$pr7_media_platform_other), sum(cw$pr7_media_platform_noneofabove))

cw$pr7_media_platform_tiktok <- as.integer(as.logical(str_detect(cw$pr7_media_platform, "TikTok")))
cw$pr7_media_platform_instagram <- as.integer(as.logical(str_detect(cw$pr7_media_platform, "Instagram")))
cw$pr7_media_platform_twitter <- as.integer(as.logical(str_detect(cw$pr7_media_platform, "Twitter/X")))
cw$pr7_media_platform_facebook <- as.integer(as.logical(str_detect(cw$pr7_media_platform, "Facebook")))
cw$pr7_media_platform_threads <- as.integer(as.logical(str_detect(cw$pr7_media_platform, "Threads")))
cw$pr7_media_platform_bluesky <- as.integer(as.logical(str_detect(cw$pr7_media_platform, "Bluesky")))
cw$pr7_media_platform_linkedin <- as.integer(as.logical(str_detect(cw$pr7_media_platform, "LinkedIn")))
cw$pr7_media_platform_reddit <- as.integer(as.logical(str_detect(cw$pr7_media_platform, "Reddit")))
cw$pr7_media_platform_gab <- as.integer(as.logical(str_detect(cw$pr7_media_platform, "Gab")))
cw$pr7_media_platform_truthsocial <- as.integer(as.logical(str_detect(cw$pr7_media_platform, "Truth Social")))
cw$pr7_media_platform_other <- as.integer(as.logical(str_detect(cw$pr7_media_platform, "Other")))
cw$pr7_media_platform_noneofabove <- as.integer(as.logical(str_detect(cw$pr7_media_platform, "None of the above")))

cw$pr7_media_mostactive <- as.character(cw$pr7_media_mostactive)
cw$pr7_media_mostactive <- ifelse(cw$pr7_media_time == "I do not have social media", "I do not have social media", cw$pr7_media_mostactive)
cw$pr7_media_mostactive <- as.factor(cw$pr7_media_mostactive)
## other social media variables

## Post-Treatment - Foreign Policy
cw$po1_postfreq_n <- recode_factor(cw$po1_postfreq,
                                     "I have never seen a post like this before" = 0,
                                     "Rarely, if ever" = 1,
                                     "A few times per week" = 2,
                                     "At least once per day" = 3,
                                     "At least a few times per day" = 4)
cw$po1_postfreq_n <- as.numeric(as.character(cw$po1_postfreq_n))
cw$po2_sharepost_n <- recode_factor(cw$po2_sharepost,
                                   "Not likely at all" = 0,
                                   "Not very likely" = 1,
                                   "Somewhat likely" = 2,
                                   "Likely" = 3,
                                   "Very likely" = 4)
cw$po2_sharepost_n <- as.numeric(as.character(cw$po2_sharepost_n))

cw$po4_feeling_afraid_n <- recode_factor(cw$po4_feeling_afraid,
                                    "Not at all" = 0,
                                    "Somewhat" = 1,
                                    "Strongly" = 2,
                                    "Very strongly" = 3)
cw$po4_feeling_afraid_n <- as.numeric(as.character(cw$po4_feeling_afraid_n))
cw$po4_feeling_exhausted_n <- recode_factor(cw$po4_feeling_exhausted,
                                            "Not at all" = 0,
                                            "Somewhat" = 1,
                                            "Strongly" = 2,
                                            "Very strongly" = 3)
cw$po4_feeling_exhausted_n <- as.numeric(as.character(cw$po4_feeling_exhausted_n))
cw$po4_feeling_angry_n <- recode_factor(cw$po4_feeling_angry,
                                         "Not at all" = 0,
                                         "Somewhat" = 1,
                                         "Strongly" = 2,
                                         "Very strongly" = 3)
cw$po4_feeling_angry_n <- as.numeric(as.character(cw$po4_feeling_angry_n))
cw$po4_feeling_sad_n <- recode_factor(cw$po4_feeling_sad,
                                         "Not at all" = 0,
                                         "Somewhat" = 1,
                                         "Strongly" = 2,
                                         "Very strongly" = 3)
cw$po4_feeling_sad_n <- as.numeric(as.character(cw$po4_feeling_sad_n))

cw$po5_sympathies_n <- recode_factor(cw$po5_sympathies,
                                      "Not sure" = 0,
                                      "With neither of them" = 0,
                                      "Entirely with the Palestinian people" = 1,
                                      "Mostly with the Palestinian people but also somewhat with the Israeli people" = 2,
                                      "Equally with both of them" = 3,
                                      "Mostly with the Israeli people but also somewhat with the Palestinian people" = 4,
                                      "Entirely with the Israeli people" = 5)
cw$po5_sympathies_n <- as.numeric(as.character(cw$po5_sympathies_n))
cw$po5_sympathies_nr <- recode_factor(cw$po5_sympathies,
                                     "Not sure" = 0,
                                     "With neither of them" = 0,
                                     "Entirely with the Palestinian people" = 1,
                                     "Mostly with the Palestinian people but also somewhat with the Israeli people" = 1,
                                     "Equally with both of them" = 2,
                                     "Mostly with the Israeli people but also somewhat with the Palestinian people" = 3,
                                     "Entirely with the Israeli people" = 3)
cw$po5_sympathies_nr <- as.numeric(as.character(cw$po5_sympathies_nr))
cw$po5_sympathies_nr2 <- recode_factor(cw$po5_sympathies,
                                      "Entirely with the Palestinian people" = 0,
                                      "Mostly with the Palestinian people but also somewhat with the Israeli people" = 0,
                                      "Equally with both of them" = 1,
                                      "Mostly with the Israeli people but also somewhat with the Palestinian people" = 2,
                                      "Entirely with the Israeli people" = 2)
cw$po5_sympathies_nr2 <- as.numeric(as.character(cw$po5_sympathies_nr2))
cw$p5_diff <- cw$po5_sympathies_nr2-cw$pr5_sympathize_ihw_n2
cw$po6_think_n <- recode_factor(cw$po6_think,
                                     "Don’t know enough to have an opinion" = 0,
                                     "Israel has gone too far, and its military actions aren’t justified" = 1,
                                     "Israel is defending its interests, and its military actions are justified" = 2)
cw$po6_think_n <- as.numeric(as.character(cw$po6_think_n))
cw$po7_usaction_n <- recode_factor(cw$po7_usaction,
                                     "Don’t know enough to have an opinion" = 0,
                                     "The U.S. should reduce military aid to Israel until it ends its war with Hamas" = 1,
                                     "The U.S. should exert diplomatic pressure on Israel but not reduce military aid" = 2,
                                     "The U.S. should let Israel pursue whatever policy it thinks best without any U.S. pressure" = 3)
cw$po7_usaction_n <- as.numeric(as.character(cw$po7_usaction_n))
cw$po8_leaderattention_ukraine_n <- recode_factor(cw$po8_leaderattention_ukraine,
                                   "Too little attention" = 1,
                                   "Right amount of attention" = 2,
                                   "Too much attention" = 3)
cw$po8_leaderattention_ukraine_n <- as.numeric(as.character(cw$po8_leaderattention_ukraine_n))
cw$po8_leaderattention_ihw_n <- recode_factor(cw$po8_leaderattention_ihw,
                                   "Too little attention" = 1,
                                   "Right amount of attention" = 2,
                                   "Too much attention" = 3)
cw$po8_leaderattention_ihw_n <- as.numeric(as.character(cw$po8_leaderattention_ihw_n))
cw$po8_leaderattention_myanmar_n <- recode_factor(cw$po8_leaderattention_myanmar,
                                   "Too little attention" = 1,
                                   "Right amount of attention" = 2,
                                   "Too much attention" = 3)
cw$po8_leaderattention_myanmar_n <- as.numeric(as.character(cw$po8_leaderattention_myanmar_n))
cw$po8_leaderattention_sudan_n <- recode_factor(cw$po8_leaderattention_sudan,
                                   "Too little attention" = 1,
                                   "Right amount of attention" = 2,
                                   "Too much attention" = 3)
cw$po8_leaderattention_sudan_n <- as.numeric(as.character(cw$po8_leaderattention_sudan_n))

cw$po9_important_recoverhostages_n <- recode_factor(cw$po9_important_recoverhostages,
                                     "Not at all important" = 0,
                                     "Slightly important" = 1,
                                     "Moderately important" = 2,
                                     "Very important" = 3,
                                     "Extremely important" = 4)
cw$po9_important_recoverhostages_n <- as.numeric(as.character(cw$po9_important_recoverhostages_n))
cw$po10_important_ceasefire_n <- recode_factor(cw$po10_important_ceasefire,
                                                    "Not at all important" = 0,
                                                    "Slightly important" = 1,
                                                    "Moderately important" = 2,
                                                    "Very important" = 3,
                                                    "Extremely important" = 4)
cw$po10_important_ceasefire_n <- as.numeric(as.character(cw$po10_important_ceasefire_n))
cw$po11_important_humanitarian_n <- recode_factor(cw$po11_important_humanitarian,
                                                    "Not at all important" = 0,
                                                    "Slightly important" = 1,
                                                    "Moderately important" = 2,
                                                    "Very important" = 3,
                                                    "Extremely important" = 4)
cw$po11_important_humanitarian_n <- as.numeric(as.character(cw$po11_important_humanitarian_n))
cw$po12_important_aidisraelmil_n <- recode_factor(cw$po12_important_aidisraelmil,
                                                    "Not at all important" = 0,
                                                    "Slightly important" = 1,
                                                    "Moderately important" = 2,
                                                    "Very important" = 3,
                                                    "Extremely important" = 4)
cw$po12_important_aidisraelmil_n <- as.numeric(as.character(cw$po12_important_aidisraelmil_n))

## Post-Treatment - Domestic
cw$po16_comfort_pales_friends_n <- recode_factor(cw$po16_comfort_pales_friends,
                                                  "Not at all comfortable" = 0,
                                                  "Not too comfortable" = 1,
                                                  "Somewhat comfortable" = 2,
                                                  "Extremely comfortable" = 3)
cw$po16_comfort_pales_friends_n <- as.numeric(as.character(cw$po16_comfort_pales_friends_n))
cw$po17_comfort_pales_neighbors_n <- recode_factor(cw$po17_comfort_pales_neighbors,
                                                 "Not at all comfortable" = 0,
                                                 "Not too comfortable" = 1,
                                                 "Somewhat comfortable" = 2,
                                                 "Extremely comfortable" = 3)
cw$po17_comfort_pales_neighbors_n <- as.numeric(as.character(cw$po17_comfort_pales_neighbors_n))
cw$po18_comfort_pales_childmarry_n <- recode_factor(cw$po18_comfort_pales_childmarry,
                                                 "Not at all comfortable" = 0,
                                                 "Not too comfortable" = 1,
                                                 "Somewhat comfortable" = 2,
                                                 "Extremely comfortable" = 3)
cw$po18_comfort_pales_childmarry_n <- as.numeric(as.character(cw$po18_comfort_pales_childmarry_n))
cw$po19_comfort_israel_friends_n <- recode_factor(cw$po19_comfort_israel_friends,
                                                 "Not at all comfortable" = 0,
                                                 "Not too comfortable" = 1,
                                                 "Somewhat comfortable" = 2,
                                                 "Extremely comfortable" = 3)
cw$po19_comfort_israel_friends_n <- as.numeric(as.character(cw$po19_comfort_israel_friends_n))
cw$po20_comfort_israel_neighbors_n <- recode_factor(cw$po20_comfort_israel_neighbors,
                                                 "Not at all comfortable" = 0,
                                                 "Not too comfortable" = 1,
                                                 "Somewhat comfortable" = 2,
                                                 "Extremely comfortable" = 3)
cw$po20_comfort_israel_neighbors_n <- as.numeric(as.character(cw$po20_comfort_israel_neighbors_n))
cw$po21_comfort_israel_childmarry_n <- recode_factor(cw$po21_comfort_israel_childmarry,
                                                 "Not at all comfortable" = 0,
                                                 "Not too comfortable" = 1,
                                                 "Somewhat comfortable" = 2,
                                                 "Extremely comfortable" = 3)
cw$po21_comfort_israel_childmarry_n <- as.numeric(as.character(cw$po21_comfort_israel_childmarry_n))

cw$po22_vote_yn_n <- recode_factor(cw$po22_vote_yn,
                                                     "Prefer not to say" = 0,
                                                     "No" = 1,
                                                     "Yes" = 2)
cw$po22_vote_yn_n <- as.numeric(as.character(cw$po22_vote_yn_n))
cw$po22_vote_candid_n <- recode_factor(cw$po22_vote_candid,
                                                     "Other" = 0,
                                                     "Prefer not to say" = 0,
                                                     "Kamala Harris" = 1,
                                                     "Donald Trump" = 2)
cw$po22_vote_candid_n <- as.numeric(as.character(cw$po22_vote_candid_n))

# Demographics
cw$d1_ideology_n <- recode_factor(cw$d1_ideology,
                                   "Extremely Conservative" = 7,
                                   "Conservative" = 6,
                                   "Slightly Conservative" = 5,
                                   "Moderate, middle of the road" = 4,
                                   "Slightly Liberal" = 3,
                                   "Liberal" = 2,
                                   "Extremely Liberal" = 1)
cw$d1_ideology_n <- as.numeric(as.character(cw$d1_ideology_n))
cw$d2_party_n <- recode_factor(cw$d2_party,
                                  "Strong Republican" = 7,
                                  "Republican" = 6,
                                  "Independent, leaning  to Republican" = 5,
                                  "Independent" = 4,
                                  "Independent, leaning to Democrat" = 3,
                                  "Democrat" = 2,
                                  "Strong Democrat" = 1)
cw$d2_party_n <- as.numeric(as.character(cw$d2_party_n))
cw$d2_party <- factor(cw$d2_party, levels=c("Strong Democrat", "Democrat", "Independent, leaning to Democrat",
                                            "Independent", "Independent, leaning  to Republican", "Republican", "Strong Republican", NA))
cw$d2_party_3 <- ifelse(cw$d2_party == "Strong Republican" | cw$d2_party == "Republican" | cw$d2_party == "Independent, leaning  to Republican", "Republican", "")
cw$d2_party_3 <- ifelse(cw$d2_party == "Independent", "Independent", cw$d2_party_3)
cw$d2_party_3 <- ifelse(cw$d2_party == "Strong Democrat" | cw$d2_party == "Democrat" | cw$d2_party == "Independent, leaning to Democrat", "Democrat", cw$d2_party_3)
cw$d2_party_3 <- factor(cw$d2_party_3, levels=c("Republican", "Independent", "Democrat"))
cw$d2_party_3_n <- recode_factor(cw$d2_party_3,
                               "Republican" = 3,
                               "Independent" = 2,
                               "Democrat" = 1)
cw$d2_party_3_n <- as.numeric(as.character(cw$d2_party_3_n))


cw$d3_pmisrael_n <- recode_factor(cw$d3_pmisrael,
                                   "Sunak" = 0,
                                   "Gantz" = 0,
                                   "Fattah el-Sisi" = 0,
                                   "Not Sure" = 0,
                                   "Netanyahu" = 1)
cw$d3_pmisrael_n <- as.numeric(as.character(cw$d3_pmisrael_n))
cw$d4_groupattack_n <- recode_factor(cw$d4_groupattack,
                                   "Hezbollah" = 0,
                                   "Al Qaeda" = 0,
                                   "Taliban" = 0,
                                   "Not Sure" = 0,
                                   "Hamas" = 1)
cw$d4_groupattack_n <- as.numeric(as.character(cw$d4_groupattack_n))
cw$d5_ihwdeaths_n <- recode_factor(cw$d5_ihwdeaths,
                                     "Higher among Israelis" = 0,
                                     "About the Same" = 0,
                                     "Not Sure" = 0,
                                     "Higher among Palestinians" = 1)
cw$d5_ihwdeaths_n <- as.numeric(as.character(cw$d5_ihwdeaths_n))
cw$knowledge <- ifelse(cw$d3_pmisrael == "Netanyahu", 1, 0)
cw$knowledge <- ifelse(cw$d4_groupattack == "Hamas", cw$knowledge+1, cw$knowledge)
cw$knowledge <- ifelse(cw$d5_ihwdeaths == "Higher among Palestinians", cw$knowledge+1, cw$knowledge)
cw$knowledge_n <- cw$knowledge
cw$knowledge <- as.factor(cw$knowledge)

cw$d6_age_n <- 2024-cw$d6_age
cw$d6_age_gen[cw$d6_age >= 1928 & cw$d6_age <= 1945] <- 'Silent Gen'
cw$d6_age_gen[cw$d6_age >= 1946 & cw$d6_age <= 1964] <- 'Baby Boomer'
cw$d6_age_gen[cw$d6_age >= 1965 & cw$d6_age <= 1980] <- 'Gen X'
cw$d6_age_gen[cw$d6_age >= 1981 & cw$d6_age <= 1996] <- 'Millennial'
cw$d6_age_gen[cw$d6_age >= 1997 & cw$d6_age <= 2010] <- 'Gen Z'
cw$d6_age_gen <- as.factor(cw$d6_age_gen)
cw$d6_age_gen <- factor(cw$d6_age_gen, levels=c("Silent Gen", "Baby Boomer", "Gen X", "Millennial", "Gen Z", NA))
cw$d6_age_gen_n <- recode_factor(cw$d6_age_gen,
                                "Silent Gen" = 0,
                                "Baby Boomer" = 1,
                                "Gen X" = 2,
                                "Millennial" = 2,
                                "Gen Z" = 3)
cw$d6_age_gen_n <- as.numeric(as.character(cw$d6_age_gen_n))

cw$d7_gender_n <- recode_factor(cw$d7_gender,
                                   "Female" = 0,
                                   "Male" = 1,
                                   "Non-binary / third gender" = 2,
                                   "Prefer to specify:" = 2)
cw$d7_gender_n <- as.numeric(as.character(cw$d7_gender_n))
cw$d8_race_n <- recode_factor(cw$d8_race,
                               "American Indian and Alaskan Native" = 0,
                               "Asian" = 1,
                               "Black" = 2,
                               "Hispanic/Latino" = 3,
                               "Native Hawaiian and Other Pacific Islander" = 4,
                               "White, Non-Hispanic" = 5,
                               "Prefer to specify:" = 6)
cw$d8_race_n <- as.numeric(as.character(cw$d8_race_n))
cw$d9_educ_n <- recode_factor(cw$d9_educ,
                              "Less than high school" = 0,
                              "High school graduate" = 1,
                              "Some college, but no degree" = 2,
                              "2-year college degree" = 3,
                              "4-year college degree" = 4,
                              "Professional degree" = 5,
                              "Doctoral degree" = 6)
cw$d9_educ_n <- as.numeric(as.character(cw$d9_educ_n))
cw$d9_educ <- factor(cw$d9_educ, levels=c("Less than high school", "High school graduate", "Some college, but no degree",
                                          "2-year college degree", "4-year college degree", "Professional degree", "Doctoral degree", NA))
cw$d10_income_n <- recode_factor(cw$d10_income,
                              "$24,999 or less" = 0,
                              "$25,000 to $49,999" = 1,
                              "$50,000 to $74,999" = 2,
                              "$75,000 to $99,999" = 3,
                              "$100,000 or $149,999" = 4,
                              "$150,000 to $199,999" = 5,
                              "$200,000 to $249,999" = 6,
                              "$250,000 or more" = 7)
cw$d10_income_n <- as.numeric(as.character(cw$d10_income_n))
cw$d10_income <- factor(cw$d10_income, levels=c("$24,999 or less", "$25,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999", 
                                                "$100,000 or $149,999", "$150,000 to $199,999", "$200,000 to $249,999", "$250,000 or more", NA))
cw$d11_mil_n <- recode_factor(cw$d11_mil,
                                  "No" = 0,
                                  "Yes" = 1)
cw$d11_mil_n <- as.numeric(as.character(cw$d11_mil_n))

cw$d12_religion_n <- recode_factor(cw$d12_religion,
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
cw$d12_religion_n <- as.numeric(as.character(cw$d12_religion_n))
cw$d13_religion_important_n <- recode_factor(cw$d13_religion_important,
                                     "Not important at all" = 0,
                                     "A little important" = 1,
                                     "Moderately important" = 2,
                                     "Very important" = 3,
                                     "Extremely important" = 4)
cw$d13_religion_important_n <- as.numeric(as.character(cw$d13_religion_important_n))
cw$d13_religion_important <- factor(cw$d13_religion_important, levels=c('Not important at all', 'A little important', 'Moderately important',
                                                                        'Very important', 'Extremely important'))
cw$d14_religion_service_n <- recode_factor(cw$d14_religion_service,
                                             "Never" = 0,
                                             "A few times a year" = 1,
                                             "Once or twice a month" = 2,
                                             "Almost every week" = 3,
                                             "Every week" = 4)
cw$d14_religion_service_n <- as.numeric(as.character(cw$d14_religion_service_n))
cw$d14_religion_service <- factor(cw$d14_religion_service, levels=c('Never', 'A few times a year', 'Once or twice a month',
                                                                    'Almost every week', 'Every week'))

cw$d15_protest_n <- recode_factor(cw$d15_protest,
                                           "Prefer not to say" = 0,
                                           "No" = 1,
                                           "Yes" = 2)
cw$d15_protest_n <- as.numeric(as.character(cw$d15_protest_n))

write.csv(cw, "Data/ih_clean.csv")
