
# REQUIRES: Data/ih.csv
# REQUIRES: Data/nk.csv
# REQUIRES: Data/sudan.csv

# REQUIRES: Load Packages.R

# REQUIRES: Cleaning_IH.R
# REQUIRES: Cleaning_NK.R
# REQUIRES: Cleaning_Sudan.R

# REQUIRES: Plots/Three Conflicts/

################# Dependencies #################
# source("Cleaning_IH.R")
# source("Cleaning_NK.R")
# source("Cleaning_Sudan.R")

################# 5 - Sympathies #################
### Movement - Vertical dot and whisker plot by condition (3 columns)

# Israel-Hamas - 2787/3294 (85%) are left after removing Not Sure and With neither of them
cw$condition <- relevel(cw$condition, ref = "Control")
cw <- cw %>%
  mutate(condition = fct_relevel(condition, c("Control", "Condition_Israel_CW", "Condition_Israel_Personal", "Condition_Israel_Statistics",
                                              "Condition_Palestine_CW", "Condition_Palestine_Personal", "Condition_Palestine_Statistics")))
sympathies.ih <- cw[which(cw$po5_sympathies != "Not sure"),]
sympathies.ih <- sympathies.ih[which(sympathies.ih$po5_sympathies != "With neither of them"),]
sympathies.ih$po5_sympathies_n <- as.numeric(sympathies.ih$po5_sympathies_n)
sympathies.ih$po5_sympathies_n <- recode_factor(sympathies.ih$po5_sympathies,
                                             "Entirely with the Palestinian people" = 0,
                                             "Mostly with the Palestinian people but also somewhat with the Israeli people" = 1,
                                             "Equally with both of them" = 2,
                                             "Mostly with the Israeli people but also somewhat with the Palestinian people" = 3,
                                             "Entirely with the Israeli people" = 4)
sympathies.ih$po5_sympathies_n <- as.numeric(sympathies.ih$po5_sympathies_n)
ih.5 <- glm(po5_sympathies_n ~ condition, data = sympathies.ih)
summary(ih.5)
conditions.ih <- c("Israel CW", "Israel Personal", "Israel Statistics", "Palestine CW", "Palestine Personal", "Palestine Statistics")
ih.5.plot <- plot_model(ih.5, axis.lim = c(-2, 2)) +
  ggtitle("Israel-Hamas") +
  theme_light() +
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = rev(conditions.ih)) +
  xlab("Condition") + ylab(expression("Pro-Palestine"%<->%"Pro-Israel   ")) + 
  geom_hline(yintercept = 0, linetype = "dashed")

# Nagorno-Karabakh - 757/1170 (65%) are left after removing Not Sure and With neither of them
ncw$condition <- relevel(ncw$condition, ref = "Control")
ncw <- ncw %>%
  mutate(condition = fct_relevel(condition, c("Control", "Condition_Azerbaijan_CW", "Condition_Azerbaijan_Personal", "Condition_Azerbaijan_Statistics",
                                              "Condition_Armenia_CW", "Condition_Armenia_Personal", "Condition_Armenia_Statistics")))
sympathies.nk <- ncw[which(ncw$po5_sympathies != "Not sure"),]
sympathies.nk <- sympathies.nk[which(sympathies.nk$po5_sympathies != "With neither of them"),]
sympathies.nk$po5_sympathies_n <- as.numeric(sympathies.nk$po5_sympathies_n)
sympathies.nk$po5_sympathies_n <- recode_factor(sympathies.nk$po5_sympathies,
                                                "Entirely with the Armenian people" = 0,
                                                "Mostly with the Armenian people but also somewhat with the Azerbaijani people" = 1,
                                                "Equally with both of them" = 2,
                                                "Mostly with the Azerbaijani people but also somewhat with the Armenian people" = 3,
                                                "Entirely with the Azerbaijani people" = 4)
sympathies.nk$po5_sympathies_n <- as.numeric(sympathies.nk$po5_sympathies_n)
nk.5 <- glm(po5_sympathies_n ~ condition, data = sympathies.nk)
summary(nk.5)
conditions.nk <- c("Azerbaijan CW", "Azerbaijan Personal", "Azerbaijan Statistics", "Armenia CW", "Armenia Personal", "Armenia Statistics")
nk.5.plot <- plot_model(nk.5, axis.lim = c(-2, 2)) +
  ggtitle("Nagorno-Karabakh") +
  theme_light() +
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.y=element_blank()) +
  scale_x_discrete(labels = rev(conditions.nk)) +
  xlab("Condition") + ylab(expression("Pro-Armenia"%<->%"Pro-Azerbaijan   ")) + 
  geom_hline(yintercept = 0, linetype = "dashed")

# Sudan - 527/1209 (44%) are left after removing Not Sure and With neither of them
scw$condition <- relevel(scw$condition, ref = "Control")
scw <- scw %>%
  mutate(condition = fct_relevel(condition, c("Control", "Condition_SAF_CW", "Condition_SAF_Personal", "Condition_SAF_Statistics",
                                              "Condition_RSF_CW", "Condition_RSF_Personal", "Condition_RSF_Statistics")))
sympathies.s <- scw[which(scw$po5_sympathies != "Not sure"),]
sympathies.s <- sympathies.s[which(sympathies.s$po5_sympathies != "With neither of them"),]
sympathies.s$po5_sympathies_n <- as.numeric(sympathies.s$po5_sympathies_n)
sympathies.s$po5_sympathies_n <- recode_factor(sympathies.s$po5_sympathies,
                                             "Entirely with the Rapid Support Forces (rebel) supporters" = 0,
                                             "Mostly with the Rapid Support Forces (rebel) supporters but also somewhat with the Sudanese Armed Forces (government) supporters" = 1,
                                             "Equally with both of them" = 2,
                                             "Mostly with the Sudanese Armed Forces (government) supporters but also somewhat with the Rapid Support Forces (rebel) supporters" = 3,
                                             "Entirely with the Sudanese Armed Forces (government) supporters" = 4)
sympathies.s$po5_sympathies_n <- as.numeric(sympathies.s$po5_sympathies_n)
s.5 <- glm(po5_sympathies_n ~ condition, data = sympathies.s)
summary(s.5)
conditions.s <- c("SAF CW", "SAF Personal", "SAF Statistics", "RSF CW", "RSF Personal", "RSF Statistics")
s.5.plot <- plot_model(s.5, axis.lim = c(-2, 2)) +
  ggtitle("Sudan") +
  theme_light() +
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.y=element_blank()) +
  scale_x_discrete(labels = rev(conditions.s)) +
  xlab("Condition") + ylab(expression("Pro-RSF"%<->%"Pro-SAF   ")) + 
  geom_hline(yintercept = 0, linetype = "dashed")

# Arrange Plot
sympathies.plot <- ggarrange(ih.5.plot, nk.5.plot, s.5.plot, ncol=3)
ggsave("Plots/Three Conflicts/5 - Sympathies.png", sympathies.plot, width=13, height=5)



################# 8 - Leader Attention #################
### Movement - Horizontal bar plot, vertical conditions (3 columns, 5 plots - 1 for each issue)

## Creating Datasets
attn_order <- c("Too much attention", "Right amount of attention", "Too little attention")

# Israel-Hamas
cw <- cw %>%
  mutate(across(c(po8_leaderattention_ihw, po8_leaderattention_sudan, po8_leaderattention_ukraine, po8_leaderattention_myanmar), ~ fct_relevel(.x, attn_order)))

ihw.leader.attention.ih <- cw %>% group_by(condition) %>% count(po8_leaderattention_ihw) %>% filter(po8_leaderattention_ihw != "") 
temp <- ihw.leader.attention.ih %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
ihw.leader.attention.ih <- left_join(ihw.leader.attention.ih, temp, by = "condition") 
ihw.leader.attention.ih <- ihw.leader.attention.ih %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

sud.leader.attention.ih <- cw %>% group_by(condition) %>% count(po8_leaderattention_sudan) %>% filter(po8_leaderattention_sudan != "")
temp <- sud.leader.attention.ih %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
sud.leader.attention.ih <- left_join(sud.leader.attention.ih, temp, by = "condition") 
sud.leader.attention.ih <- sud.leader.attention.ih %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

ukr.leader.attention.ih <- cw %>% group_by(condition) %>% count(po8_leaderattention_ukraine) %>% filter(po8_leaderattention_ukraine != "")
temp <- ukr.leader.attention.ih %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
ukr.leader.attention.ih <- left_join(ukr.leader.attention.ih, temp, by = "condition") 
ukr.leader.attention.ih <- ukr.leader.attention.ih %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

mya.leader.attention.ih <- cw %>% group_by(condition) %>% count(po8_leaderattention_myanmar) %>% filter(po8_leaderattention_myanmar != "")
temp <- mya.leader.attention.ih %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
mya.leader.attention.ih <- left_join(mya.leader.attention.ih, temp, by = "condition") 
mya.leader.attention.ih <- mya.leader.attention.ih %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))


# Nagorno-Karabakh
ncw <- ncw %>%
  mutate(across(c(po8_leaderattention_ihw, po8_leaderattention_sudan, po8_leaderattention_ukraine, po8_leaderattention_myanmar, po8_leaderattention_nk), ~ fct_relevel(.x, attn_order)))

ihw.leader.attention.nk <- ncw %>% group_by(condition) %>% count(po8_leaderattention_ihw) %>% filter(po8_leaderattention_ihw != "") 
temp <- ihw.leader.attention.nk %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
ihw.leader.attention.nk <- left_join(ihw.leader.attention.nk, temp, by = "condition") 
ihw.leader.attention.nk <- ihw.leader.attention.nk %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

sud.leader.attention.nk <- ncw %>% group_by(condition) %>% count(po8_leaderattention_sudan) %>% filter(po8_leaderattention_sudan != "")
temp <- sud.leader.attention.nk %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
sud.leader.attention.nk <- left_join(sud.leader.attention.nk, temp, by = "condition") 
sud.leader.attention.nk <- sud.leader.attention.nk %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

nkc.leader.attention.nk <- ncw %>% group_by(condition) %>% count(po8_leaderattention_nk) %>% filter(po8_leaderattention_nk != "")
temp <- nkc.leader.attention.nk %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
nkc.leader.attention.nk <- left_join(nkc.leader.attention.nk, temp, by = "condition") 
nkc.leader.attention.nk <- nkc.leader.attention.nk %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

ukr.leader.attention.nk <- ncw %>% group_by(condition) %>% count(po8_leaderattention_ukraine) %>% filter(po8_leaderattention_ukraine != "")
temp <- ukr.leader.attention.nk %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
ukr.leader.attention.nk <- left_join(ukr.leader.attention.nk, temp, by = "condition") 
ukr.leader.attention.nk <- ukr.leader.attention.nk %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

mya.leader.attention.nk <- ncw %>% group_by(condition) %>% count(po8_leaderattention_myanmar) %>% filter(po8_leaderattention_myanmar != "")
temp <- mya.leader.attention.nk %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
mya.leader.attention.nk <- left_join(mya.leader.attention.nk, temp, by = "condition") 
mya.leader.attention.nk <- mya.leader.attention.nk %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))


# Sudan
scw <- scw %>%
  mutate(across(c(po8_leaderattention_ihw, po8_leaderattention_sudan, po8_leaderattention_ukraine, po8_leaderattention_myanmar, po8_leaderattention_nk), ~ fct_relevel(.x, attn_order)))

ihw.leader.attention.s <- scw %>% group_by(condition) %>% count(po8_leaderattention_ihw) %>% filter(po8_leaderattention_ihw != "") 
temp <- ihw.leader.attention.s %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
ihw.leader.attention.s <- left_join(ihw.leader.attention.s, temp, by = "condition") 
ihw.leader.attention.s <- ihw.leader.attention.s %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

sud.leader.attention.s <- scw %>% group_by(condition) %>% count(po8_leaderattention_sudan) %>% filter(po8_leaderattention_sudan != "")
temp <- sud.leader.attention.s %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
sud.leader.attention.s <- left_join(sud.leader.attention.s, temp, by = "condition") 
sud.leader.attention.s <- sud.leader.attention.s %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

nkc.leader.attention.s <- scw %>% group_by(condition) %>% count(po8_leaderattention_nk) %>% filter(po8_leaderattention_nk != "")
temp <- nkc.leader.attention.s %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
nkc.leader.attention.s <- left_join(nkc.leader.attention.s, temp, by = "condition") 
nkc.leader.attention.s <- nkc.leader.attention.s %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

ukr.leader.attention.s <- scw %>% group_by(condition) %>% count(po8_leaderattention_ukraine) %>% filter(po8_leaderattention_ukraine != "")
temp <- ukr.leader.attention.s %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
ukr.leader.attention.s <- left_join(ukr.leader.attention.s, temp, by = "condition") 
ukr.leader.attention.s <- ukr.leader.attention.s %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

mya.leader.attention.s <- scw %>% group_by(condition) %>% count(po8_leaderattention_myanmar) %>% filter(po8_leaderattention_myanmar != "")
temp <- mya.leader.attention.s %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
mya.leader.attention.s <- left_join(mya.leader.attention.s, temp, by = "condition") 
mya.leader.attention.s <- mya.leader.attention.s %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))


## Plots
conditions.ih2 <- c("Control", "Israel CW", "Israel Personal", "Israel Statistics", "Palestine CW", "Palestine Personal", "Palestine Statistics")
conditions.nk2 <- c("Control", "Azerbaijan CW", "Azerbaijan Personal", "Azerbaijan Statistics", "Armenia CW", "Armenia Personal", "Armenia Statistics")
conditions.s2 <- c("Control", "SAF CW", "SAF Personal", "SAF Statistics", "RSF CW", "RSF Personal", "RSF Statistics")

# Israel-Hamas War
ihw.attention.ih.plot <- ggplot(ihw.leader.attention.ih, aes(x = perc, y = condition, fill = po8_leaderattention_ihw, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Israel-Hamas") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  ylab(expression("Condition")) + 
  scale_y_discrete(limits = rev, labels = rev(conditions.ih2)) +
  scale_fill_manual(values = c("#56CCF2", "#6FCF97", "#E26A6A")) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

ihw.attention.nk.plot <- ggplot(ihw.leader.attention.nk, aes(x = perc, y = condition, fill = po8_leaderattention_ihw, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Nagorno-Karabakh") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.nk2)) +
  scale_fill_manual(values = c("#56CCF2", "#6FCF97", "#E26A6A")) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

ihw.attention.s.plot <- ggplot(ihw.leader.attention.s, aes(x = perc, y = condition, fill = po8_leaderattention_ihw, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Sudan") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.s2)) +
  scale_fill_manual(values = c("#56CCF2", "#6FCF97", "#E26A6A")) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

ihw.attention.plot <- ggarrange(ihw.attention.ih.plot, ihw.attention.nk.plot, ihw.attention.s.plot, ncol=3)
ggsave("Plots/Three Conflicts/8 - IHW Attention.png", ihw.attention.plot, width=15, height=5)


# Sudanese Civil War
sud.attention.ih.plot <- ggplot(sud.leader.attention.ih, aes(x = perc, y = condition, fill = po8_leaderattention_sudan, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Israel-Hamas") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  ylab(expression("Condition")) + 
  scale_y_discrete(limits = rev, labels = rev(conditions.ih2)) +
  scale_fill_manual(values = c("#56CCF2", "#6FCF97", "#E26A6A")) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

sud.attention.nk.plot <- ggplot(sud.leader.attention.nk, aes(x = perc, y = condition, fill = po8_leaderattention_sudan, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Nagorno-Karabakh") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.nk2)) +
  scale_fill_manual(values = c("#56CCF2", "#6FCF97", "#E26A6A")) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

sud.attention.s.plot <- ggplot(sud.leader.attention.s, aes(x = perc, y = condition, fill = po8_leaderattention_sudan, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Sudan") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.s2)) +
  scale_fill_manual(values = c("#56CCF2", "#6FCF97", "#E26A6A")) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

sud.attention.plot <- ggarrange(sud.attention.ih.plot, sud.attention.nk.plot, sud.attention.s.plot, ncol=3)
ggsave("Plots/Three Conflicts/8 - Sudan Attention.png", sud.attention.plot, width=15, height=5)


# Nagorno-Karabakh Conflict (No Israel-Hamas)
nkc.attention.nk.plot <- ggplot(nkc.leader.attention.nk, aes(x = perc, y = condition, fill = po8_leaderattention_nk, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Nagorno-Karabakh") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.nk2)) +
  scale_fill_manual(values = c("#56CCF2", "#6FCF97", "#E26A6A")) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

nkc.attention.s.plot <- ggplot(nkc.leader.attention.s, aes(x = perc, y = condition, fill = po8_leaderattention_nk, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Sudan") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.s2)) +
  scale_fill_manual(values = c("#56CCF2", "#6FCF97", "#E26A6A")) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

nkc.attention.plot <- ggarrange(nkc.attention.nk.plot, nkc.attention.s.plot, ncol=2)
ggsave("Plots/Three Conflicts/8 - NK Attention.png", nkc.attention.plot, width=12, height=5)

# Russia-Ukraine War
ukr.attention.ih.plot <- ggplot(ukr.leader.attention.ih, aes(x = perc, y = condition, fill = po8_leaderattention_ukraine, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Israel-Hamas") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  ylab(expression("Condition")) + 
  scale_y_discrete(limits = rev, labels = rev(conditions.ih2)) +
  scale_fill_manual(values = c("#56CCF2", "#6FCF97", "#E26A6A")) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

ukr.attention.nk.plot <- ggplot(ukr.leader.attention.nk, aes(x = perc, y = condition, fill = po8_leaderattention_ukraine, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Nagorno-Karabakh") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.nk2)) +
  scale_fill_manual(values = c("#56CCF2", "#6FCF97", "#E26A6A")) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

ukr.attention.s.plot <- ggplot(ukr.leader.attention.s, aes(x = perc, y = condition, fill = po8_leaderattention_ukraine, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Sudan") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.s2)) +
  scale_fill_manual(values = c("#56CCF2", "#6FCF97", "#E26A6A")) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

ukr.attention.plot <- ggarrange(ukr.attention.ih.plot, ukr.attention.nk.plot, ukr.attention.s.plot, ncol=3)
ggsave("Plots/Three Conflicts/8 - Ukraine Attention.png", ukr.attention.plot, width=15, height=5)

# Myanmar Civil War
mya.attention.ih.plot <- ggplot(mya.leader.attention.ih, aes(x = perc, y = condition, fill = po8_leaderattention_myanmar, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Israel-Hamas") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  ylab(expression("Condition")) + 
  scale_y_discrete(limits = rev, labels = rev(conditions.ih2)) +
  scale_fill_manual(values = c("#56CCF2", "#6FCF97", "#E26A6A")) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

mya.attention.nk.plot <- ggplot(mya.leader.attention.nk, aes(x = perc, y = condition, fill = po8_leaderattention_myanmar, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Nagorno-Karabakh") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.nk2)) +
  scale_fill_manual(values = c("#56CCF2", "#6FCF97", "#E26A6A")) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

mya.attention.s.plot <- ggplot(mya.leader.attention.s, aes(x = perc, y = condition, fill = po8_leaderattention_myanmar, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Sudan") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.s2)) +
  scale_fill_manual(values = c("#56CCF2", "#6FCF97", "#E26A6A")) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

mya.attention.plot <- ggarrange(mya.attention.ih.plot, mya.attention.nk.plot, mya.attention.s.plot, ncol=3)
ggsave("Plots/Three Conflicts/8 - Myanmar Attention.png", mya.attention.plot, width=15, height=5)



### Ordered Logits
# Israel - Hamas
attention.ih <- cw %>% filter(po8_leaderattention_ihw != "") 
attention.ih$po8_leaderattention_ihw <- droplevels(attention.ih$po8_leaderattention_ihw)
attention.ih$po8_leaderattention_ihw <- relevel(attention.ih$po8_leaderattention_ihw, ref = "Right amount of attention")
attention.ih <- attention.ih %>%
  mutate(po8_leaderattention_ihw = fct_relevel(po8_leaderattention_ihw, c("Too little attention", "Right amount of attention", "Too much attention")))
attention.ih$po8_leaderattention_ihw <- factor(attention.ih$po8_leaderattention_ihw, ordered = TRUE)
attention.ih$condition <- droplevels(attention.ih$condition)

# attn2.ih <- polr(po8_leaderattention_ihw ~ condition, data = attention.ih, Hess = TRUE)
# attn2.m.ih <- ggpredict(attn2.ih, terms = "condition")

attn2.ih <- glm(po8_leaderattention_ihw_n ~ condition, data = attention.ih)
summary(attn2.ih)
ih.8.plot2 <- plot_model(attn2.ih, axis.lim = c(-0.5, 0.5)) +
  ggtitle("Israel-Hamas") +
  theme_light() +
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = rev(conditions.ih)) +
  xlab("Condition") + ylab(expression("Too Little Attention"%<->%"Too Much Attention  ")) + 
  ylim(-0.5, 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed")


# Nagorno-Karabakh
attention.nk <- ncw %>% filter(po8_leaderattention_nk != "") 
attention.nk$po8_leaderattention_nk <- droplevels(attention.nk$po8_leaderattention_nk)
attention.nk$po8_leaderattention_nk <- relevel(attention.nk$po8_leaderattention_nk, ref = "Right amount of attention")
attention.nk <- attention.nk %>%
  mutate(po8_leaderattention_nk = fct_relevel(po8_leaderattention_nk, c("Too little attention", "Right amount of attention", "Too much attention")))
attention.nk$po8_leaderattention_nk <- factor(attention.nk$po8_leaderattention_nk, ordered = TRUE)
attention.nk$condition <- droplevels(attention.nk$condition)

attn2.nk <- glm(po8_leaderattention_nk_n ~ condition, data = attention.nk)
summary(attn2.nk)
nk.8.plot2 <- plot_model(attn2.nk, axis.lim = c(-0.5, 0.5)) +
  ggtitle("Nagorno-Karabakh") +
  theme_light() +
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.y=element_blank()) +
  scale_x_discrete(labels = rev(conditions.nk)) +
  xlab("Condition") + ylab(expression("Too Little Attention"%<->%"Too Much Attention  ")) + 
  ylim(-0.5, 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed")

# Sudan
attention.s <- scw %>% filter(po8_leaderattention_sudan != "") 
attention.s$po8_leaderattention_s <- droplevels(attention.s$po8_leaderattention_sudan)
attention.s$po8_leaderattention_s <- relevel(attention.s$po8_leaderattention_sudan, ref = "Right amount of attention")
attention.s <- attention.s %>%
  mutate(po8_leaderattention_sudan = fct_relevel(po8_leaderattention_sudan, c("Too little attention", "Right amount of attention", "Too much attention")))
attention.s$po8_leaderattention_sudan <- factor(attention.s$po8_leaderattention_sudan, ordered = TRUE)
attention.s$condition <- droplevels(attention.s$condition)

attn2.s <- glm(po8_leaderattention_sudan_n ~ condition, data = attention.s)
summary(attn2.s)
s.8.plot2 <- plot_model(attn2.s, axis.lim = c(-0.5, 0.5)) +
  ggtitle("Sudan") +
  theme_light() +
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.y=element_blank()) +
  scale_x_discrete(labels = rev(conditions.s)) +
  xlab("Condition") + ylab(expression("Too Little Attention"%<->%"Too Much Attention  ")) + 
  ylim(-0.5, 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed")

# Arrange Plot
attention.plot2 <- ggarrange(ih.8.plot2, nk.8.plot2, s.8.plot2, ncol=3)
ggsave("Plots/Three Conflicts/8 - Attention per Conflict.png", attention.plot2, width=13, height=5)










################# 10 - Negotiate Peace #################
### Movement - Vertical dot and whisker plot by condition

# Israel-Hamas
peace.ih <- cw[which(cw$po10_important_ceasefire != ""),]
ih.10 <- glm(po10_important_ceasefire_n ~ condition, data = peace.ih)
summary(ih.10)
ih.10.plot <- plot_model(ih.10, axis.lim = c(-1, 1)) +
  ggtitle("Israel-Hamas") +
  theme_light() +
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = rev(conditions.ih)) +
  xlab("Condition") + ylab(expression("      Not Important"%<->%"Extremely Important")) + 
  geom_hline(yintercept = 0, linetype = "dashed")

# Nagorno-Karabakh
peace.nk <- ncw[which(ncw$po10_important_ceasefire != ""),]
nk.10 <- glm(po10_important_ceasefire_n ~ condition, data = peace.nk)
summary(nk.10)
nk.10.plot <- plot_model(nk.10, axis.lim = c(-1, 1), colors = "#377EB8") +
  ggtitle("Nagorno-Karabakh") +
  theme_light() +
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.y=element_blank()) +
  scale_x_discrete(labels = rev(conditions.nk)) +
  xlab("Condition") + ylab(expression("      Not Important"%<->%"Extremely Important")) + 
  geom_hline(yintercept = 0, linetype = "dashed")

# Sudan
peace.s <- scw[which(scw$po10_important_ceasefire != ""),]
s.10 <- glm(po10_important_ceasefire_n ~ condition, data = peace.s)
summary(s.10)
s.10.plot <- plot_model(s.10, axis.lim = c(-1, 1), colors = "#377EB8") +
  ggtitle("Sudan") +
  theme_light() +
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.y=element_blank()) +
  scale_x_discrete(labels = rev(conditions.s)) +
  xlab("Condition") + ylab(expression("      Not Important"%<->%"Extremely Important")) + 
  geom_hline(yintercept = 0, linetype = "dashed")

# Arrange Plot
peace.plot <- ggarrange(ih.10.plot, nk.10.plot, s.10.plot, ncol=3)
ggsave("Plots/Three Conflicts/10 - Peace.png", peace.plot, width=13, height=5)



### Statics

peace_order <- c("Extremely important", "Very important", "Moderately important",
                 "Slightly important", "Not at all important")

# Israel-Hamas
cw <- cw %>%
  mutate(across(c(po10_important_ceasefire), ~ fct_relevel(.x, peace_order)))
peace.ih2 <- cw %>% group_by(condition) %>% count(po10_important_ceasefire) %>% filter(po10_important_ceasefire != "") 
temp <- peace.ih2 %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
peace.ih2 <- left_join(peace.ih2, temp, by = "condition") 
peace.ih2 <- peace.ih2 %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

# Nagorno-Karabakh
ncw <- ncw %>%
  mutate(across(c(po10_important_ceasefire), ~ fct_relevel(.x, peace_order)))
peace.nk2 <- ncw %>% group_by(condition) %>% count(po10_important_ceasefire) %>% filter(po10_important_ceasefire != "") 
temp <- peace.nk2 %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
peace.nk2 <- left_join(peace.nk2, temp, by = "condition") 
peace.nk2 <- peace.nk2 %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

# Sudan
cw <- cw %>%
  mutate(across(c(po10_important_ceasefire), ~ fct_relevel(.x, peace_order)))
peace.s2 <- cw %>% group_by(condition) %>% count(po10_important_ceasefire) %>% filter(po10_important_ceasefire != "") 
temp <- peace.s2 %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
peace.s2 <- left_join(peace.s2, temp, by = "condition") 
peace.s2 <- peace.s2 %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))


peace.text.col <- c("white", "white", "black", "black", "black", "white", "white", "black", "black", "black", "white", "white", "black", "black", "black",
                    "white", "white", "black", "black", "black", "white", "white", "black", "black", "black", "white", "white", "black", "black", "black", 
                    "white", "white", "black", "black", "black")


peace.ih2.plot <- ggplot(peace.ih2, aes(x = perc, y = condition, fill = po10_important_ceasefire, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Israel-Hamas") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  ylab(expression("Condition")) + 
  scale_y_discrete(limits = rev, labels = rev(conditions.ih2)) +
  scale_fill_manual(values = c("#5A86AD", "#81A5C3", "#A9C4DA", "#D0E3F0", "#E8F1F7")) +
  geom_text(position = position_stack(vjust = 0.5), color = peace.text.col, size = 3.5, family="Times New Roman")

peace.nk2.plot <- ggplot(peace.nk2, aes(x = perc, y = condition, fill = po10_important_ceasefire, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Nagorno-Karabakh") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.nk2)) +
  scale_fill_manual(values = c("#5AAD8F", "#81C3AB", "#A9DAC8", "#D0F0E4", "#E8F7F2")) +
  geom_text(position = position_stack(vjust = 0.5), color = peace.text.col, size = 3.5, family="Times New Roman")

peace.s2.plot <- ggplot(peace.s2, aes(x = perc, y = condition, fill = po10_important_ceasefire, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Sudan") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.s2)) +
  scale_fill_manual(values = c("#B05252", "#C56161", "#E4A1A0", "#FEE4E1", "#FEF1F0")) +
  geom_text(position = position_stack(vjust = 0.5), color = peace.text.col, size = 3.5, family="Times New Roman")

peace.plot2 <- ggarrange(peace.ih2.plot, peace.nk2.plot, peace.s2.plot, ncol=3)
ggsave("Plots/Three Conflicts/10 - Peace per Conflict.png", peace.plot2, width=15, height=5)



################# 4 - Feelings #################
### Horizontal bar plot, vertical conditions (3 columns, 4 plots - Afraid, Exhausted, Angry, Sad)

feeling_order <- c("Very strongly", "Strongly", "Somewhat", "Not at all")

## Afraid
# Israel-Hamas
cw <- cw %>%
  mutate(across(c(po4_feeling_afraid, po4_feeling_exhausted, po4_feeling_angry, po4_feeling_sad), ~ fct_relevel(.x, feeling_order)))
afraid.ih <- cw %>% group_by(condition) %>% count(po4_feeling_afraid) %>% filter(po4_feeling_afraid != "") 
temp <- afraid.ih %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
afraid.ih <- left_join(afraid.ih, temp, by = "condition") 
afraid.ih <- afraid.ih %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

# Nagorno-Karabakh
ncw <- ncw %>%
  mutate(across(c(po4_feeling_afraid, po4_feeling_exhausted, po4_feeling_angry, po4_feeling_sad), ~ fct_relevel(.x, feeling_order)))
afraid.nk <- ncw %>% group_by(condition) %>% count(po4_feeling_afraid) %>% filter(po4_feeling_afraid != "") 
temp <- afraid.nk %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
afraid.nk <- left_join(afraid.nk, temp, by = "condition") 
afraid.nk <- afraid.nk %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

# Sudan
scw <- scw %>%
  mutate(across(c(po4_feeling_afraid, po4_feeling_exhausted, po4_feeling_angry, po4_feeling_sad), ~ fct_relevel(.x, feeling_order)))
afraid.s <- scw %>% group_by(condition) %>% count(po4_feeling_afraid) %>% filter(po4_feeling_afraid != "") 
temp <- afraid.s %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
afraid.s <- left_join(afraid.s, temp, by = "condition") 
afraid.s <- afraid.s %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

feeling.text.col <- c("white", "white", "black", "black", "white", "white", "black", "black", "white", "white", "black", "black",
                     "white", "white", "black", "black", "white", "white", "black", "black", "white", "white", "black", "black")

afraid.ih.plot <- ggplot(afraid.ih, aes(x = perc, y = condition, fill = po4_feeling_afraid, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Israel-Hamas") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  ylab(expression("Condition")) + 
  scale_y_discrete(limits = rev, labels = rev(conditions.ih)) +
  scale_fill_manual(values = c("#73586C", "#94788D", "#B59DAE", "#D8C6E0")) +
  geom_text(position = position_stack(vjust = 0.5), color = feeling.text.col, size = 3.5, family="Times New Roman")

afraid.nk.plot <- ggplot(afraid.nk, aes(x = perc, y = condition, fill = po4_feeling_afraid, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Nagorno-Karabakh") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.nk)) +
  scale_fill_manual(values = c("#73586C", "#94788D", "#B59DAE", "#D8C6E0")) +
  geom_text(position = position_stack(vjust = 0.5), color = feeling.text.col, size = 3.5, family="Times New Roman")

afraid.s.plot <- ggplot(afraid.s, aes(x = perc, y = condition, fill = po4_feeling_afraid, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Sudan") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.s)) +
  scale_fill_manual(values = c("#73586C", "#94788D", "#B59DAE", "#D8C6E0")) +
  geom_text(position = position_stack(vjust = 0.5), color = feeling.text.col, size = 3.5, family="Times New Roman")

afraid.plot <- ggarrange(afraid.ih.plot, afraid.nk.plot, afraid.s.plot, ncol=3)
ggsave("Plots/Three Conflicts/4 - Feeling - Afraid.png", afraid.plot, width=15, height=5)


## Exhausted
# Israel-Hamas
cw <- cw %>%
  mutate(across(c(po4_feeling_exhausted, po4_feeling_exhausted, po4_feeling_angry, po4_feeling_sad), ~ fct_relevel(.x, feeling_order)))
exhausted.ih <- cw %>% group_by(condition) %>% count(po4_feeling_exhausted) %>% filter(po4_feeling_exhausted != "") 
temp <- exhausted.ih %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
exhausted.ih <- left_join(exhausted.ih, temp, by = "condition") 
exhausted.ih <- exhausted.ih %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

# Nagorno-Karabakh
ncw <- ncw %>%
  mutate(across(c(po4_feeling_exhausted, po4_feeling_exhausted, po4_feeling_angry, po4_feeling_sad), ~ fct_relevel(.x, feeling_order)))
exhausted.nk <- ncw %>% group_by(condition) %>% count(po4_feeling_exhausted) %>% filter(po4_feeling_exhausted != "") 
temp <- exhausted.nk %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
exhausted.nk <- left_join(exhausted.nk, temp, by = "condition") 
exhausted.nk <- exhausted.nk %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

# Sudan
scw <- scw %>%
  mutate(across(c(po4_feeling_exhausted, po4_feeling_exhausted, po4_feeling_angry, po4_feeling_sad), ~ fct_relevel(.x, feeling_order)))
exhausted.s <- scw %>% group_by(condition) %>% count(po4_feeling_exhausted) %>% filter(po4_feeling_exhausted != "") 
temp <- exhausted.s %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
exhausted.s <- left_join(exhausted.s, temp, by = "condition") 
exhausted.s <- exhausted.s %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

feeling.text.col2 <- c("white", "black", "black", "black", "white", "black", "black", "black", "white", "black", "black", "black",
                       "white", "black", "black", "black", "white", "black", "black", "black", "white", "black", "black", "black")

exhausted.ih.plot <- ggplot(exhausted.ih, aes(x = perc, y = condition, fill = po4_feeling_exhausted, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Israel-Hamas") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  ylab(expression("Condition")) + 
  scale_y_discrete(limits = rev, labels = rev(conditions.ih)) +
  scale_fill_manual(values = c("#A6927D", "#C6B39E", "#E6D7C3", "#F4EBE1")) +
  geom_text(position = position_stack(vjust = 0.5), color = feeling.text.col2, size = 3.5, family="Times New Roman")

exhausted.nk.plot <- ggplot(exhausted.nk, aes(x = perc, y = condition, fill = po4_feeling_exhausted, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Nagorno-Karabakh") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.nk)) +
  scale_fill_manual(values = c("#A6927D", "#C6B39E", "#E6D7C3", "#F4EBE1")) +
  geom_text(position = position_stack(vjust = 0.5), color = feeling.text.col2, size = 3.5, family="Times New Roman")

exhausted.s.plot <- ggplot(exhausted.s, aes(x = perc, y = condition, fill = po4_feeling_exhausted, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Sudan") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.s)) +
  scale_fill_manual(values = c("#A6927D", "#C6B39E", "#E6D7C3", "#F4EBE1")) +
  geom_text(position = position_stack(vjust = 0.5), color = feeling.text.col2, size = 3.5, family="Times New Roman")

exhausted.plot <- ggarrange(exhausted.ih.plot, exhausted.nk.plot, exhausted.s.plot, ncol=3)
ggsave("Plots/Three Conflicts/4 - Feeling - Exhausted.png", exhausted.plot, width=15, height=5)


## Angry
# Israel-Hamas
cw <- cw %>%
  mutate(across(c(po4_feeling_angry, po4_feeling_angry, po4_feeling_angry, po4_feeling_sad), ~ fct_relevel(.x, feeling_order)))
angry.ih <- cw %>% group_by(condition) %>% count(po4_feeling_angry) %>% filter(po4_feeling_angry != "") 
temp <- angry.ih %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
angry.ih <- left_join(angry.ih, temp, by = "condition") 
angry.ih <- angry.ih %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

# Nagorno-Karabakh
ncw <- ncw %>%
  mutate(across(c(po4_feeling_angry, po4_feeling_angry, po4_feeling_angry, po4_feeling_sad), ~ fct_relevel(.x, feeling_order)))
angry.nk <- ncw %>% group_by(condition) %>% count(po4_feeling_angry) %>% filter(po4_feeling_angry != "") 
temp <- angry.nk %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
angry.nk <- left_join(angry.nk, temp, by = "condition") 
angry.nk <- angry.nk %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

# Sudan
scw <- scw %>%
  mutate(across(c(po4_feeling_angry, po4_feeling_angry, po4_feeling_angry, po4_feeling_sad), ~ fct_relevel(.x, feeling_order)))
angry.s <- scw %>% group_by(condition) %>% count(po4_feeling_angry) %>% filter(po4_feeling_angry != "") 
temp <- angry.s %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
angry.s <- left_join(angry.s, temp, by = "condition") 
angry.s <- angry.s %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

angry.ih.plot <- ggplot(angry.ih, aes(x = perc, y = condition, fill = po4_feeling_angry, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Israel-Hamas") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  ylab(expression("Condition")) + 
  scale_y_discrete(limits = rev, labels = rev(conditions.ih)) +
  scale_fill_manual(values = c("#B05252", "#C56161", "#E4A1A0", "#FEE4E1")) +
  geom_text(position = position_stack(vjust = 0.5), color = feeling.text.col, size = 3.5, family="Times New Roman")

angry.nk.plot <- ggplot(angry.nk, aes(x = perc, y = condition, fill = po4_feeling_angry, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Nagorno-Karabakh") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.nk)) +
  scale_fill_manual(values = c("#B05252", "#C56161", "#E4A1A0", "#FEE4E1")) +
  geom_text(position = position_stack(vjust = 0.5), color = feeling.text.col, size = 3.5, family="Times New Roman")

angry.s.plot <- ggplot(angry.s, aes(x = perc, y = condition, fill = po4_feeling_angry, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Sudan") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.s)) +
  scale_fill_manual(values = c("#B05252", "#C56161", "#E4A1A0", "#FEE4E1")) +
  geom_text(position = position_stack(vjust = 0.5), color = feeling.text.col, size = 3.5, family="Times New Roman")

angry.plot <- ggarrange(angry.ih.plot, angry.nk.plot, angry.s.plot, ncol=3)
ggsave("Plots/Three Conflicts/4 - Feeling - Angry.png", angry.plot, width=15, height=5)


## Sad
# Israel-Hamas
cw <- cw %>%
  mutate(across(c(po4_feeling_sad, po4_feeling_sad, po4_feeling_sad, po4_feeling_sad), ~ fct_relevel(.x, feeling_order)))
sad.ih <- cw %>% group_by(condition) %>% count(po4_feeling_sad) %>% filter(po4_feeling_sad != "") 
temp <- sad.ih %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
sad.ih <- left_join(sad.ih, temp, by = "condition") 
sad.ih <- sad.ih %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

# Nagorno-Karabakh
ncw <- ncw %>%
  mutate(across(c(po4_feeling_sad, po4_feeling_sad, po4_feeling_sad, po4_feeling_sad), ~ fct_relevel(.x, feeling_order)))
sad.nk <- ncw %>% group_by(condition) %>% count(po4_feeling_sad) %>% filter(po4_feeling_sad != "") 
temp <- sad.nk %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
sad.nk <- left_join(sad.nk, temp, by = "condition") 
sad.nk <- sad.nk %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

# Sudan
scw <- scw %>%
  mutate(across(c(po4_feeling_sad, po4_feeling_sad, po4_feeling_sad, po4_feeling_sad), ~ fct_relevel(.x, feeling_order)))
sad.s <- scw %>% group_by(condition) %>% count(po4_feeling_sad) %>% filter(po4_feeling_sad != "") 
temp <- sad.s %>% group_by(condition) %>% summarise(sum = sum(n, na.rm = TRUE))
sad.s <- left_join(sad.s, temp, by = "condition") 
sad.s <- sad.s %>% mutate(perc = (n/sum)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

sad.ih.plot <- ggplot(sad.ih, aes(x = perc, y = condition, fill = po4_feeling_sad, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Israel-Hamas") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  ylab(expression("Condition")) + 
  scale_y_discrete(limits = rev, labels = rev(conditions.ih)) +
  scale_fill_manual(values = c("#5A86AD", "#81A5C3", "#A9C4DA", "#D0E3F0")) +
  geom_text(position = position_stack(vjust = 0.5), color = feeling.text.col, size = 3.5, family="Times New Roman")

sad.nk.plot <- ggplot(sad.nk, aes(x = perc, y = condition, fill = po4_feeling_sad, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Nagorno-Karabakh") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.nk)) +
  scale_fill_manual(values = c("#5A86AD", "#81A5C3", "#A9C4DA", "#D0E3F0")) +
  geom_text(position = position_stack(vjust = 0.5), color = feeling.text.col, size = 3.5, family="Times New Roman")

sad.s.plot <- ggplot(sad.s, aes(x = perc, y = condition, fill = po4_feeling_sad, label = perc_label)) +
  geom_col(position = "stack") + 
  ggtitle("Sudan") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(limits = rev, labels = rev(conditions.s)) +
  scale_fill_manual(values = c("#5A86AD", "#81A5C3", "#A9C4DA", "#D0E3F0")) +
  geom_text(position = position_stack(vjust = 0.5), color = feeling.text.col, size = 3.5, family="Times New Roman")

sad.plot <- ggarrange(sad.ih.plot, sad.nk.plot, sad.s.plot, ncol=3)
ggsave("Plots/Three Conflicts/4 - Feeling - Sad.png", sad.plot, width=15, height=5)



################# Attention Checks #################
### Horizontal bar plots, vertical conditions (3 columns)

# Israel-Hamas
temp <- cw %>% group_by(condition) %>% count(condition) %>% filter(condition != "Control")
tmp1 <- cw_full  %>% group_by(AIC) %>% count(AIC) %>% filter(AIC == "Ariel Bibas") %>% rename(condition = AIC) %>% mutate(condition = "Condition_Israel_CW")
tmp2 <- cw_full  %>% group_by(AIP) %>% count(AIP) %>% filter(AIP == "Ariel Bibas") %>% rename(condition = AIP) %>% mutate(condition = "Condition_Israel_Personal")
tmp3 <- cw_full  %>% group_by(AIS) %>% count(AIS) %>% filter(AIS == "1,200") %>% rename(condition = AIS) %>% mutate(condition = "Condition_Israel_Statistics")
tmp4 <- cw_full  %>% group_by(APC) %>% count(APC) %>% filter(APC == "Hind Rajab") %>% rename(condition = APC) %>% mutate(condition = "Condition_Palestine_CW")
tmp5 <- cw_full  %>% group_by(APP) %>% count(APP) %>% filter(APP == "Hind Rajab") %>% rename(condition = APP) %>% mutate(condition = "Condition_Palestine_Personal")
tmp6 <- cw_full  %>% group_by(APS) %>% count(APS) %>% filter(APS == "40,000") %>% rename(condition = APS) %>% mutate(condition = "Condition_Palestine_Statistics")
attn.ih <- bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)
attn.ih <- left_join(attn.ih, temp, by = "condition") 
attn.ih <- attn.ih %>% mutate(perc = (n.x/n.y)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

# Nagorno-Karabakh
temp <- ncw %>% group_by(condition) %>% count(condition) %>% filter(condition != "Control")
tmp1 <- ncw_full  %>% group_by(AZC) %>% count(AZC) %>% filter(AZC == "Rashad Abbasov") %>% rename(condition = AZC) %>% mutate(condition = "Condition_Azerbaijan_CW")
tmp2 <- ncw_full  %>% group_by(AZP) %>% count(AZP) %>% filter(AZP == "Rashad Abbasov") %>% rename(condition = AZP) %>% mutate(condition = "Condition_Azerbaijan_Personal")
tmp3 <- ncw_full  %>% group_by(AZS) %>% count(AZS) %>% filter(AZS == "155") %>% rename(condition = AZS) %>% mutate(condition = "Condition_Azerbaijan_Statistics")
tmp4 <- ncw_full  %>% group_by(AMC) %>% count(AMC) %>% filter(AMC == "Gita Narimanyan") %>% rename(condition = AMC) %>% mutate(condition = "Condition_Armenia_CW")
tmp5 <- ncw_full  %>% group_by(AMP) %>% count(AMP) %>% filter(AMP == "Gita Narimanyan") %>% rename(condition = AMP) %>% mutate(condition = "Condition_Armenia_Personal")
tmp6 <- ncw_full  %>% group_by(AMS) %>% count(AMS) %>% filter(AMS == "190") %>% rename(condition = AMS) %>% mutate(condition = "Condition_Armenia_Statistics")
attn.nk <- bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)
attn.nk <- left_join(attn.nk, temp, by = "condition") 
attn.nk <- attn.nk %>% mutate(perc = (n.x/n.y)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

# Sudan
temp <- scw %>% group_by(condition) %>% count(condition) %>% filter(condition != "Control")
tmp1 <- scw_full  %>% group_by(ASC) %>% count(ASC) %>% filter(ASC == "Dewata Babiker") %>% rename(condition = ASC) %>% mutate(condition = "Condition_SAF_CW")
tmp2 <- scw_full  %>% group_by(ASP) %>% count(ASP) %>% filter(ASP == "Dewata Babiker") %>% rename(condition = ASP) %>% mutate(condition = "Condition_SAF_Personal")
tmp3 <- scw_full  %>% group_by(ASS) %>% count(ASS) %>% filter(ASS == "6,000") %>% rename(condition = ASS) %>% mutate(condition = "Condition_SAF_Statistics")
tmp4 <- scw_full  %>% group_by(ARC) %>% count(ARC) %>% filter(ARC == "Israr Mahmoud") %>% rename(condition = ARC) %>% mutate(condition = "Condition_RSF_CW")
tmp5 <- scw_full  %>% group_by(ARP) %>% count(ARP) %>% filter(ARP == "Israr Mahmoud") %>% rename(condition = ARP) %>% mutate(condition = "Condition_RSF_Personal")
tmp6 <- scw_full  %>% group_by(ARS) %>% count(ARS) %>% filter(ARS == "26,000") %>% rename(condition = ARS) %>% mutate(condition = "Condition_RSF_Statistics")
attn.s <- bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)
attn.s <- left_join(attn.s, temp, by = "condition") 
attn.s <- attn.s %>% mutate(perc = (n.x/n.y)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

attn.ih.plot <- ggplot(attn.ih, aes(x = perc, y = condition, label = perc_label)) +
  geom_col(position = "stack", fill = "#56CCF2") + 
  ggtitle("Israel-Hamas") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        legend.title = element_blank()) +
  ylab(expression("Condition")) + 
  scale_y_discrete(limits = rev, labels = rev(conditions.ih)) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

attn.nk.plot <- ggplot(attn.nk, aes(x = perc, y = condition, label = perc_label)) +
  geom_col(position = "stack", fill = "#6FCF97") + 
  ggtitle("Nagorno-Karabakh") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.position = "none") +
  scale_y_discrete(limits = rev, labels = rev(conditions.nk)) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

attn.s.plot <- ggplot(attn.s, aes(x = perc, y = condition, label = perc_label)) +
  geom_col(position = "stack", fill = "#E26A6A") + 
  ggtitle("Sudan") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.position = "none") +
  scale_y_discrete(limits = rev, labels = rev(conditions.s)) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

attn.plot <- ggarrange(attn.ih.plot, attn.nk.plot, attn.s.plot, ncol=3)
ggsave("Plots/Three Conflicts/Attention Check.png", attn.plot, width=15, height=5)



################# Donations #################
### Horizontal bar plots, vertical conditions (3 columns)

# Israel-Hamas
temp <- cw %>% group_by(condition) %>% count(condition) %>% filter(condition != "Control")
tmp1 <- cw_full %>% group_by(condition) %>% count(DIC) %>% filter(DIC == "Yes")
tmp2 <- cw_full %>% group_by(condition) %>% count(DIP) %>% filter(DIP == "Yes")
tmp3 <- cw_full %>% group_by(condition) %>% count(DIS) %>% filter(DIS == "Yes")
tmp4 <- cw_full %>% group_by(condition) %>% count(DPC) %>% filter(DPC == "Yes")
tmp5 <- cw_full %>% group_by(condition) %>% count(DPP) %>% filter(DPP == "Yes")
tmp6 <- cw_full %>% group_by(condition) %>% count(DPS) %>% filter(DPS == "Yes")
donate.ih <- bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)
donate.ih <- left_join(donate.ih, temp, by = "condition") 
donate.ih <- donate.ih %>% mutate(perc = (n.x/n.y)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

# Nagorno-Karabakh
temp <- ncw %>% group_by(condition) %>% count(condition) %>% filter(condition != "Control")
tmp1 <- ncw_full %>% group_by(condition) %>% count(DZC) %>% filter(DZC == "Yes")
tmp2 <- ncw_full %>% group_by(condition) %>% count(DZP) %>% filter(DZP == "Yes")
tmp3 <- ncw_full %>% group_by(condition) %>% count(DZS) %>% filter(DZS == "Yes")
tmp4 <- ncw_full %>% group_by(condition) %>% count(DMC) %>% filter(DMC == "Yes")
tmp5 <- ncw_full %>% group_by(condition) %>% count(DMP) %>% filter(DMP == "Yes")
tmp6 <- ncw_full %>% group_by(condition) %>% count(DMS) %>% filter(DMS == "Yes")
donate.nk <- bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)
donate.nk <- left_join(donate.nk, temp, by = "condition") 
donate.nk <- donate.nk %>% mutate(perc = (n.x/n.y)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

# Sudan
temp <- scw %>% group_by(condition) %>% count(condition) %>% filter(condition != "Control")
tmp1 <- scw_full %>% group_by(condition) %>% count(DSC) %>% filter(DSC == "Yes")
tmp2 <- scw_full %>% group_by(condition) %>% count(DSP) %>% filter(DSP == "Yes")
tmp3 <- scw_full %>% group_by(condition) %>% count(DSS) %>% filter(DSS == "Yes")
tmp4 <- scw_full %>% group_by(condition) %>% count(DRC) %>% filter(DRC == "Yes")
tmp5 <- scw_full %>% group_by(condition) %>% count(DRP) %>% filter(DRP == "Yes")
tmp6 <- scw_full %>% group_by(condition) %>% count(DRS) %>% filter(DRS == "Yes")
donate.s <- bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)
donate.s <- left_join(donate.s, temp, by = "condition") 
donate.s <- donate.s %>% mutate(perc = (n.x/n.y)*100)%>% mutate(perc_label = str_c(round(perc, digits = 0),"%"))

donate.ih.plot <- ggplot(donate.ih, aes(x = perc, y = condition, label = perc_label)) +
  geom_col(position = "stack", fill = "#56CCF2") + 
  ggtitle("Israel-Hamas") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        legend.title = element_blank()) +
  ylab(expression("Condition")) + 
  scale_y_discrete(limits = rev, labels = rev(conditions.ih)) +
  scale_x_continuous(limits = c(0, 100)) + 
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

donate.nk.plot <- ggplot(donate.nk, aes(x = perc, y = condition, label = perc_label)) +
  geom_col(position = "stack", fill = "#6FCF97") + 
  ggtitle("Nagorno-Karabakh") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.position = "none") +
  scale_y_discrete(limits = rev, labels = rev(conditions.nk)) +
  scale_x_continuous(limits = c(0, 100)) + 
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

donate.s.plot <- ggplot(donate.s, aes(x = perc, y = condition, label = perc_label)) +
  geom_col(position = "stack", fill = "#E26A6A") + 
  ggtitle("Sudan") +
  theme_light() + 
  theme(text=element_text(family="Times New Roman"), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.position = "none") +
  scale_y_discrete(limits = rev, labels = rev(conditions.s)) +
  scale_x_continuous(limits = c(0, 100)) + 
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3.5, family="Times New Roman")

donate.plot <- ggarrange(donate.ih.plot, donate.nk.plot, donate.s.plot, ncol=3)
ggsave("Plots/Three Conflicts/Donations.png", donate.plot, width=15, height=5)



################# Which Survey Did They Take First? #################

took_both <- inner_join(ncw, scw, by = "participantId")

took_both$date2.x <- word(took_both$date.x, 1)
took_both$time2.x <- word(took_both$date.x, 2)
took_both$date2.y <- word(took_both$date.y, 1)
took_both$time2.y <- word(took_both$date.y, 2)

took_both$date2.x <- mdy(took_both$date2.x)
took_both$date2.y <- mdy(took_both$date2.y)

took_both <- took_both %>%
  mutate(time2.x = str_remove_all(time2.x, ":")) %>% mutate(time2.y = str_remove_all(time2.y, ":"))
took_both$time2.x <- as.numeric(took_both$time2.x)
took_both$time2.y <- as.numeric(took_both$time2.y)

took_nk_first <- took_both %>% filter(date2.x < date2.y | (date2.x == date2.y & time2.x < time2.y))
took_s_first <- took_both %>% filter(date2.y < date2.x | (date2.y == date2.x & time2.y < time2.x))
took_same_time <- took_both %>% filter(date2.y == date2.x & time2.y == time2.x) ### How?

took_nk_first_ids <- as.list(as.character(took_nk_first$participantId))
took_s_first_ids <- as.list(as.character(took_s_first$participantId))
took_same_time_ids <- as.list(as.character(took_same_time$participantId))

ncw <- ncw %>%
  mutate(survey_order = case_when(
    participantId %in% took_nk_first_ids ~ "nk_first",
    participantId %in% took_s_first_ids ~ "sudan_first",
    participantId %in% took_same_time_ids ~ "same_time",
    .default = "nk_only"))

scw <- scw %>%
  mutate(survey_order = case_when(
    participantId %in% took_nk_first_ids ~ "nk_first",
    participantId %in% took_s_first_ids ~ "sudan_first",
    participantId %in% took_same_time_ids ~ "same_time",
    .default = "sudan_only"))
    
test <- scw %>% filter(survey_order == "sudan_first")
test <- as.list(as.character(test$participantId))
setdiff(unlist(took_s_first_ids), unlist(test))

ncw %>%
  count(participantId) %>%
  filter(n > 1)

scw %>%
  count(participantId) %>%
  filter(n > 1)

temp <- ncw %>% filter(participantId == "CF71A645A7F24E43986D0121C648A176")
temp2 <- scw %>% filter(participantId == "CF71A645A7F24E43986D0121C648A176")















