# Harmonizes ih/nk/sudan raw CSVs (Finished + consent filter) and estimates each arm vs Control (lm_robust, HC2).
# Coding: higher = more favorable to Israel / Azerbaijan / SAF ("side A"); side B = Palestinians / Armenians / RSF.
# Run from this folder. Outputs: harm.rds, effects_by_arm.csv
suppressMessages({library(tidyverse); library(estimatr)})
d <- "../Data/"
imp <- c("Not at all important"=1,"Slightly important"=2,"Moderately important"=3,"Very important"=4,"Extremely important"=5)
feel <- c("Not at all"=1,"Somewhat"=2,"Strongly"=3,"Very strongly"=4)
cfg <- list(ih=list(A="Israel",B="Palestin",Aw="Israeli",Bw="Palestinian"),
            nk=list(A="Azerbaijan",B="Armenia",Aw="Azerbaijani",Bw="Armenian"),
            sudan=list(A="SAF",B="RSF",Aw="Sudanese Armed Forces",Bw="Rapid Support Forces"))
all <- map_dfr(names(cfg), function(f){
  c <- cfg[[f]]; x <- read.csv(paste0(d,f,".csv"), check.names=FALSE) %>% filter(Finished %in% c(TRUE,"TRUE","True"), `Consent 2`=="Yes, I am")
  names(x) <- sub("^[*]","",names(x))
  s <- x$`Post-Treatment 5`
  symp <- case_when(grepl(paste0("^Entirely with the ",c$Aw),s)~5, grepl(paste0("^Mostly with the ",c$Aw),s)~4, grepl("^Equally",s)~3,
                    grepl(paste0("^Mostly with the ",c$Bw),s)~2, grepl(paste0("^Entirely with the ",c$Bw),s)~1)
  th <- x$`Post-Treatment 6`; ua <- x$`Post-Treatment 7`
  tibble(survey=f, cond=x$condition,
    side=case_when(x$condition=="Control"~"Control", grepl(c$A,x$condition)~"A", TRUE~"B"),
    format=case_when(x$condition=="Control"~"Control", grepl("_CW",x$condition)~"Witness", grepl("Personal",x$condition)~"News story", TRUE~"Statistics"),
    sympathy=symp, symp_nosure=as.numeric(s %in% c("Not sure","With neither of them")),
    justified=ifelse(grepl("Don’t know",th)|th=="",NA, as.numeric(grepl("defending",th))),
    dk_think=as.numeric(grepl("Don’t know",th)),
    us_lenient=case_when(grepl("diplomatic",ua)~2, grepl("reduce",ua)~1, grepl("should let",ua)~3),  # "diplomatic" first: that option also says "not reduce",
    ceasefire=imp[x$`Post-Treatment 10`], humanit=imp[x$`Post-Treatment 11`], milaid=imp[x$`Post-Treatment 12`],
    sad=feel[x$`Post-Treatment 4_4`], angry=feel[x$`Post-Treatment 4_3`])
}) %>% mutate(arm=ifelse(cond=="Control","Control",paste(side,format)), arm=relevel(factor(arm),"Control"))
saveRDS(all,"harm.rds")
dvs <- c("sympathy","justified","us_lenient","milaid","ceasefire","humanit","dk_think","sad")
res <- map_dfr(c("ih","nk","sudan"), function(s) map_dfr(dvs, function(v){
  m <- lm_robust(as.formula(paste(v,"~arm")), data=filter(all,survey==s)); tidy(m) %>% filter(term!="(Intercept)") %>%
  transmute(survey=s, dv=v, arm=sub("arm","",term), est=estimate, p=p.value, ctrl=coef(m)[1])}))
write.csv(res, "effects_by_arm.csv", row.names=FALSE)
res %>% mutate(cell=sprintf("%+.2f%s", est, ifelse(p<.01,"**",ifelse(p<.05,"* ",ifelse(p<.1,"+ ","  "))))) %>%
  select(survey,dv,arm,cell) %>% pivot_wider(names_from=arm,values_from=cell) %>% print(n=50, width=200)
