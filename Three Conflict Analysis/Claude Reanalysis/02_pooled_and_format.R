# Pooled side effects (any A / any B post vs Control) and side x format interactions on sympathy. Needs harm.rds from 01.
suppressMessages({library(tidyverse); library(estimatr)})
all <- readRDS("harm.rds") %>% mutate(side=relevel(factor(side),"Control"))
print(table(all$survey, all$arm)); print(tapply(all$sad, list(all$survey, all$arm), function(z) sum(!is.na(z))))
r <- map_dfr(c("ih","nk","sudan"), function(s) map_dfr(c("sympathy","justified","us_lenient","milaid","ceasefire","humanit"), function(v){
  m <- lm_robust(as.formula(paste(v,"~side")), data=filter(all,survey==s)); data.frame(survey=s,dv=v,term=names(coef(m))[-1],est=round(coef(m)[-1],3),p=round(m$p.value[-1],3), ctrl=round(coef(m)[1],2))}))
write.csv(r, "effects_pooled_by_side.csv", row.names=FALSE)
print(as.data.frame(r), row.names=FALSE)
# format comparisons pooled across sides: directional effect (sign-flip B)
all <- all %>% filter(side!="Control") %>% mutate(dir=ifelse(side=="A",1,-1), fmt=relevel(factor(format),"Statistics"))
for (s in c("ih","nk","sudan")) { m <- lm_robust(I(sympathy) ~ side*fmt, data=filter(all,survey==s)); cat("\n",s,"\n"); print(round(summary(m)$coefficients[,c(1,4)],3)) }
print(all %>% bind_rows(readRDS("harm.rds") %>% filter(side=="Control")) %>% group_by(survey) %>% summarise(n=n(), notsure=round(mean(symp_nosure),2), dk=round(mean(dk_think),2)) %>% as.data.frame())
