
t_q_acc.imp2 <- 
  t_q_acc.imp %>% 
  select(id, itype_generic, ilevel, scenario,
         score_satisfying) %>% 
  mutate(itype_generic = as.factor(itype_generic),
         ilevel = as.factor(ilevel),
         scenario = as.factor(scenario))

model_satisfying <- 
  ezANOVA(data = t_q_acc.imp2, 
          dv = .(score_satisfying), 
          wid = .(id), 
          within = .(itype_generic, ilevel, scenario), 
          type = 3, 
          detailed = T,
          return_aov = T)
model_satisfying$ANOVA

options("contrasts")
options_constrast_backup <- options("contrasts")
options(contrasts = c("contr.helmert", "contr.poly"))
options(contrasts = options_constrast_backup$contrasts)


library(nlme)
baseline <- 
  lme(score_satisfying ~ 1, 
      random = ~1|id/itype_generic/ilevel/scenario, 
      data = t_q_acc.imp, 
      method = "ML")

baseline.itype <- 
  update(baseline, .~. + itype_generic)

baseline.itype.ilevel <- 
  update(baseline.itype, .~. + ilevel)

baseline.itype.ilevel2 <- 
  update(baseline.itype, .~. + itype_generic:ilevel)

baseline.itype.ilevel.scenario <- 
  update(baseline.itype.ilevel2, .~. + scenario)

baseline.itype.ilevel.scenario2 <- 
  update(baseline.itype.ilevel.scenario, .~. + itype_generic:scenario)

baseline.itype.ilevel.scenario3 <- 
  update(baseline.itype.ilevel.scenario2, .~. + itype_generic:ilevel)

baseline.itype.ilevel.scenario4 <- 
  update(baseline.itype.ilevel.scenario3, .~. + itype_generic:ilevel:scenario)

anova(baseline,
      baseline.itype,
      baseline.itype.ilevel,
      baseline.itype.ilevel.scenario,
      baseline.itype.ilevel2,
      baseline.itype.ilevel.scenario2,
      baseline.itype.ilevel.scenario3,
      baseline.itype.ilevel.scenario4)

summary(baseline.itype.ilevel.scenario4)



test <- 
  spread(t_q_acc.imp,  expfocus, score_satisfying) %>% 
  group_by(id) %>% 
  summarise(city_gestures_complex = max(city_gestures_complex, na.rm = T),
            city_gestures_simple = max(city_gestures_simple, na.rm = T),
            city_touch_complex = max(city_touch_complex, na.rm = T),
            city_touch_simple = max(city_touch_simple, na.rm = T),
            motorway_gestures_complex = max(motorway_gestures_complex, na.rm = T),
            motorway_gestures_simple = max(motorway_gestures_simple, na.rm = T),
            motorway_touch_complex = max(motorway_touch_complex, na.rm = T),
            motorway_touch_simple = max(motorway_touch_simple, na.rm = T))

write.table(test, "clipboard", sep="\t", row.names=FALSE)


## Insight
## itype significant with F(1, 36) = 38.66 with p < 0.001 and generalised eta-squared 0.19

library(nlme)
model2 <- 
  lme(score_satisfying ~ itype_generic + ilevel + scenario, 
      random = ~1 | id/itype_generic, 
      data = t_q_acc.imp,
      method = "ML")
anova(model2)



dat2analyse <- t_q_acc.imp
dat2analyse2 <- t_q_acc.imp

dat2analyse$itype_generic <- as.factor(dat2analyse2$itype_generic)
dat2analyse$itype_ilevel <- as.factor(dat2analyse2$itype_ilevel)
dat2analyse$scenario <- as.factor(dat2analyse2$scenario)

options_constrast_backup <- options("contrasts")

options(contrasts = c("contr.helmert", "contr.poly"))

model1 <- lm(score_overall ~ itype_generic * ilevel * scenario, data = dat2analyse2)
car::Anova(model1, type = 3)



anova(lm(score_overall ~ 1, dat2analyse2), 
      lm(score_overall ~ id, dat2analyse2))

