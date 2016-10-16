glances <- dbGetSrc("t_glances")


ggplot() + 
  geom_boxplot(data = glances,
               aes(x = cond,
                   y = glances_n))

ggplot() + 
  geom_boxplot(data = glances,
               aes(x = cond,
                   y = glances_dur_s/3))


lm(glances$glances_n ~ glances$cond)
anova(lm(glances$glances_n ~ glances$cond))

summary(aov(glances_n ~ cond, glances))
TukeyHSD((aov(glances_n ~ cond, glances)))



