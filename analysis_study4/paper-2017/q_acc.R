
# Preparatory settings ----------------------------------------------------

## Settings for table selection
name4dbsrc$q <- "acc"
name4dbsrc$prefix <- paste("t_q", name4dbsrc$q, sep = "_")
name4dbsrc$suffix <- 
  c("city_gestures_complex",
    "city_gestures_simple",
    "city_touch_complex",
    "city_touch_simple")



# Preprocessing -----------------------------------------------------------

source("preprocessing/dataPreparation.R")



# Boxplots ----------------------------------------------------------------

## Complete variation ----
plotdata <- plotBoxplot(name4data$df, "score", "score", ylim4score, "expfocus")
plotdata <- makePrintablePlot_boxPlot(plotdata)
plotdata <- makePrintablePlot_theme(plotdata)

ggsave(filename = "q_acc_city_boxplot_expfocus.png",
       plot = plotdata,
       path = file.path(set4plot$dir4plot, "boxplots"), 
       width = 8, height = 4, units = "cm", dpi = 600)



lm(t_q_acc_dat$score ~ t_q_acc_dat$expfocus)
anova(lm(t_q_acc_dat$score ~ t_q_acc_dat$expfocus))


summary(aov(score ~ expfocus, t_q_acc_dat))
TukeyHSD((aov(score ~ expfocus, t_q_acc_dat)))
library(lsr)
etaSquared(aov(score ~ expfocus, t_q_acc_dat), type = 2, anova = T)


library(psych)
alpha(select(t_q_acc_dat, 1 + 1:9))
## Usefullness
alpha(select(t_q_acc_dat, 1 + c(1,3,5,7,9)))
## Satisfaction
alpha(select(t_q_acc_dat, 1 + c(2,4,6,8)))
