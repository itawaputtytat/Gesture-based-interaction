
# Preparatory settings ----------------------------------------------------

set4proc <- c()
set4proc$plot_boxplots4items <- F ## Show box plots for single items?

## Settings for table selection
set4proc$name4df <- "dat_attrakdiff"
set4proc$q <- "attrakdiff"
set4proc$prefix <- paste("t_q", set4proc$q, sep = "_")
set4proc$suffix <- 
  c("gestures",
    "touch")

#source("preprocessing/dataPreparation.R")
outputString(paste("* Loading item labels:", set4proc$q))
set4proc$varnames4items <- set4items[[set4proc$q]]$varnames



# Data processing ---------------------------------------------------------

qPreprocess()



# Visualisation: Item profiles --------------------------------------------

plotdat <- plotItemProfile_attrakdiff(set4proc$name4df, "mean", "itype_generic")
grid.draw(plotdat)



# Visualisation: Matrix ---------------------------------------------------

plotdat <- plotMatrix_attrakdiff(set4proc$name4df, ".long.stats4scales_wide", "expfocus")
grid.draw(plotdat)



# Reliability -------------------------------------------------------------

computeReliability()



# Visualisation: Boxplots -------------------------------------------------

plotdat <- plotBoxplot(set4proc$name4df, "score_overall", "expfocus")
grid.draw(plotdat)

plotdat <- plotBoxplot(set4proc$name4df, "score_pq", "expfocus")
grid.draw(plotdat)

plotdat <- plotBoxplot(set4proc$name4df, "score_hqi", "expfocus")
grid.draw(plotdat)

plotdat <- plotBoxplot(set4proc$name4df, "score_hqs", "expfocus")
grid.draw(plotdat)

plotdat <- plotBoxplot(set4proc$name4df, "score_hq", "expfocus")
grid.draw(plotdat)

plotdat <- plotBoxplot(set4proc$name4df, "score_att", "expfocus")
grid.draw(plotdat)


# Inference statistics ----------------------------------------------------

model_attrakdiff <- 
  ezANOVA(data = get(set4proc$name4df), 
          dv = score_hq, 
          wid = id, 
          within = .(itype_generic), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_attrakdiff$ANOVA

model_attrakdiff <- 
  ezANOVA(data = get(set4proc$name4df) %>% filter(id != 4038), 
          dv = score_pq, 
          wid = id, 
          within = .(itype_generic), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_attrakdiff$ANOVA
