
# Preparatory settings ----------------------------------------------------

set4proc <- c()

## Settings for local data
set4proc$q <- "acc"
set4proc$name4df <- "t_q_acc"
set4proc$name4df.imp <- paste(set4proc$name4df, ".imp", sep = "")

## Settings for database table selection
set4proc$prefix <- paste("t_q", set4proc$q, sep = "_")
set4proc$suffix <- 
  c("exp3_fb1_nui_n6",
    "exp3_fb2_nui_n6",
    "exp3_fb3_nui_n6")

## Show box plots for single items?
set4proc$plot_boxplots4items <- F 

## Settings for questionnaire
#source("preprocessing/dataPreparation.R")
outputString(paste("* Loading item labels:", set4proc$q))
set4proc$varnames4items <- set4items[[set4proc$q]]$varnames



# Data pre-processing -----------------------------------------------------

## Load and rbind data from database
dbGetSrc_rbind("dbconn_study1to3", 
               set4proc$prefix, set4proc$suffix, 
               set4proc$name4df)

## Code missing cases (if not already NA)
codePatternAsNA(set4proc$name4df, 99, set4proc$varnames4items)

# Code new variable for interaction type
codeExpFactors(set4proc$name4df, "table", 1)



# Compute scores and key values -------------------------------------------

# Compute scores
computeScores(set4proc$name4df, set4proc$q) 

## Create long data formats for item profiles
## Reshape to long data format for summarizing data
reshapeDat2Long(set4proc$name4df, set4proc$varnames4items)

## Compute key values for each item
computeKeyValues(set4proc$name4df, "expfocus")
computeKeyValues(set4proc$name4df, "itype_generic")
computeKeyValues(set4proc$name4df, "itype_detail")



# Compute means and sd ----------------------------------------------------

acc_means <- 
  get(set4proc$name4df) %>% 
  group_by(table) %>% 
  summarise(score_usefulness.avg = mean(score_usefulness),
            score_satisfying.avg = mean(score_satisfying),
            score_usefulness.sd = sd(score_usefulness),
            score_satisfying.sd = sd(score_satisfying))



# Visualisation: Item profiles --------------------------------------------

## Experimental focus
plotdat <- plotItemProfile(set4proc$name4df, "mean", "expfocus", set4proc$q) 
plotdat <- makePrintablePlot_ItemProfile(plotdat, set4proc$q)
grid.draw(plotdat)



# Inference statistics ----------------------------------------------------

#model_usefulness <- ezANOVA(data = t_q_acc, dv = score_usefulness, wid = id, within = .(scenario, itype, ilevel), type = 3, detailed = F)
model_usefulness <- 
  ezANOVA(data = t_q_acc, 
          dv = score_usefulness, 
          wid = id, 
          within = .(expfocus), 
          type = 2, 
          detailed = F,
          return_aov = T)
model_usefulness$ANOVA

## Insight:
## itype significant with F(1, 36) = 40.47 with p < 0.001 and generalised eta-squared 0.17

# pairwise.t.test(t_q_acc$score_usefulness, 
#                 t_q_acc$itype, 
#                 paired = T, 
#                 p.adjust.method = "bonferroni")


## Compute with complete case removal
# Re-compute scores
model_satisfying <- 
  ezANOVA(data = t_q_acc, 
          dv = score_satisfying, 
          wid = id, 
          within = .(expfocus), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_satisfying$ANOVA