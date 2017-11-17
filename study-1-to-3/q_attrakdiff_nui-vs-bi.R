
# Preparatory settings ----------------------------------------------------

set4proc <- c()

## Settings for local data
set4proc$q <- "attrakdiff"
set4proc$name4df <- "t_q_attrakdiff"
set4proc$name4df.imp <- paste(set4proc$name4df, ".imp", sep = "")

## Settings for database table selection
set4proc$prefix <- paste("t_q", set4proc$q, sep = "_")
set4proc$suffix <- 
  c("exp1_nui_n6",
    "exp1_bi_n6")

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

means <- 
  get(set4proc$name4df) %>% 
  group_by(table) %>% 
  summarise(score_overall.avg = mean(score_overall),
            score_overall.sd = sd(score_overall))

sapply(names(set4items[[set4proc$q]]$itemnrs4scales), function (s) {
  varname4score <- paste("score_", s, sep = "")
  by(get(set4proc$name4df), get(set4proc$name4df)$table, 
     function(x) mean(x[, varname4score], na.rm = T))
})

sapply(names(set4items[[set4proc$q]]$itemnrs4scales), function (s) {
  varname4score <- paste("score_", s, sep = "")
  by(get(set4proc$name4df), get(set4proc$name4df)$table, 
     function(x) sd(x[, varname4score], na.rm = T))
})



# Visualisation: Item profiles --------------------------------------------

## Experimental focus
plotdat <- plotItemProfile_attrakdiff(set4proc$name4df, "mean", "expfocus") 
plotdat <- makePrintablePlot_ItemProfile(plotdat, set4proc$q)
grid.draw(plotdat)
