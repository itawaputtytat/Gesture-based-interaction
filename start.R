
# Version info ------------------------------------------------------------

library(puttytat4R)
puttytat4R::.outputFunProc_status(F)
outputSectionTitle("Project: Gesture-based interaction", char_aes = "#", sepline_char = "=")
outputString("* Framework V3", type = "message")



# Database ----------------------------------------------------------------

outputSectionTitle("Database")
outputString("Attaching libraries ...")
library(RPostgreSQL)
outputDone(step = T)

outputString("Initialise set4db")
dbInitSettings(dir_name = "settings")
outputDone(step = T)

outputString("Connecting to database ...")
dbConnectOperator()
outputDone(step = T)

sourceWithEcho("fun/dbGetQuery_batch.R")
sourceWithEcho("fun/dbCreateQueryString.R")
sourceWithEcho("fun/createVector_var_sxx.R")
sourceWithEcho("settings/sett_id_names.R")
sourceWithEcho("fun/renameVar_sxx_exx.R")
sourceWithEcho("fun/intrpldf_batch.R")
sourceWithEcho("fun/deparseDataFunArg.R")
library(dplyr)
library(ggplot2)
sourceWithEcho("fun/codePedalActivity.R")
library(lazyeval)
library(zoo)

# 
# # Data processing ---------------------------------------------------------
# 
# outputSectionTitle("Data processing")
# outputString("Attaching libraries ...")
# library(dplyr)
# library(tidyr)
# library(reshape2)
# library(mice) ## Deal with missing values
# library(psych) ## Reliability
# library(ez) ## ANOVA
# library(lsr) ## Eta-Squared
# outputDone(step = T)
# 
# outputString("Attaching functions ...")
# source("fun/qPreprocess.R")
# source("fun/dbGetSrc_rbind.R")
# source("fun/codePatternAsNA.R")
# source("fun/codeExpFactors.R")
# source("fun/computeScores.R")
# source("fun/reshapeDat2Long.R")
# source("fun/reverseFactorLevels.R")
# source("fun/computeKeyValues.R")
# source("fun/computReliability.R")
# source("fun/addScaleInfo_attrakdiff.R")
# source("fun/computeMeansSD_attrakdiff.R")
# source("fun/reshapeDat2Wide_attrakdiff_stats4scales.R")
# outputDone(step = T)
# 
# 
# 
# # Visualisation -----------------------------------------------------------
# 
# outputString("Attaching libraries ...")
# library(ggplot2)
# library(grid) ## Draw TabeGrob 
# outputDone(step = T)
# 
# outputString("Attaching functions ...")
# source("fun/plotBoxplot.R")
# source("fun/createLabels4PlotLegend.R")
# source("fun/plotBoxplot.R")
# source("fun/plotItemProfile.R")
# source("fun/makePrintablePlot_boxplot.R")
# source("fun/makePrintablePlot_itemProfile.R")
# source("fun/makePrintablePlot_theme.R")
# source("fun/plotItemProfile_attrakdiff.R")
# source("fun/plotMatrix_attrakdiff.R")
# outputDone(step = T)



# Initialise settings and ressources --------------------------------------

# outputSectionTitle("Settings and ressources")
# 
# outputString("Attaching settings ...")
# source("settings/set4items.R")
# source("settings/set4plot.R")
# outputDone(step = T)
# 
# outputSectionTitle("Ressources")
# source("resources/plotItemProfile_attrakdiff_template.R") ## requires ggplot2
# source("resources/plotMatrix_attrakdiff_template.R")
# outputDone(step = T)
# 
# 
# 
















# library(ggplot2)
# library(grid) # for legend.key.height = unit(...)
# library(gridExtra) # for gtable_width
# library(reshape2)
# outputDone()

# 
# 
# 
# source("init/items_study4.R")
# 
# test <-
#   data.frame(cond = factor(rep(c("A", "B"), each= 200)),
#              rating = c(rnorm(200), rnorm(200, mean= .8)))
# 
# 
# plotdat_test <- 
#   ggplot(test) + 
#   geom_point(aes(x = cond, y = rating))
#   
# 
# 
# 
# cat("Setting standards for data names ... \n")
# cat("* (If necessary look for variable name4data) \n")
# cat("*** Done! *** \n\n")
# 
# cat("Initialising variable: name4dbsrc ... \n")
# cat("* (Adjustment in individual analysis files) \n") 
# name4dbsrc <- c()
# cat("*** Done! *** \n\n")
# 
# 
# # Initialise R-functions --------------------------------------------------
# 
# ## Pre-Processing
# # source("preprocessing/createTemplate_name4data.R")
# source("processing/revLevels.R")
# source("preprocessing/getRbindDataFromDB.R")
# source("preprocessing/setCasesNA.R")
# source("preprocessing/codeInteractionType.R")
# source("preprocessing/buildOverallScore.R")
# source("preprocessing/buildOverallScore4AttrakDiff.R")
# source("preprocessing/dataLong4groupVar.R")
# source("preprocessing/dataStats4groupVar.R")
# source("preprocessing/addScaleInfo4attrakdiff.R")
# source("preprocessing/computeMeanSD4AttrakDiff.R")
# source("preprocessing/dcastData4AttrakDiff_stats4scale.R")
# 
# 
# # Postprocessing ----------------------------------------------------------
# 
# #source("postprocessing/adjustMarginGtable.R")
# 
# 
# ## Visualisations
# source("vis/plotSettings.R")
# source("vis/createLabels4PlotLegend.R")
# source("vis/plotBoxplot.R")
# source("vis/boxplot4Items.R")
# source("vis/plotItemProfile.R")
# source("vis/attrakdiff_rect.R")
# source("vis/plotItemProfile4attrakdiff_template.R")
# source("vis/plotItemProfile4attrakdiff.R")
# #source("vis/plotMatrix4attrakdiff_template.R")
# source("vis/plotMatrix4attrakdiff.R")
# source("vis/plotItemProfile_post4nat.R")
# 
# 
# # Postprocessing ----------------------------------------------------------
# source("postprocessing/reset_name4data.R")
# source("postprocessing/makePrintablePlot_boxPlot.R")
# source("postprocessing/makePrintablePlot_itemProfile.R")
# source("postprocessing/makePrintablePlot_theme.R")
# name4data <- c()
# reset_name4data()
# 
# 
# #source("processing/addScaleInfo4nat.R")
# #source("postprocessing/addScaleInfo4attrakdiff.R")
# 
# cat("*** Done! *** \n\n")
# 
# 
# 



