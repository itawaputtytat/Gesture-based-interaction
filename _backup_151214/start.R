
# Reset R-Environment -----------------------------------------------------

cat("Resetting R-Environment ... \n")

rm(list = ls())

cat("*** Done! *** \n\n")



# Intialise settings ------------------------------------------------------

setlanguage = "eng"

cat("Setting standards for data names ... \n")
cat("* (If necessary look for variable name4data) \n")

cat("*** Done! *** \n\n")


cat("Initialising variable: name4dbsrc ... \n")
cat("* (Adjustment in individual analysis files) \n") 

name4dbsrc <- c()

cat("*** Done! *** \n\n")



# Load libraries ----------------------------------------------------------

cat("Loading libraries ... \n")

library(RPostgreSQL) 
library(dplyr)
library(ggplot2)
library(grid) # for legend.key.height = unit(...)
library(gridExtra) # for gtable_width
library(reshape2)

cat("*** Done! *** \n\n")



# Connect to database -----------------------------------------------------

cat("Connecting to database ... \n")

## Prompt for database
cat("* Select database / study (\"1-3\"/\"4\")? \n")
studyselect <- readline(">>> ")

## Connect to database
source("db/dbconn.R")

cat("*** Done! *** \n\n")



# Initialise R-functions --------------------------------------------------

cat("Initialising functions ... \n")

## Pause-and-Continue
source("processing/pauseAndContinue.R")

#source("db/loadData.R") # was necessary in Framework_V1
dbquery <- c()
source("db/dbGetSrc.R")
source("db/dbListViews.R")

## Pre-Processing
# source("preprocessing/createTemplate_name4data.R")
source("processing/revLevels.R")
source("preprocessing/getRbindDataFromDB.R")
source("preprocessing/setCasesNA.R")
source("preprocessing/codeInteractionType.R")
source("preprocessing/buildOverallScore.R")
source("preprocessing/buildOverallScore4AttrakDiff.R")
source("preprocessing/dataLong4groupVar.R")
source("preprocessing/dataStats4groupVar.R")
source("preprocessing/addScaleInfo4attrakdiff.R")
source("preprocessing/computeMeanSD4AttrakDiff.R")
source("preprocessing/dcastData4AttrakDiff_stats4scale.R")


# Postprocessing ----------------------------------------------------------
source("postprocessing/adjustMarginGtable.R")


## Visualisations
source("vis/plotSettings.R")
source("vis/createLabels4PlotLegend.R")
source("vis/plotBoxplot.R")
source("vis/boxplot4Items.R")
source("vis/plotItemProfile.R")
source("vis/attrakdiff_rect.R")
source("vis/plotItemProfile4attrakdiff_template.R")
source("vis/plotItemProfile4attrakdiff.R")
source("vis/plotMatrix4attrakdiff_template.R")
source("vis/plotMatrix4attrakdiff.R")
source("vis/plotItemProfile_post4nat.R")



# Postprocessing ----------------------------------------------------------
source("postprocessing/reset_name4data.R")
source("postprocessing/makePrintablePlot_boxPlot.R")
source("postprocessing/makePrintablePlot_itemProfile.R")
source("postprocessing/makePrintablePlot_theme.R")
name4data <- c()
reset_name4data()


#source("processing/addScaleInfo4nat.R")
#source("postprocessing/addScaleInfo4attrakdiff.R")

cat("*** Done! *** \n\n")



# Initialise item labels --------------------------------------------------

cat("Initialising item labels ... \n")

itemsets <- c()
labels <- c()
source("resources/qlabels.R")

cat("*** Done! *** \n\n")




