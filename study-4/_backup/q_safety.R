
# Preparatory settings ----------------------------------------------------

set4proc <- c()
set4proc$plot_boxplots4items <- F ## Show box plots for single items?

## Settings for table selection
set4proc$name4df <- "t_q_safety"
set4proc$q <- "safety"
set4proc$prefix <- paste("t_q", set4proc$q, sep = "_")

#source("preprocessing/dataPreparation.R")
outputString(paste("* Loading item labels:", set4proc$q))
set4proc$varnames4items <- set4items[[set4proc$q]]$varnames



# Data processing ---------------------------------------------------------

## Load data
assign(set4proc$name4df, dbGetSrc("dbconn_study4", set4proc$name4df))

## Remove subject #13
assign(set4proc$name4df, 
       get(set4proc$name4df) %>% 
         filter(id != 4013 & id != 4001 & id != 4008))


## Remove missing cases
codePatternAsNA(set4proc$name4df, 99, set4proc$varnames4items)

# Compute scores
computeScores(set4proc$name4df, set4proc$q) 

## Reshape to long data format for summarizing data
## Necessary for item profiles (and attrakdiff matrix plot)
reshapeDat2Long(set4proc$name4df, set4proc$varnames4items)

## Compute key values
computeKeyValues(set4proc$name4df)



# Visualisation: Item profiles --------------------------------------------

plotdat <- plotItemProfile(set4proc$name4df, "mean", q = set4proc$q) 
plotdat <- makePrintablePlot_ItemProfile(plotdat, set4proc$q)
grid.draw(plotdat)
