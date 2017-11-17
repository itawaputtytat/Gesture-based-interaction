
# Preparatory settings ----------------------------------------------------

set4proc <- c()
set4proc$plot_boxplots4items <- F ## Show box plots for single items?

## Settings for table selection
set4proc$name4df <- "t_q_eval"
set4proc$q <- "eval"
set4proc$prefix <- paste("t_q", set4proc$q, sep = "_")
set4proc$suffix <- 
  c("gestures",
    "touch")

#source("preprocessing/dataPreparation.R")
outputString(paste("* Loading item labels:", set4proc$q))
set4proc$varnames4items <- set4items[[set4proc$q]]$varnames



# Data processing ---------------------------------------------------------

## Load and rbind data from database
dbGetSrc_rbind("dbconn_study4", set4proc$prefix, set4proc$suffix, set4proc$name4df)

## Remove subject #13
assign(set4proc$name4df, 
       get(set4proc$name4df) %>% 
         filter(id != 4013 & id != 4001 & id != 4008))

## Remove missing cases
codePatternAsNA(set4proc$name4df, 99, set4proc$varnames4items)

# Code new variable for interaction type
codeExpFactors(set4proc$name4df, "table", 2)

# Compute scores
computeScores(set4proc$name4df, set4proc$q) 

## Create long data formats for item profiles
## Reshape to long data format for summarizing data
reshapeDat2Long(set4proc$name4df, set4proc$varnames4items)

## Compute key values
computeKeyValues(set4proc$name4df, "expfocus")



# Visualisation: Item profiles --------------------------------------------

plotdat <- plotItemProfile(set4proc$name4df, "mean", "expfocus", set4proc$q) 
plotdat <- makePrintablePlot_ItemProfile(plotdat, set4proc$q)
grid.draw(plotdat)




# Post-processing ---------------------------------------------------------

plotdat <- 
  plotdat +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), alpha = 0.5, width = 0.2) +
  theme(legend.position = "top") + 
  labs(title = "TEST",
       y = "Score")



# Export item profile -----------------------------------------------------

ggsave(filename = "q_eval_expfocus.png",
       plot = plotdat,
       path = file.path("plots/BMW_Gestures_Study4"),
       width = 14, height = 6, units = "cm", dpi = 600)
