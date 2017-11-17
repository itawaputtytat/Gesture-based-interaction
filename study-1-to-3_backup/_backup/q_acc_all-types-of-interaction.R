# Objective ---------------------------------------------------------------

## Acceptance comparison for all types of interaction



# Preparatory settings ----------------------------------------------------

## Show box plots for single items?
showboxplot4items <- F

## Settings for table selection
name4dbsrc <- c()
name4dbsrc$q <- "acc"
name4dbsrc$prefix <- paste("t_q", name4dbsrc$q, sep = "_")
name4dbsrc$suffix <- 
  c("exp1_mouse",
    "exp2_mouse",
    "exp3_mouse",
    "exp1_nui_n6",
    "exp2_nui_n4",
    "exp3_nui_n2",
    "exp1_bi_n6")

## Settings for data names
name4data <- c()
name4data$df_main <- "expx_interacttypes"
name4data$df_long_main <- paste(name4data$df_main, "_long4", sep = "")

# Settings for plot scaling
yscale4items <- c(-2.5, 2.5)
yscale4score <- c(-25, 25)



# Preprocessing -----------------------------------------------------------

cat("Prepare data... \n")

# Item names for labeling
varnames4items <- itemsets[[name4dbsrc$q]]

## Load and rbind data from database
getRbindDataFromDB(name4dbsrc, name4data$df_main)

# Remove missing cases
setCasesNA(name4data$df)

# Code new variable for interaction type
codeInteractionType(name4data$df)

# Build overall score
assign(name4data$df, 
       get(name4data$df) %>% 
         mutate(score = rowSums(.[itemsets[[name4dbsrc$q]]])))   

# Reshape to long data format for summarizing data
dataLong4groupVar(name4data$df, "expfocus")
dataStats4groupVar(name4data$df, "expfocus")

# Adjustments for interaction type
dataLong4groupVar(name4data$df, "typegeneric")
dataStats4groupVar(name4data$df, "typegeneric")

dataLong4groupVar(name4data$df, "typedetail")
dataStats4groupVar(name4data$df, "typedetail")

cat("*** Done! *** \n")

pauseAndContinue()



# Visualisation -----------------------------------------------------------

# Boxplot for each item
if(showboxplot4items == T) {
  invisible(lapply(varnames4items, function (x)
    plotBoxplot(name4data$main, x, x, yscale_items, "expfocus")))
} else
  cat("Skipped: Plotting of boxplots for each item \n")

pauseAndContinue()


# Boxplot for overall score
plotBoxplot(name4data$df, "score", "score", yscale4score, "expfocus")

pauseAndContinue()


# Item profiles
plotItemProfile(name4data$df, "mean", "expfocus", name4dbsrc$q)
plotItemProfile(name4data$df, "mean", "typegeneric", name4dbsrc$q)
plotItemProfile(name4data$df, "mean", "typedetail", name4dbsrc$q)
