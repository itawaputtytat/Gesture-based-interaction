cat("Prepare data... \n\n")

## Item names for labeling
cat("* Loading itemsets for: ", name4dbsrc$q, "\n", sep = "")
varnames4items <- itemsets[[name4dbsrc$q]]

## Load and rbind data from database
getRbindDataFromDB(name4dbsrc, name4data$df_main)

## Remove missing cases
setCasesNA(name4data$df)

# Code new variable for interaction type
codeInteractionType(name4data$df)

# Build overall score
buildOverallScore(name4data$df)  

# Reshape to long data format for summarizing data
dataLong4groupVar(name4data$df, "expfocus")
dataStats4groupVar(name4data$df, "expfocus")

dataLong4groupVar(name4data$df, "typegeneric")
dataStats4groupVar(name4data$df, "typegeneric")

dataLong4groupVar(name4data$df, "typedetail")
dataStats4groupVar(name4data$df, "typedetail")

if (name4dbsrc$q == "attrakdiff") {
  
  ## Add variable for scale identification
  addScaleInfo4attrakdiff(name4dbsrc, name4data, "expfocus")
  
  ## Compute mean scores for PQ and HQ
  computeMeanSD4AttrakDiff(name4dbsrc, name4data, "expfocus")
  
  ## Reshape again
  dcastData4AttrakDiff_stats4scale(name4dbsrc, name4data, "expfocus")
  
}

cat("*** Done! *** \n")


pauseAndContinue()