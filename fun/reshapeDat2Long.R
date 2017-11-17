reshapeDat2Long <- function(name4df, colnames4items) {
  
  outputFunProc(R)
  
  ## Get data and reshape
  dat2proc <- get(name4df) %>% melt(measure.vars = colnames4items)
  
  # Reverse factor levels of items for profile plotting
  #dat2proc <- as.data.frame(reverseFactorLevels(dat2proc, "variable"))
  
  ## Save to new object name
  name4df_new <- paste(name4df, ".long", sep = "")
  assign(name4df_new, dat2proc, env = .GlobalEnv)

  outputString(paste("* Created object:", name4df_new))
  outputDone()
}
