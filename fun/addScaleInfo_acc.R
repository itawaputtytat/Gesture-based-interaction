addScaleInfo_acc <- function(name4df, colnames4items) {
  
  outputFunProc(R)

  ## Get data and column names for scales
  dat2proc <- get(name4df)
  itemfinder <- set4items$names$acc[set4items$nrs$acc_useful]
  dat2proc$scale[dat2proc$variable %in% itemfinder]  <- "usefulness"
  set4items$names$acc[set4items$nrs$acc_satis]
  dat2proc$scale[dat2proc$variable %in% itemfinder]  <- "satisfying"
  
  assign(name4df, dat2proc, env = .GlobalEnv)
  outputDone()
}