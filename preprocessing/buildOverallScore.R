buildOverallScore <- function (name4df) {
  
  outputFunProc(R)
  
  dat2proc <- 
    get(name4df) %>% 
    mutate( score = rowSums(.[itemsets[[name4dbsrc$q]]]) )
  
  assign(name4df, dat2proc, env = .GlobalEnv) 
  
  outputDone()
}