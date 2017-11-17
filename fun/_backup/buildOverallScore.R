buildOverallScore <- function (name4df, q) {
  
  outputFunProc(R)

  ## Get varnames for items
  varnames <- set4items[[q]]$varnames
  
  ## Get data and compute overall scores
  dat2proc <- 
    get(name4df) %>% 
    mutate( score_overall = rowSums(.[varnames]) )
  
  ## Get scales
  name4scales <- names(set4items[[q]]$items4scales)
  
  ## Compute score for each scale
  scores <- 
    lapply(name4scales, function(n) {
    varnames4scale <- varnames[set4items[[q]]$item_nrs4scales[[n]]]
    score_temp <- rowSums(dat2proc[, varnames4scale]) %>% data.frame()
    return(score_temp)
  }) %>% bind_cols()
  names(scores) <- paste("score_", name4scales, sep = "")
  
  assign(name4df, cbind(dat2proc, scores), env = .GlobalEnv) 
  outputDone()
}