computeScores <- function (name4df, q) {
  
  outputFunProc(R)

  ## Get varnames for items
  varnames <- set4items[[q]]$varnames
  
  ## Get data and compute overall scores
  dat2proc <- 
    get(name4df) %>% 
    #mutate( score_overall = rowSums(.[varnames]) )
    mutate( score_overall = rowMeans(.[varnames]) )
  
  ## Get scales
  name4scales <- names(set4items[[q]]$itemnrs4scales)

  ## Compute score for each scale
  lapply(name4scales, function(n) {
    itemnrs4scale <- set4items[[q]]$itemnrs4scales[[n]]
    varnames4scale <- varnames[itemnrs4scale]
    
    ## In case of NASA-TLX, there is only one item per scale
    if (length(varnames4scale) > 1) {
      
      ## Compute means for attrakdiff instead of sums (changed to mean; e.g. acceptance)
      if (set4proc$q == "attrakdiff") {
        score_temp <- rowMeans(dat2proc[, varnames4scale]) %>% data.frame()
      } else {
        score_temp <- rowMeans(dat2proc[, varnames4scale]) %>% data.frame() 
      }
      
    } else {
      score_temp <- dat2proc[, varnames4scale]
    }
        
    varname4score <- paste("score_", n, sep = "")
    dat2proc <- dat2proc %>% mutate_(.dots = setNames(list(0), varname4score))
    dat2proc[, varname4score] <<- score_temp
    return(dat2proc)
  }) 
  
  #assign(paste(name4df, ".score", sep = ""), dat2proc, env = .GlobalEnv) 
  assign(name4df, dat2proc, env = .GlobalEnv) 
  outputDone()
}