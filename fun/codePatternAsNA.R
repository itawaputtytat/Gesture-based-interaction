## Code value pattern as NA
## Already existing NA will not be changed

codePatternAsNA <- function(name4df, pattern, colnames4items) {
  
  outputFunProc(R)
  
  ## Get data
  dat2proc <- get(name4df)
  
  ## Initialise collector for each missing case and column
  idcoll <- c()
  
  ## Look for rows in each specified columns
  ## ... which contains cells contents matching to pattern
  for(r in 1:nrow(dat2proc)) {
    
    ## Find columns which matches pattern
    colnr_temp <- which(dat2proc[r, colnames4items] == pattern)
    colnames_temp <- colnames4items[colnr_temp]
    tablename_temp <-dat2proc[r, "table"]
    
    if (length(colnames_temp) != 0) {
      
      ## Find corresponding id
      idfinder_temp <- dat2proc[r, "id"]
      
      ## Save id and found columns (and table)
      idcoll <- c(idcoll, idfinder_temp)
      
      cat(paste("ID:", idfinder_temp, "in", tablename_temp, "\n"))
      cat(paste("*", colnames_temp, sep = "", collapse = "\n"))
      cat("\n")
      
      ## Replace pattern with NA
      dat2proc[r, colnames_temp] <- NA
    }
  }
  
  if(length(idcoll) == 0)
    outputString("* No pattern matchings")
 
  assign(name4df, dat2proc, env = .GlobalEnv)
  outputDone()
  
  #return(idcoll)
}