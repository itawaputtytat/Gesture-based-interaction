qPreprocess <- function() {
  
  ## Load and rbind data from database
  dbGetSrc_rbind("dbconn_study4", set4proc$prefix, set4proc$suffix, set4proc$name4df)
  
  ## Remove subject #13
  assign(set4proc$name4df, 
         get(set4proc$name4df) )#%>% 
           # filter(id != 4013 & id != 4001))
          # filter(id != 4013))
  
  ## Remove missing cases
  ## DOES NOT WORK
  #codePatternAsNA(set4proc$name4df, 99, set4proc$varnames4items)
  
  # Code new variable for interaction type
  codeExpFactors(set4proc$name4df, "table", 2)
  
  ## Reshape to long data format for summarizing data
  ## Necessary for item profiles (and attrakdiff matrix plot)
  reshapeDat2Long(set4proc$name4df, set4proc$varnames4items)
  
  ## Compute key values mean and median
  ## Necessary for item profiles
  computeKeyValues(set4proc$name4df, "expfocus")
  computeKeyValues(set4proc$name4df, "itype_generic")
  if (set4proc$q != "attrakdiff") {
    computeKeyValues(set4proc$name4df, "itype_ilevel")
  }
  
  ## Necessary for matrix plot
  if (name4dbsrc$q == "attrakdiff") {
    
    ## Add variable for scale identification
    addScaleInfo_attrakdiff(set4proc$name4df, ".long")
    #addScaleInfo_attrakdiff(set4proc$name4df, ".long.stats_expfocus")
    #addScaleInfo_attrakdiff(set4proc$name4df, ".long.stats_itype_generic")
    
    ## Compute mean scores for PQ and HQ
    #computeMeansSD_attrakdiff(set4proc$name4df, ".long", "expfocus")
    computeMeansSD_attrakdiff(set4proc$name4df, ".long", "expfocus")
    
    ## Reshape again
    reshapeDat2Wide_attrakdiff_stats4scales(set4proc$name4df, ".long.stats4scales", "expfocus")
  }
  
  #ä Compute scores
  ## Necessary for ANOVA
  computeScores(set4proc$name4df, set4proc$q) 
}