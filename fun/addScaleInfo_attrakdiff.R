addScaleInfo_attrakdiff <- function(name4df, suffix) {
  
  outputFunProc(R)
  
  name4df <- paste(name4df, suffix, sep = "")
  dat2proc <- get(name4df) %>% data.frame()
  
  varnames4items <- set4items$attrakdiff$varnames
  
  ## Pragmatic quality
  itemfinder <- set4items$attrakdiff$itemnrs4scales$pq
  dat2proc$scale[dat2proc$variable %in% varnames4items[itemfinder]]  <- "pq"
  
  ## Hedonic quality
  itemfinder <-set4items$attrakdiff$itemnrs4scales$hq
  dat2proc$scale[dat2proc$variable %in% varnames4items[itemfinder]]  <- "hq"
  
  ## Attractivity 
  itemfinder <-set4items$attrakdiff$itemnrs4scales$att
  dat2proc$scale[dat2proc$variable %in% varnames4items[itemfinder]]  <- "att"
  
  ## Detailed (HQI/HQS)
  
  ## Pragmatic quality
  itemfinder <-set4items$attrakdiff$itemnrs4scales$pq
  dat2proc$scale_detail[dat2proc$variable %in% varnames4items[itemfinder]]  <- "pq"
  
  ## Hedonic quality (identity)
  itemfinder <-set4items$attrakdiff$itemnrs4scales$hqi
  dat2proc$scale_detail[dat2proc$variable %in% varnames4items[itemfinder]]  <- "hqi"
  
  ## Hedonic quality (Stimulation)
  itemfinder <-set4items$attrakdiff$itemnrs4scales$pqs
  dat2proc$scale_detail[dat2proc$variable %in% varnames4items[itemfinder]]  <- "hqs"
  
  ## Attractivity
  itemfinder <-set4items$attrakdiff$itemnrs4scales$att
  dat2proc$scale_detail[dat2proc$variable %in% varnames4items[itemfinder]]  <- "att"
  
  assign(name4df, dat2proc, env = .GlobalEnv)
  outputDone()
}