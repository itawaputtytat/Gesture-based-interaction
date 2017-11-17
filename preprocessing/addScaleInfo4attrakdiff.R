addScaleInfo4attrakdiff <- function(srcname, dataname, var2group) {
  
  cat("* Processing with function addScaleInfo4attrakdiff \n")
  
  dataname <- 
    paste(srcname$prefix, "_", dataname$df_long_main, "expfocus", sep = "")
  
  dat2proc <- get(dataname)
  
  ## Pragmatic quality
  dat2proc$scale[dat2proc$variable %in% 
                       varnames4items[itemnr_attrakdiff_pq]]  <- "pq"
  
  ## Hedonic quality
  dat2proc$scale[dat2proc$variable %in% 
                       varnames4items[itemnr_attrakdiff_hq]]  <- "hq"
  
  ## Attractivity 
  dat2proc$scale[dat2proc$variable %in% 
                       varnames4items[itemnr_attrakdiff_att]] <- "att"
  
  ## Detailed (HQI/HQS)
  
  ## Pragmatic quality
  dat2proc$scale_detail[dat2proc$variable %in% 
                          varnames4items[itemnr_attrakdiff_pq]]  <- "pq"
  
  ## Hedonic quality (identity)
  dat2proc$scale_detail[dat2proc$variable %in% 
                          varnames4items[itemnr_attrakdiff_hqi]] <- "hqi"
  
  ## Hedonic quality (Stimulation)
  dat2proc$scale_detail[dat2proc$variable %in% 
                          varnames4items[itemnr_attrakdiff_hqs]] <- "hqs"
  
  ## Attractivity
  dat2proc$scale_detail[dat2proc$variable %in% 
                          varnames4items[itemnr_attrakdiff_att]] <- "att"
      
  assign(dataname,
         dat2proc,
         env = .GlobalEnv)
  
  cat("** Added scale for AttrakDiff in: ", dataname, "\n", sep = "")
  cat("*** Done! *** \n")
  
}