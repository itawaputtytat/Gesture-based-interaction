addScaleInfo4attrakdiff <- function(srcname, dataname, var2group) {
  
  cat("* Processing with function addScaleInfo4attrakdiff \n")
  
  dataname <- 
    paste(srcname$prefix, "_", dataname$df_long_main, "expfocus", sep = "")
  
  data2process <- get(dataname)
  
  ## Pragmatic quality
  data2process$scale[data2process$variable %in% 
                       varnames4items[itemnr_attrakdiff_pq]]  <- "pq"
  
  ## Hedonic quality
  data2process$scale[data2process$variable %in% 
                       varnames4items[itemnr_attrakdiff_hq]]  <- "hq"
  
  ## Attractivity 
  data2process$scale[data2process$variable %in% 
                       varnames4items[itemnr_attrakdiff_att]] <- "att"
  
  ## Detailed (HQI/HQS)
  
  ## Pragmatic quality
  data2process$scale_detail[data2process$variable %in% 
                          varnames4items[itemnr_attrakdiff_pq]]  <- "pq"
  
  ## Hedonic quality (identity)
  data2process$scale_detail[data2process$variable %in% 
                          varnames4items[itemnr_attrakdiff_hqi]] <- "hqi"
  
  ## Hedonic quality (Stimulation)
  data2process$scale_detail[data2process$variable %in% 
                          varnames4items[itemnr_attrakdiff_hqs]] <- "hqs"
  
  ## Attractivity
  data2process$scale_detail[data2process$variable %in% 
                          varnames4items[itemnr_attrakdiff_att]] <- "att"
      
  assign(dataname,
         data2process,
         env = .GlobalEnv)
  
  cat("** Added scale for AttrakDiff in: ", dataname, "\n", sep = "")
  cat("*** Done! *** \n")
  
}