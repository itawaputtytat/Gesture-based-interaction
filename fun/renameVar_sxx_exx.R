renameVar_sxx_exx <- function (dat) {
  
  outputFunProc(R)
  outputString("* Renaming pxx related variables")
  
  ## Get data names in current workspace which contain pattern like "s01"
  pattern <- "s\\d\\d_e\\d\\d"
  
  ## Get old (current) variable names and rename sxx related variables
  varnames_old <- names(dat)
  varnames_new <- gsub(pattern, "sxx_exx", varnames_old)
  names(dat) <- varnames_new
  
  outputDone()
  
  return(dat)
}
