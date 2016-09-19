## Remove missing cases
# identify by 99 values

setCasesNA <- function (dataname) {
  
  cat("* Processing with function: setCasesNA \n")
  cat("** Using data: ", dataname, "\n", sep = "")
  
  data2recode <- get(dataname)
  
  idfinder <- data2recode$id[which(data2recode[, varnames4items] == 99)]
  idfinder <- idfinder[1]
  
  if (is.na(idfinder) == F)
    data2recode[data2recode$id == idfinder, varnames4items] <- NA
  
  assign(dataname,
         data2recode,
         env = .GlobalEnv)
  
  cat("** Set cases to NA (if necessary in: ", dataname, ") \n", sep = "")
  cat("*** Done! *** \n\n")
  
}
