connect2db <- function() {
  
  ## Prompt for database
  outputString(paste("* Select study:", paste(set4db$select, collapse = " | ")))
  studyselect <- readline(">>> ") 
  
  ## Adjust settings
  if (studyselect %in% c(1:3, "1:3")) {
    set4db$name_short <- "study1to3"
    studyselect <- "1-3"
  }
  if (studyselect == "4")
    set4db$name_short <- "study4"
  set4db$dns <- paste(set4db$dns, studyselect, sep = "")
  set4db$name <- set4db$dns

  ## Connect to database
  dbConn(set4db, set4db$name_short)
}