# Objective ---------------------------------------------------------------

# New variable for table: mouse vs. nui vs. bi

codeInteractionType <- function (dataname) {
  
   cat("* Processing with function: codeInteractionType \n")
  
  data2recode <- get(dataname) %>% select(table)
  
  data2recode$typedetail[grepl("mouse", data2recode$table)] <- "mouse"
  data2recode$typedetail[grepl("nui", data2recode$table)]   <- "nui"
  data2recode$typedetail[grepl("bi", data2recode$table)]    <- "bi"
  
  data2recode$typegeneric[grepl("mouse", data2recode$table)] <- "mouse"
  data2recode$typegeneric[grepl("nui", data2recode$table) |
                             grepl("bi", data2recode$table)] <- "gesture"
  
  assign(dataname,
         cbind(get(dataname),
               typedetail = data2recode$typedetail,
               typegeneric = data2recode$typegeneric),
         env = .GlobalEnv)
  
  cat("** Coded interaction types ... \n")
  cat("** \"typedetail\": mouse/nui/bi \n")
  cat("** \"typegeneric\": mouse/gesture \n")
  cat("*** Done! *** \n\n")
  
}