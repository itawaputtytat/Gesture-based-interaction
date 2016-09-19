# Objective ---------------------------------------------------------------

# New variable for table: mouse vs. nui vs. bi

codeInteractionType <- function (dataname) {
  
   cat("* Processing with function: codeInteractionType \n")
  
  data2recode <- get(dataname) %>% select(table)
  
  ## Study 1 ----
  data2recode$typegeneric[grepl("mouse", data2recode$table)] <- "mouse"
  data2recode$typegeneric[grepl("nui", data2recode$table) |
                            grepl("bi", data2recode$table)] <- "gesture"
  
  data2recode$typedetail[grepl("mouse", data2recode$table)] <- "mouse"
  data2recode$typedetail[grepl("nui", data2recode$table)]   <- "nui"
  data2recode$typedetail[grepl("bi", data2recode$table)]    <- "bi"
  
  ## Study 2 ----
  data2recode$typegeneric[grepl("gestures", data2recode$table)] <- "gestures"
  data2recode$typegeneric[grepl("touch", data2recode$table)] <- "touch"
  
  data2recode$typedetail[grepl("gestures_simple", data2recode$table)] <- "gestures_simple"
  data2recode$typedetail[grepl("gestures_complex", data2recode$table)] <- "gestures_complex"
  data2recode$typedetail[grepl("touch_simple", data2recode$table)] <- "touch_simple"
  data2recode$typedetail[grepl("touch_complex", data2recode$table)] <- "touch_complex"
  
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