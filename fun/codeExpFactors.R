# Objective ---------------------------------------------------------------

# New variable for table: mouse vs. nui vs. bi

codeExpFactors <- function (name4df, colname4ref, studynr) {
  
  outputFunProc(R)
  
  dat2proc <- get(name4df) %>% select_(ref = colname4ref) 
  
  if (studynr == 1) {
    
    dat2proc$itype_generic[grepl("mouse", dat2proc$ref)] <- "mouse"
    dat2proc$itype_generic[grepl("nui", dat2proc$ref) | 
                             grepl("bi", dat2proc$ref)] <- "gesture"
    
    dat2proc$itype_detail[grepl("mouse", dat2proc$ref)] <- "mouse"
    dat2proc$itype_detail[grepl("nui", dat2proc$ref)]   <- "nui"
    dat2proc$itype_detail[grepl("bi", dat2proc$ref)]    <- "bi"
    
    dat2proc <- cbind(get(name4df),
                      itype_generic = dat2proc$itype_generic,
                      itype_detail = dat2proc$itype_detail,
                      stringsAsFactors = F)
  }
  
  if (studynr == 2) {
    
    dat2proc$itype_generic[grepl("gestures", dat2proc$ref)] <- "gestures"
    dat2proc$itype_generic[grepl("touch", dat2proc$ref)]    <- "touch"
    
    if (!set4proc$q %in% c("attrakdiff", "eval")) {
      
      dat2proc$scenario[grepl("city", dat2proc$ref)] <- "city"
      dat2proc$scenario[grepl("motorway", dat2proc$ref)] <- "motorway"
      
      dat2proc$scenario_itype <- c()
      dat2proc$scenario_itype[which(grepl("city", t_q_acc$table) & grepl("gestures", t_q_acc$table))] <- "city_gestures"
      dat2proc$scenario_itype[which(grepl("city", t_q_acc$table) & grepl("touch", t_q_acc$table))] <- "city_touch"
      dat2proc$scenario_itype[which(grepl("motorway", t_q_acc$table) & grepl("gestures", t_q_acc$table))] <- "motorway_gestures"
      dat2proc$scenario_itype[which(grepl("motorway", t_q_acc$table) & grepl("touch", t_q_acc$table))] <- "motorway_touch"
      
      dat2proc$scenario_ilevel <- c()
      dat2proc$scenario_ilevel[which(grepl("city", t_q_acc$table) & grepl("simple", t_q_acc$table))] <- "city_simple"
      dat2proc$scenario_ilevel[which(grepl("city", t_q_acc$table) & grepl("complex", t_q_acc$table))] <- "city_complex"
      dat2proc$scenario_ilevel[which(grepl("motorway", t_q_acc$table) & grepl("simple", t_q_acc$table))] <- "motorway_simple"
      dat2proc$scenario_ilevel[which(grepl("motorway", t_q_acc$table) & grepl("complex", t_q_acc$table))] <- "motorway_complex"
       
      dat2proc$ilevel[grepl("simple", dat2proc$ref)]  <- "simple"
      dat2proc$ilevel[grepl("complex", dat2proc$ref)] <- "complex"
      dat2proc$ilevel[grepl("simple", dat2proc$ref)]  <- "simple"
      dat2proc$ilevel[grepl("complex", dat2proc$ref)] <- "complex"
      
      dat2proc$itype_ilevel[grepl("gestures_simple", dat2proc$ref)]  <- "gestures_simple"
      dat2proc$itype_ilevel[grepl("gestures_complex", dat2proc$ref)] <- "gestures_complex"
      dat2proc$itype_ilevel[grepl("touch_simple", dat2proc$ref)]     <- "touch_simple"
      dat2proc$itype_ilevel[grepl("touch_complex", dat2proc$ref)]    <- "touch_complex"
      
      dat2proc <- cbind(get(name4df),
                        scenario = dat2proc$scenario,
                        scenario_itype = dat2proc$scenario_itype,
                        scenario_ilevel = dat2proc$scenario_ilevel,
                        itype_generic = dat2proc$itype_generic,
                        ilevel = dat2proc$ilevel,
                        itype_ilevel = dat2proc$itype_ilevel,
                        stringsAsFactors = F)
    } else {
      dat2proc <- cbind(get(name4df),
                        itype_generic = dat2proc$itype_generic,
                        stringsAsFactors = F)
    }
    
  }

  assign(name4df, dat2proc, env = .GlobalEnv)
  outputDone()
}