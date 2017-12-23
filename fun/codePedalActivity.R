## Code pedalactitivy
##  1 = acceleration pedal
##  0 = no pedal actitivy
## -1 = braking pedal

codePedalActivity <- function(dat,
                              colname_acc_pedal_pos = "acc_pedal_pos_perc_corr",
                              colname_brake_status = "brake_status",
                              colname_brake_press = "brake_press_bar",
                              colname_group = "passing",
                              brake_press_bar_treshold = 60) {
  
  outputFunProc(R)
  ptm <- proc.time()
  
  name4obj <- paste(deparse(substitute(dat)))
  outputString(paste("* Current processing:", name4obj, "... "), linebreak = F)
  
  ## Get data
  #dat <- get(dfname)
  
  ## Initialise pedal activity
  dat$pedal_act <- 0
  
  ## Code pedal activity as -1 (for braking) when brake_press_status == 1
  ## No pedal activity will result in: 0 * -1
  dat[, colname_brake_status] <- 
    vapply(dat[, colname_brake_status], 
           function(x) ifelse(x, 1, 0), FUN.VALUE = numeric(1))
  dat$pedal_act <- dat[, colname_brake_status] * -1
  
  ## Code pedal activity as 1 for accelerating when:
  ## ... pedal_act has not been coded as -1 (for braking) before
  rowfinder <-
    which(dat$pedal_act != -1 & dat[, colname_acc_pedal_pos] > 0)
  dat$pedal_act[rowfinder] <- 1
  
  ## Initialise pedal intensity
  dat$pedal_int_perc <- 0
  
  ## Code pedal intensity for braking as percentage to 60 bar
  dat$pedal_int_perc[dat$pedal_act == -1] <-
    100/brake_press_bar_treshold * dat[dat$pedal_act == -1, colname_brake_press]
  
  ## Code pedal intensity for accelerating (already in percentage)
  dat$pedal_int_perc[dat$pedal_act == 1] <-
    dat[dat$pedal_act == 1, colname_acc_pedal_pos]
  
  ## Code pedal intensity for no pedal activity as zero
  ## ... not really necessary
  dat$pedal_int_perc[dat$pedal_act == 0] <- 0
  
  ## Code every pedal intensity below zero as zero
  ## Reason: Pedal position for accelerating pedal might be below zero
  dat$pedal_int_perc[dat$pedal_int_perc < 0] <- 0
  
  ## Create sequence ids for each pedal activity
  dat <- 
    dat %>% 
    mutate(row_nr = row_number()) %>% 
    group_by(pedal_act) %>% 
    mutate(pedal_act_id = row_nr - row_number()) %>% 
    mutate(row_nr = NULL) %>% 
    data.frame()

  ## Save data
  assign(name4obj, dat, envir = .GlobalEnv)
  
  outputDone(T)
  outputString("** New columns: pedal_act, pedal_act_id, pedal_int_perc")    
  outputProcTime(ptm)
}
