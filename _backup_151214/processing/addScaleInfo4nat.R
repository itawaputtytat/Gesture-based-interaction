addScaleInfo4nat <- function(data) {
  
  data$scale[data$variable %in% varnames4items[itemnr_nat_seamless]]  <- "seamless"
  data$scale[data$variable %in% varnames4items[itemnr_nat_learning]]  <- "learning"
  data$scale[data$variable %in% varnames4items[itemnr_nat_joy]] <- "joy"
  data$scale[data$variable %in% varnames4items[itemnr_nat_context]] <- "context"
  data$scale[data$variable %in% varnames4items[itemnr_nat_comminterface]] <- "comminterface"
  data$scale[data$variable %in% varnames4items[itemnr_nat_interpersonal]] <- "interpersonal"
  data$scale[data$variable %in% varnames4items[itemnr_nat_simplicity]] <- "simplicity"
  
  
  return(data)
}