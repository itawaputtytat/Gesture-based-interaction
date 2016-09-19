revLevels <- function(data, varname) {
  
  data[, varname] <- as.character(data[, varname])
  #data <- arrange(data, desc(variable))
  
   data[, varname] <- 
     with(data, reorder(get(varname), get(varname), function(x) -length(x)))
  
  #levels(data[, varname]) <- 
   # rev(levels(data[, varname]))
  
  return(data)
  
}