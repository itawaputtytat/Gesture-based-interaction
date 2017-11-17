revLevels <- function(dat2proc, varname) {
  
  dat2proc[, varname] <- as.character(dat2proc[, varname])
  dat2proc[, varname] <- 
     with(dat2proc, 
          reorder(get(varname), 
                  get(varname), function(x) -length(x)))
  
  #levels(dat2proc[, varname]) <- rev(levels(dat2proc[, varname]))
  return(dat2proc)
}