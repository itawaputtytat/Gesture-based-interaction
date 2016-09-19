getRbindDataFromDB <- function (srcname, dataname) {
  
  cat("* Processing with function: getRbindDataFromDB \n")
  
  reset_name4data()
  
  dataname <- paste(srcname$prefix,
                    dataname,
                    sep = "_")
  
  assign("name4data",
         c(name4data,
           df = dataname),
         env = .GlobalEnv)
  
  ## Load tables created from suffix variable (= list of tables)
  data2process <- 
    lapply(srcname$suffix, function(x)
    dbGetSrc(paste(srcname$prefix, x, sep = "_")))
  
  ## Rbind list of tables
  ## ... and assign new column with table names for indicating origin
  ## ... amd assign new columng for experimental focus of interest (expfocus)
  data2process <- 
    cbind(do.call("rbind", data2process),
          table = rep(paste(srcname$prefix, srcname$suffix, sep = "_"), 
                      lapply(data2process, nrow)),
          expfocus = rep(srcname$suffix, 
                         lapply(data2process, nrow)))

  assign(dataname,
         data2process,
         env = .GlobalEnv)
  
  cat("** Created variable: ", dataname, "\n", sep = "")
  cat("*** Done! *** \n\n")
  
}