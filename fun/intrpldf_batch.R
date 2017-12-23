intrpldf_batch <- function(dat,
                              col_name_ref,
                              col_name_group = "case",
                              stepsize = 0.1,
                              colnames2excl = NULL,
                              binary_vars = NULL,
                              outputFlag = F,
                              suffix = "intrpld",
                              replace_preceding = T) {
  
  outputFunProc(R)
  
  name4obj <- deparseDataFunArg(dat, return_dat = F)
  name4obj <- paste(name4obj, suffix, sep = "_")
  dat <- deparseDataFunArg(dat)
  
  col_name_ref_finder <- grep(col_name_ref, colnames(dat))
  col_name_ref <- colnames(dat)[col_name_ref_finder]
  dat_intrpld <- 
    invisible( lapply(unique(dat[, col_name_group]), function(p) {
      
      if (outputFlag)
        outputString(paste0("* Interpolating: ", p, " ... "), linebreak = F)
      
      ## Get data and run inteprolation
      dat_temp <- dat %>% filter_(paste0(col_name_group, "== \"", p, "\""))
      
      dat_intrpld <-
        intrpldf(dat_temp,
                 colname4ref = col_name_ref,
                 stepsize = stepsize,
                 colnames2excl = colnames2excl,
                 binary_vars = binary_vars,
                 replace_preceding = replace_preceding)
      
      if (outputFlag)
        outputDone(T)
      
      return(dat_intrpld)
      
    }) ## lapply
    ) %>% dplyr::bind_rows()
  
  assign(name4obj, dat_intrpld, env = .GlobalEnv)
  rm(dat_intrpld)
  outputString(paste("* New object:", name4obj))
  outputDone(T)
}