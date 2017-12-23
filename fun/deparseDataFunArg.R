deparseDataFunArg <- function(arg, return_dat = T) {
  
  ## If argument is string
  if (is.character(arg)) {
    
    arg_string <- arg 
    
  } else {
    
    ## Deparse argument as string
    arg_string <- deparse(substitute(arg))
    
  }
  
  if (!return_dat) {
    
    arg_string <- paste0("deparse(substitute(", arg_string, "))")
    arg_string <- eval(parse(text = arg_string), env = parent.frame())
    return(arg_string)
    
  } else {
    
    ## If string does not contain "get" add get
    if (grepl("get", arg_string)) {
      
      arg_string_get <- arg_string
      
    } else {
      
      arg_string_get <- paste0("get(\"", arg_string, "\")")
      
    }
    
    dat <- eval(parse(text = arg_string_get), env = parent.frame())
    return(dat)
    
  }
}

# testFun <- function(dat_new, ...) {
# 
#   dat <- deparseDataFunArg(dat_new, ...)
#   return(dat)
# 
# }
# 
# testdat <- head(study1_t_adtf_pxx_full_dti_rnd1)
# testdat <- study1_t_adtf_pxx_full_dti_rnd1
# testFun(testdat, return_dat = F)
