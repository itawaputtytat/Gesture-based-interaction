buildOverallScore <- function (dataname) {
   
  cat("* Processing with function: buildOVerallScore \n")
  
  assign(dataname, 
         get(dataname) %>% 
           mutate(score = rowSums(.[itemsets[[name4dbsrc$q]]])),
         env = .GlobalEnv) 
  
  cat("** Added rowSums to: ", dataname, "\n", sep = "")
  
}