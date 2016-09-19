buildOverallScore4AttrakDiff <- function (dataname) {
  
  cat("* Processing with function: buildOVerallScore4AttrakDiff \n")
  
  assign(dataname, 
         cbind(get(dataname),
               score = rowSums(get(name4data$df)[, 2:(length(itemsets[[name4dbsrc$q]])+1)]),
               score_pq  = rowMeans(get(name4data$df)[, itemnr_attrakdiff_pq  + 1]),
               score_hqi = rowMeans(get(name4data$df)[, itemnr_attrakdiff_hqi + 1]),
               score_hqs = rowMeans(get(name4data$df)[, itemnr_attrakdiff_hqs + 1]),
               score_hq  = rowMeans(get(name4data$df)[, itemnr_attrakdiff_hq  + 1]),
               score_att = rowMeans(get(name4data$df)[, itemnr_attrakdiff_att + 1])
         ), #cbind
         env = .GlobalEnv) #assign

  cat("** Added rowMeans to: ", dataname, "\n", sep = "")
  cat("*** Done! *** \n\n")
  
}