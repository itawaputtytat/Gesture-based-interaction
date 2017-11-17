computeReliability <- function() {
  invisible( lapply(unique(get(set4proc$name4df)$table), function(t) {
    
    ## Get data
    outputSectionTitle(t)
    dat2proc <- get(set4proc$name4df) %>% filter(table == t)
    
    ## Identify scale names
    name4scales <- names(set4items[[set4proc$q]]$itemnrs4scales)
    
    if (mean(sapply((set4items[[set4proc$q]]$itemnrs4scales), length)) == 1) {
      colfinder <- set4items[[set4proc$q]]$varnames
      rel <- psych::alpha(dat2proc[, colfinder])
    } else {
      ## Compute reliability for each scale
      lapply(name4scales, function(n) {
        catWSepLine(n)
        itemnrs <- set4items[[set4proc$q]]$itemnrs4scales[[n]]
        colfinder <- set4items[[set4proc$q]]$varnames[itemnrs]
        #rel <- psych::alpha(dat2proc[, colfinder], check.keys = T)
        rel <- alpha_new(dat2proc[, colfinder])
        print(rel)
      })
    }
    cat("\n\n\n")
  }) )
}