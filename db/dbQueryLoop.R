
# Objective ---------------------------------------------------------------

## load data from database by looping through selected situations
## for each situation a dataframe gets creates


# Query loop --------------------------------------------------------------

dbQueryLoop <- function(set4query) {

  ptm <- proc.time()

  ## initialiae query string
  dbquery <<- c()

  cat("Query data from database... \n\n")

  for (sxx in set4query$sxx) {

    ## set name for dataframe
    dbquery$save2df <<- paste(sprintf("s%02d", sxx),
                               set4query$distvar,
                               sep = "_")

    if (is.null(set4query$save2df_prefix) == F)
      dbquery$save2df <<- paste(set4query$save2df_prefix,
                                 dbquery$save2df,
                                 sep = "_")

    ## print current query process
    cat(paste("into dataframe: ", dbquery$save2df, "\n", sep = ""))
    if (grepl("ind", set4query$dist1)) {

      remember_ind = T

      assign("sxx_critdist",
             dbGetSrc("t_sxx_critdist"),
             env = .GlobalEnv)
      rowfinder <- which(sxx_critdist$situation == sxx)
      set4query$dist1 <- sxx_critdist$final[rowfinder] * (-1)

      cat("with individual distances \n")

    } else {
      cat("with standard distances \n")
      remember_ind = F
    }

    cat(paste(" from ", set4query$distvar, " = ", set4query$dist1, "\n",
              " to   ",   set4query$distvar, " = ", set4query$dist2, "\n",
              sep = ""))
    cat("\n\n")
    cat("internal adjusted to x-, 50 units/x+50 units, \n")
    cat("...for avoiding spline interpolation of starting/ending values \n")

    ## Adjust distance data for querying (see last)
    set4query$dist1_backup <- set4query$dist1
    temp_dist1 <- set4query$dist1 -50

    set4query$dist2_backup <- set4query$dist2
    temp_dist2 <- set4query$dist2 +50

    ## create string for query
    dbQueryString(set4query, sxx, temp_dist1, temp_dist2)

    ## run query
    assign(dbquery$save2df,
           dbGetQuery(dbconn, dbquery$string),
           envir = .GlobalEnv)

    ## It makes no sense to readjust these data again to old values (no win)

#     ## Adjust distance to values from setting
#     ## ... remove adjustment for querying, see above
#
#     varname4dist <-  paste(sprintf("s%02d", sxx),
#                            set4query$distvar,
#                            sep = "_")
#
    set4query$dist1 <<- set4query$dist1_backup
    set4query$dist2 <<- set4query$dist2_backup
#
#     data2process <- get(dbquery$save2df)
#     data2process <-
#       data2process[, which(data2process[, varname4dist] >= set4query$dist1 &
#                              data2process[, varname4dist] <= set4query$dist2)]
#
#     assign(dbquery$save2df,
#            data2process,
#            envir = .GlobalEnv)

    ## reset to "ind" (when necessary)
    if (remember_ind) set4query$dist1 <- "ind"

    cat("\n")

  } # situation

  cat("\n")
  cat("Done!")
  cat("\n\n")
  cat("Overall time:", (proc.time() - ptm)[3], "s" )
  cat("\n\n")
}
