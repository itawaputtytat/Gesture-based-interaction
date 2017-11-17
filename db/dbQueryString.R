dbQueryString <- function(set4query, sxx, temp_dist1, temp_dist2) {

  query <- c()


  ## ----- SELECT -----

  query$select <- "SELECT "

  ## create string for each selected session related variable
  var_session <-
    lapply(set4query$var_session,
           function(v) paste(set4query$src, ".", v, sep = ""))

  ## create string for each selected situation related variable
  var_sxx <-
    lapply(set4query$var_sxx,
           function(v) paste(set4query$src, ".",
                             sprintf("s%02d", sxx), v, sep = ""))

  ## create string for each data related (measurements to analyse) variable
  var_data <-
    lapply(set4query$var_data,
           function(v) paste(set4query$src, ".", v, sep = ""))

  query$select <-
    paste("SELECT ",
          paste(c(var_session,
                  var_sxx,
                  var_data),
                collapse = ", "),
          sep = " ")


  ## ----- FROM -----

  query$from <- paste("FROM ", set4query$src, sep = "")

  #_query$join <- paste("LEFT JOIN testdaten ON ",
  #                     data2$src, ".vp = testdaten.vp", sep = "")


  ## ----- WHERE -----

  where_subid <-
    lapply(set4query$subid,
           function(w) paste(set4query$src, ".subid = ", w,
                             sep = ""))
  where_subid <-
    paste(where_subid, collapse = " OR ")

  where_round_txt <-
    lapply(set4query$round,
           function(w) paste(set4query$src, ".round_txt = '", w, "'",
                             sep = ""))
  where_round_txt <- paste(where_round_txt, collapse = " OR ")

#   if (grepl("ind", set4query$dist1)) {
#
#     dbGetSrc("sxx_critdist", "t_sxx_critdist")
#     rowfinder <- which(sxx_critdist$situation == sxx)
#     temp_dist1 <- sxx_critdist$final[rowfinder] * (-1)
#
#     cat("with individual distances \n")
#
#   } else
    # temp_dist1 <- set4query$dist1

  where_distance <-
    lapply(set4query$sxx,
           function(w) paste(set4query$src, ".",
                             sprintf("s%02d", sxx), "_",
                             set4query$distvar, " >= ",
                             temp_dist1, " AND ",
                             set4query$src, ".",
                             sprintf("s%02d", sxx), "_",
                             set4query$distvar, " <= ",
                             temp_dist2,
                             sep = ""))

  query$where <- paste("WHERE ",
                       paste("( ", c(where_subid,
                                     where_round_txt,
                                     where_distance), ")",
                             collapse = " AND ",
                             sep = ""))


  ## ----- GROUP BY -----

  query$group_by <- c()
  #e.g. dbquery$group <- paste("GROUP BY ",
  #                            set4query$table, ".vp, ",
  #                            "testdaten.runde",
  #                            sep="")


  ## ----- HAVING -----

  query$having <- c()


  ## ----- ORDER BY -----

  timevar <- which(grepl("time", set4query$var_session))
  timevar <- set4query$var_session[timevar]

  query$order <- paste("ORDER BY ",
                       set4query$src, ".subid",
                       ", ",
                       set4query$src, ".", timevar,
                       sep = "")


  ## ----- LIMIT -----

  query$limit <- c()


  ## ----- COMPLETE STRING -----

  dbquery$string <<- paste(query$select,
                           query$from, sep = " ")

  ## if exist
  if (is.null(query$join)  == F)
    dbquery$string <<- paste(dbquery$string, query$join)

  if (is.null(query$where) == F)
    dbquery$string <<- paste(dbquery$string, query$where)

  if (is.null(query$group) == F)
    dbquery$string <<- paste(dbquery$string, query$group)

  if (is.null(query$order) == F)
    dbquery$string <<- paste(dbquery$string, query$order)

}
