dbQueryRun <- function(save2df, querystring) {
  assign(save2df,
         dbGetQuery(dbconn, querystring),
         envir = .GlobalEnv)
}
