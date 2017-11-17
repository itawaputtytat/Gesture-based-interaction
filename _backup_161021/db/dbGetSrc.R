dbGetSrc <- function(src) {

  dbquery$string <-
    paste("SELECT * FROM ", src, sep = "")

  dbGetQuery(dbconn, dbquery$string)
}
