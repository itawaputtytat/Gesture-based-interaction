dbRemoveTable <- function (con, name, ..., cascade = TRUE)
{
  if (dbExistsTable (con, name)) {
    dbGetQuery (con, paste ("drop table ", name, ifelse (cascade, "
                                                         cascade", ""), ";", sep=""))
  }
  }
