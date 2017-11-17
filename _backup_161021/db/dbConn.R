# Resources ---------------------------------------------------------------

library(RPostgreSQL)


# Database settings -------------------------------------------------------

dbset <- c()
dbset$dns  <- paste("BMW_Gestures_Study", studyselect, sep = "")
dbset$host <- "localhost"
dbset$port <- 5432
dbset$name <- dbset$dns
dbset$user <- "postgres"
dbset$pwd  <- "keines"
dbset$drv  <- dbDriver("PostgreSQL")


# Connect to database -----------------------------------------------------

dbConn <- function() {

  dbconn <<-
  dbConnect(dbset$drv,
            host    = dbset$host,
            port     = dbset$port,
            dbname   = dbset$name,
            user     = dbset$user,
            password = dbset$pwd)

}

dbConn()

#dbDisconnect(dbconn)
#dbUnloadDriver(dbset$drv)

cat("* Selected database: ", dbset$dns, "\n", sep = "")

## For correct import of strings
postgresqlpqExec(dbconn, "SET client_encoding = 'windows-1252'");
cat("* Reset encoding for import strings from db using: \n")
cat("** postgresqlpqExec(con, \"SET client_encoding = 'windows-1252'\"); \n")