set4db <- c()
set4db$dns <- "BMW_Gestures_Study"
set4db$host <- "localhost"
set4db$port <- 5432
set4db$name <- set4db$dns
set4db$user <- "postgres"
set4db$pwd  <- "WRITE-PASSWORD-HERE"
set4db$drv  <- dbDriver("PostgreSQL")
set4db$select <- c("1-3", "4")
