dbListViews <- function(con, ...) {
  out <- dbGetQuery(con, "select viewname
                          from pg_views
                          where schemaname !='information_schema' and
                                schemaname !='pg_catalog'", ...)
  if (is.null(out) || nrow(out) == 0) out<-character(0)
  else out <- out[, 1]
  out
}
