
# Objective ---------------------------------------------------------------

## Batch processing relevant STISIM files



# Preparatory settings ----------------------------------------------------

## Initialise (import-data.R works with "dat2import")
dat2import <- c()
dat2import$filelist <- 
  list.files("analysis-study-4/export-stisim-data-to-db/data_stisim/")

## Remove practice trials for 402
finder <- which(grepl("Übung", dat2import$filelist))
dat2import$filelist <- dat2import$filelist[-finder]

## Info:
## filelist will be in alphabetical order
## As the run number of trials will not be considered
## ... the data with highest run number will overwrite previous data
## ... with same name



# Batch process -----------------------------------------------------------

## Start timer
ptm <- proc.time()

## Initialise protocol
sink("analysis_study4/stisim-data-to-db/import_log.txt", append = T)

for(fname in dat2import$filelist) {

  dat2import$filename <- fname
  cat("\n")
  cat(fname, "\n")
  
  ## Run import script
  source("analysis_study4/stisim-data-to-db/import-data.R")
  
  ## Create object name for export
  objname4exp <- c()
  
  ## Subject
  objname4exp$subject <- meta$id
  
  ## Scenario
  if (grepl("adt", dat2import$filename)) 
    objname4exp$scenario <- "city"
  if (grepl("utob", dat2import$filename)) 
    objname4exp$scenario <- "motorway"
  
  ## Type
  if (grepl("est",  dat2import$filename)) 
    objname4exp$type = "gesture"
  if (grepl("ouch",  dat2import$filename)) 
    objname4exp$type = "touch"
  if (grepl("line1",  dat2import$filename)) 
    objname4exp$type = "baseline1"
  if (grepl("line2",  dat2import$filename)) 
    objname4exp$type = "baseline2"
  
  ## Type: Special case for 401
  if (grepl("line_2",  dat2import$filename)) 
    objname4exp$type = "baseline1"
  if (grepl("line_3",  dat2import$filename)) # "line_2" must be first trial
    objname4exp$type = "baseline1"
  if (grepl("line_4",  dat2import$filename)) 
    objname4exp$type = "baseline1"
  if (grepl("line 2",  dat2import$filename)) 
    objname4exp$type = "baseline2"
  
  ## Type: Special case for 402
  if (grepl("ohne_",  dat2import$filename)) 
    objname4exp$type = "baseline1"
  if (grepl("ohne_2_",  dat2import$filename)) 
    objname4exp$type = "baseline2"
  
  ## Depth
  if (grepl("inf",  dat2import$filename)) 
    objname4exp$depth = "simple"
  if (grepl("ompl",  dat2import$filename)) 
    objname4exp$depth = "complex"
  
  objname4exp$finalname <- 
    paste("t_stisim_",
          paste(unlist(objname4exp), collapse = "_"),
          sep = "")
  cat(objname4exp$finalname, "\n")
  
  ## Export to data base
  dbWriteTable(dbconn, objname4exp$final, dat_df, overwrite = T, row.names = F)
  
}

## End of protocoll
sink()
unlink("analysis_study4/stisim-data-to-db/import_log.txt")

## Stop and output timer
stoptime <- proc.time() - ptm
cat("Elapsed:", stoptime[3], "\n")