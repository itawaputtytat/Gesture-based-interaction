
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$dir_path <- file.path("E:", "_DATEN_ADTF")
sett_dat$file_name_pattern <- ".csv"
sett_dat$db_conn_name <- dbFindConnObj("Study-5")
sett_dat$src_name <- "t_adtf_raw"




# Obtain list of subdirectories -------------------------------------------

dat_subdirectories <- 
  list.dirs(path = sett_dat$dir_path, 
             recursive = T)

dat_subdirectories <- dat_subdirectories[-1]



# Load data and import in database ----------------------------------------

ptm <- proc.time()

i = 0
for(subdirectory in dat_subdirectories) {

  ## Obtaint list of csv files
  dat_file_names_temp <- 
    list.files(subdirectory, 
               sett_dat$file_name_pattern)
  
  for(file_name in dat_file_names_temp) {
    
    outputString(paste("* Processing:", file_name))

    file_path_temp <- file.path(subdirectory, file_name)
    
    ## Check for length
    check_length <- length(readLines(file_path_temp))
    
    if (check_length != 0) {
      
      dat_temp <- read.csv(file_path_temp, header = T, skip = 13, sep = ",")
      dat_temp$file_name <- file_name
      
      ## Row numbers will now be computed within database
      # ## Order by time and create row number per file
      # dat_temp <- 
      #   dat_temp %>% 
      #   arrange(Time) %>% 
      #   mutate(row_nr = row_number())
      
      dbWriteTable(get(sett_dat$db_conn_name),
                   sett_dat$src_name,
                   dat_temp,
                   append = T,
                   row.names = F)
      
    }
    

    
  }
  
  
  # i = i + 1
  # if (i == 3) break

}

outputProcTime(ptm)