
# Import data -------------------------------------------------------------

dat_glances <- 
  read.table(file.path("analysis-study-4",
                       "process-videoannot-data-and-export-to-db",
                       "170125_elan-export_V2.txt"),
                       #"170126_elan-export_V3_only-tasks.txt"),
                       #"170126_elan-export_V3_only-tasks_V2.txt"),
             sep = "\t",
             header = T,
             stringsAsFactors = F)

## Rename columns
colnames_new <- c(
  "time_start_timestamp",
  "time_start_s",
  "time_start_ms",
  "time_end_timestamp",
  "time_end_s",
  "time_end_ms",
  "dur_timestamp",
  "dur_s",
  "dur_ms",
  "glance",
  "action",
  "interface_nr",
  "file",
  "filepath"
)
colnames(dat_glances) <- colnames_new



# Remove file "Vp428-1" ---------------------------------------------------

dat_glances <- 
  dat_glances %>% 
  filter(!grepl("Vp428-1", dat_glances$file))



# Reconstruct Vp428-1 -----------------------------------------------------

## Import data
dat_glances_vp481_1 <- 
  read.table(file.path("analysis-study-4",
                       "process-videoannot-data-and-export-to-db",
                       #"170125_elan-export_V2.txt"),
                       #"170126_elan-export_V3_only-tasks.txt"),
                       "Vp428-1_reconstructed_V3_ohne_ss.msec.csv"),
             sep = ";",
             header = T,
             stringsAsFactors = F)

## Rename columns
colnames_new_vp428_1 <- c(
  "time_start_timestamp",
  "time_end_timestamp",
  "dur_timestamp",
  "glance",
  "action",
  "interface_nr",
  "filepath"
)
colnames(dat_glances_vp481_1) <- colnames_new_vp428_1

## Convert timestamps to seconds
dat_glances_vp481_1$time_start_s <- 
  conv.timestamp2sec(dat_glances_vp481_1$time_start_timestamp)
dat_glances_vp481_1$time_end_s <-
  conv.timestamp2sec(dat_glances_vp481_1$time_end_timestamp)
dat_glances_vp481_1$dur_s <-
  conv.timestamp2sec(dat_glances_vp481_1$dur_timestamp)

## Compute ms
dat_glances_vp481_1$time_start_ms <- dat_glances_vp481_1$time_start_s * 1000
dat_glances_vp481_1$time_end_ms <- dat_glances_vp481_1$time_end_s * 1000
dat_glances_vp481_1$dur_ms <- dat_glances_vp481_1$dur_s * 1000

## Add file info
dat_glances_vp481_1$file <- "Vp428-1"

## Re-order columns
dat_glances_vp481_1 <- dat_glances_vp481_1[, colnames_new]



# Rbind data --------------------------------------------------------------

dat_glances_complete <- 
  rbind(dat_glances, dat_glances_vp481_1)



# Convert data ------------------------------------------------------------

#dat_glances$glance <- as.numeric(dat_glances$glance)
dat_glances_complete$action <- as.numeric(dat_glances_complete$action)



# Extract subject id ------------------------------------------------------

dat_glances_complete$id <- as.numeric( substr(dat_glances_complete$file, 4, 5) ) + 4000
dat_glances_complete$part_nr <- as.numeric(substr(dat_glances_complete$file, 7, 7) )
dat_glances_complete$info <- substr(dat_glances_complete$file, 6, 99)


# Export to database ------------------------------------------------------

dbWriteTable(dbconn_study4, "t_videoannot", dat_glances_complete, overwrite = T, row.names = F)
