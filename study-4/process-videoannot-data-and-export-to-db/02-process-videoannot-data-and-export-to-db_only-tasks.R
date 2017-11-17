
# Import data -------------------------------------------------------------

dat_videoannot_actions <- 
  read.table(file.path("analysis-study-4",
                       "process-videoannot-data-and-export-to-db",
                       #"170125_elan-export_V2.txt"),
             #"170126_elan-export_V3_only-tasks.txt"),
             #"170126_elan-export_V3_only-tasks_V2.txt"),
             "170127_elan-export_V3_only-tasks_V3.txt"),
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
  #"glance",
  "action",
  "interface_nr",
  "file",
  "filepath"
)
colnames(dat_videoannot_actions) <- colnames_new



# Remove file "Vp428-1" ---------------------------------------------------

dat_videoannot_actions <- 
  dat_videoannot_actions %>% 
  filter(!grepl("Vp428-1", dat_videoannot_actions$file))



# Reconstruct Vp428-1 from data -------------------------------------------

## Import data
dat_videoannot_actions_vp481_1 <- 
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
colnames(dat_videoannot_actions_vp481_1) <- colnames_new_vp428_1


## Convert timestamps to seconds
dat_videoannot_actions_vp481_1$time_start_s <- 
  conv.timestamp2sec(dat_videoannot_actions_vp481_1$time_start_timestamp)
dat_videoannot_actions_vp481_1$time_end_s <-
  conv.timestamp2sec(dat_videoannot_actions_vp481_1$time_end_timestamp)
dat_videoannot_actions_vp481_1$dur_s <-
  conv.timestamp2sec(dat_videoannot_actions_vp481_1$dur_timestamp)


## Compute ms
dat_videoannot_actions_vp481_1$time_start_ms <- dat_videoannot_actions_vp481_1$time_start_s * 1000
dat_videoannot_actions_vp481_1$time_end_ms <- dat_videoannot_actions_vp481_1$time_end_s * 1000
dat_videoannot_actions_vp481_1$dur_ms <- dat_videoannot_actions_vp481_1$dur_s * 1000


## Add file info
dat_videoannot_actions_vp481_1$file <- "Vp428-1"


## Re-order columns
dat_videoannot_actions_vp481_1 <- dat_videoannot_actions_vp481_1[, c(colnames_new, "glance")]


dat_videoannot_actions_vp481_1.v2 <- 
  dat_videoannot_actions_vp481_1 %>% 
  # select(-filepath, 
  #        -time_start_timestamp, -time_start_ms, 
  #        -time_end_timestamp, -time_end_ms,
  #        -dur_timestamp, -dur_ms) %>% 
  filter(is.na(glance)) %>% 
  #arrange(interface_nr) %>% 
  #group_by(interface_nr) %>% 
  #mutate(time_diff_s.to_prev = time_start_s - lag(time_end_s, default = first(time_start_s))) %>% 
  #mutate(seqhelp = ifelse(time_diff_s.to_prev <= 5, 0, 1)) %>% 
  #mutate(time_end_s.v2 = ifelse(time_diff_s.to_prev <= 5, time_end_s + time_diff_s.to_prev, time_end_s)) %>% 
  data.frame()


## Remove glance column
dat_videoannot_actions_vp481_1.v2$glance <- NULL


# ## Remove interface.nr 6
# dat_videoannot_actions_vp481_1.v2 <- 
#   dat_videoannot_actions_vp481_1.v2 %>% 
#   filter(interface_nr != 6)


# 
# ## Correct data (due to glance data before)
# dat_videoannot_actions_vp481_1.v2 <-
#   dat_videoannot_actions_vp481_1.v2 %>%
#   arrange(interface_nr, desc) %>%
#   mutate(time_end_s.cummax)
#   mutate(time_start_s.lead = lead(time_start_s),
#          time_end_s.lead = lead(time_end_s)) %>%
#   mutate(time_end_s.v2 = ifelse(time_start_s.lead <= time_end_s & time_end_s.lead > time_end_s,
#                                 time_end_s.lead,
#                                 time_end_s))
# 
# 
# 
# 
# 
# ## Aggregate data
# dat_videoannot_actions_vp481_1.aggr <-
#   dat_videoannot_actions_vp481_1_v2 %>%
#   group_by(interface_nr, action) %>%
# 
#   summarise(naction = distinct(time_end_s.v2, na.rm = T),
#             time_start_timestamp = min(time_start_timestamp),
#             time_start_s = min(time_start_s),
#             time_start_ms = min(time_start_ms),
#             time_end_timestamp = max(time_end_timestamp),
#             time_end_s = max(time_end_s),
#             time_end_ms = max(time_end_ms),
#             dur_timestamp = max(dur_timestamp),
#             dur_ms = max(dur_ms),
#             #action = max(action),
#             file = max(file),
#             filepath = max(filepath)) %>%
#   arrange(interface_nr) %>%
#   data.frame()
# 
# 
# dat_videoannot_actions_vp481_1_v2 %>%
#   select(-filepath,
#          -time_start_timestamp, -time_start_ms,
#          -time_end_timestamp, -time_end_ms,
#          -dur_timestamp, -dur_ms) %>%
#   arrange(interface_nr)



# Rbind data --------------------------------------------------------------

dat_videoannot_actions_complete <- 
  rbind(dat_videoannot_actions, 
        dat_videoannot_actions_vp481_1.v2)



# Adjust data -------------------------------------------------------------

dat_videoannot_actions_complete <- 
  dat_videoannot_actions_complete %>% 
  mutate(action = as.numeric(action),
         id = as.numeric(substr(file, 4, 5) ) + 4000,
         part_nr = as.numeric(substr(file, 7, 7) ),
         info = substr(file, 6, 99))
         
         

# Export to database ------------------------------------------------------

dbWriteTable(dbconn_study4, "t_videoannot_actions", dat_videoannot_actions_complete, overwrite = T, row.names = F)
