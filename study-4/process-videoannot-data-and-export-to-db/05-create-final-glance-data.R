
# Prepare data ------------------------------------------------------------

## Load glance times
dat_glancetimes <- dbGetSrc("dbconn_study4", "t_videoannot_glance_times")

## Load action times
dat_actiontimes <- dbGetSrc("dbconn_study4", "t_videoannot_actions_times")


## Join data
## Each glance sequence will be assigned to each possible action tasks
## ... meaning that there will be glance duplicate as it is still not clear ...
## ... which task might be the right one (depends on there start and end time)
## (Will be corrected in the step)
dat_joined <- 
  left_join(dat_actiontimes,
            dat_glancetimes,
            by = c("id", "interface_nr")) %>% 
  data.frame()


## Select and rename columns for clean dataset
dat_joined <- 
  dat_joined %>% 
  filter(!is.na(glance_seqid)) %>% 
  select(id,
         part_nr = part_nr.x,
         
         interface_nr, 
         time_start_s.v2.min_interface_nr,
         time_end_s.v2.max_interface_nr,
         
         task_nr,
         time_start_s.action = time_start_s.x,
         time_end_s.action = time_end_s.x,
         dur_s.action = dur_s.x,
         time_start_s.v2.action = time_start_s.v2.x,
         time_end_s.v3.action = time_end_s.v3.x,
         
         #glance,
         glance_seqid,
         time_start_s.glance = time_start_s.y,
         time_end_s.glance = time_end_s.y,
         dur_s.glance = dur_s.y,
         time_start_s.v2.glance = time_start_s.v2.y,
         time_end_s.v3.glance = time_end_s.v3.y
  )



# Assign task numbers to glances corresponding to their time --------------

## Find minimum time between task ending and next task starting time
dat_actiontimes.diff <- 
  dat_actiontimes %>% 
  arrange(id, interface_nr, time_start_s.v2) %>% 
  group_by(id, interface_nr) %>% 
  mutate(time_diff_s.action = time_start_s.v2 - lag(time_end_s.v3)) %>% 
  #ungroup() %>% 
  summarise(time_diff_s.action.min = min(time_diff_s.action, na.rm = T))


dat_joined.task_nr <- 
  left_join(dat_joined,
            dat_actiontimes.diff)
  

dat_joined.task_nr <- 
  dat_joined.task_nr %>% 
  
  ## Do only for exploring ELAN data
  mutate(timestamp_start = conv.sec2timestamp(time_start_s.glance)) %>% 

  group_by(id, interface_nr, task_nr) %>% 
  mutate(task_nr.glance = 
           as.numeric(ifelse(time_start_s.v2.glance >= (time_start_s.v2.action - 1) &
                               time_end_s.v3.glance <= (time_end_s.v3.action + 2),
                  #task_nr,
                  1,
                  NA))) %>% 
## Additional filter for glances, which follows action times ...
  ## But may not be interesting to analyse
  mutate(task_nr.glance.v2 = 
           as.numeric(ifelse(is.na(task_nr.glance) &
                               time_start_s.v2.glance >= (time_start_s.v2.action - 1) &
                               time_end_s.v3.glance >= (time_end_s.v3.action + 1) &
                               time_end_s.v3.glance <= (time_end_s.v3.action + 10),
                             #task_nr * 10 + 9,
                             1,
                             task_nr.glance))) %>%
  #group_by(id, interface_nr) %>% 
  ## In order to catch every glance
  mutate(task_nr.glance.all = 
           as.numeric(ifelse(is.na(task_nr.glance.v2) & 
                               time_start_s.v2.glance >= (time_start_s.v2.action - 10) &
                               time_start_s.v2.glance <= (time_end_s.v3.action + 5) &
                               time_end_s.v3.glance <= (time_end_s.v3.action + time_diff_s.action.min - 10),
                             #task_nr * 100 + 99,
                             1,
                             task_nr.glance.v2)))



## Results will be NA column values for cases with no time-corresponding ...
## ... task numbers, which can be excluded
dat_joined.task_nr.aggr <- 
  dat_joined.task_nr %>% 
  #filter(!is.na(task_nr.glance.all)) %>% 
  filter(task_nr.glance.all != 0) %>% 
  arrange(id, interface_nr, task_nr) %>% 
  group_by(id, interface_nr, task_nr.glance) %>% 
  mutate(glance_nr = row_number()) %>% 
  data.frame()


## Check if all glances have been included
# test <- 
#   dat_joined.task_nr %>% 
#   group_by(glance_seqid) %>% 
#   summarise(id = min(id),
#             interface_nr = min(interface_nr))
# 
# test$indata <- 0
# rowfinder <- 
#   which(test$glance_seqid %in% 
#           unique(dat_joined.task_nr.aggr$glance_seqid))
# test$indata[rowfinder] <- 1



# Sort columns ------------------------------------------------------------

dat_joined.task_nr.aggr <- 
  dat_joined.task_nr.aggr %>% 
  select(id, part_nr, interface_nr,
         time_start_s.v2.min_interface_nr, time_end_s.v2.max_interface_nr,
         task_nr,
         time_start_s.action, time_end_s.action, dur_s.action,
         time_start_s.v2.action, time_end_s.v3.action,
         glance_seqid,
         task_nr.glance,
         task_nr.glance.v2,
         task_nr.glance.all,
         time_start_s.glance, time_end_s.glance, dur_s.glance,
         time_start_s.v2.glance, time_end_s.v3.glance) %>% 
  group_by(id, interface_nr, task_nr) %>% 
  mutate(glances_n = sum(task_nr.glance, na.rm = T),
         glances_n.v2 = sum(task_nr.glance.v2, na.rm = T),
         glances_n.all = sum(task_nr.glance.all, na.rm = T))




# Add procedure data ------------------------------------------------------

## Load data
dat_proc <- dbGetSrc("dbconn_study4", "t_procedure")

## Aggregate
dat_proc.aggr <- 
  dat_proc %>% 
  filter(block > 0 & block < 9 & block_task != 1) %>% 
  group_by(id, interface_nr = block, block_task) %>% 
  summarise(condition = max(condition),
            task = max(task)) %>% 
  ungroup() %>% 
  mutate(id = substr(as.character(id), 2, 3)) %>% 
  mutate(id = as.numeric(id) + 4000,
         task_nr = block_task - 1)

## Add information
dat_proc.aggr <- 
  dat_proc.aggr %>% 
  mutate(itype = ifelse(grepl("g", condition), "gesture", "touch")) %>% 
  mutate(ilevel = ifelse(grepl("s", condition), "simple", "complex")) %>% 
  mutate(scenario = ifelse(grepl("s", condition), "city", "motorway")) 

## Join data
dat_joined.task_nr.aggr.proc <- 
  left_join(dat_proc.aggr, 
            dat_joined.task_nr.aggr)



# Filter (Lisa) -----------------------------------------------------------

# Vp402 - Fahrt 8, Aufgabe 3
# Vp409 - Fahrt 1, Aufgabe 1
# Vp410 - Fahrt 3, Aufgabe 1
# Vp412 - Fahrt 4, Aufgabe 1
# Vp421 - Fahart 5, Aufgabe 2
# Vp433 - Fahrt 6, Aufgabe 2
# Vp434 - Fahrt 6, Aufgabe 2
# Vp436 - Fahrt 4, Aufgabe 1
# Vp438 - Fahrt 1

filterlist <- list(
  c(4002, 8, 3),
  c(4009, 1, 1),
  c(4010, 3, 1),
  c(4012, 4, 1),
  c(4021, 5, 2),
  c(4033, 6, 2),
  c(4034, 6, 2),
  c(4036, 4, 1),
  c(4038, 1, 1),
  c(4038, 1, 2),
  c(4038, 1, 3))

dat_joined.task_nr.aggr.proc.filtered <- 
  dat_joined.task_nr.aggr.proc %>% 
  filter(!is.na(glance_seqid))

invisible(
  lapply(filterlist, function(x) {
    print(x)
    rowfinder <- 
      which(dat_joined.task_nr.aggr.proc.filtered$id == x[1] & 
              dat_joined.task_nr.aggr.proc.filtered$interface_nr == x[2] &
              dat_joined.task_nr.aggr.proc.filtered$task_nr == x[3])
    
    print(rowfinder)
    #print(length(rowfinder))
    #print(test6[rowfinder, c("id", "interface_nr", "condition", "task_nr", "dur_s.glance")] %>% data.frame())
    if (length(rowfinder) > 0)
      dat_joined.task_nr.aggr.proc.filtered <<- 
        dat_joined.task_nr.aggr.proc.filtered[-rowfinder, ]
    #return(test6[-rowfinder, ])
  })
)


# Export to database ------------------------------------------------------

dbWriteTable(dbconn_study4,
             "t_videoannot_glances_final",
             dat_joined.task_nr.aggr.proc.filtered,
             overwrite = T,
             row.names = F)




