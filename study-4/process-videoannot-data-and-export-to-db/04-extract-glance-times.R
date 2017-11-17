
# Load data ---------------------------------------------------------------

## Load data
dat_glances <- dbGetSrc("dbconn_study4", "t_videoannot")



# Adjust data -------------------------------------------------------------

## Create consistent part number (in case there is no number)
## Remove action column
## Filter for glance == 1 (due to "empty" glance values by action annotation)
dat_glances <-
  dat_glances %>% 
  ## Sort data
  arrange(id, part_nr, time_start_s) %>% 
  ## Correct part number
  mutate(part_nr = ifelse(is.na(part_nr), 1, part_nr)) %>% 
  ## Remove column for actions
  mutate(action = NULL) %>% 
  ## Filter for glances
  filter(glance == 1) %>% 
  ## Create glance sequence ids
  mutate(glance_seqid = row_number())



# Case exlusion #1 --------------------------------------------------------

## Complete 4005
## 4021: Only "erste Fahrt"
dat_glances <- 
  dat_glances %>% 
  filter(id != 4005) %>% 
  ## These following filters absolutely make sense
  ## ... as the "erste Fahrt" contains all relevant information
  ## "zweite Fahrt" contains only information beginning at interface-nr. 4
  filter(!grepl("Vp421-1", file)) %>% 
  #filter(!grepl("Vp421-erste Fahrt", file))
  filter(!grepl("Vp421-zweite Fahrt", file)) 



# Part problem ------------------------------------------------------------

## Create filter variable
dat_glances$use_partx <- dat_glances$part_nr

## 4002
rowfinder = which(dat_glances$id == 4002 & dat_glances$interface_nr == 4)
dat_glances$use_partx[rowfinder] <- 2

## 4027
rowfinder = which(dat_glances$id == 4027 & dat_glances$interface_nr == 5)
dat_glances$use_partx[rowfinder] <- 2

## 4037
rowfinder = which(dat_glances$id == 4037 & dat_glances$interface_nr == 6)
dat_glances$use_partx[rowfinder] <- 2

## Filter for corresponding parts
dat_glances <- 
  dat_glances %>% 
  filter(part_nr == use_partx) %>% 
  data.frame()



# Clean dataset #1 --------------------------------------------------------

## Remove unnecessary columns
dat_glances <- 
  dat_glances %>% 
  select(-file, -filepath, -info) %>% 
  select(-time_start_timestamp, -time_end_timestamp, -dur_timestamp) %>% 
  select(-time_start_ms, -time_end_ms, -dur_ms)



# Create addtional time variable containing part 1 and part 2 -------------

# ## Find maximum time in part 1
# time_max_part1 <- 
#   dat_glances %>% 
#   filter(part_nr == 1) %>% 
#   group_by(id) %>% 
#   summarise(time_end_s.max_part1 = max(time_end_s))
# 
# 
# ## Find minimum time in part 2
# time_min_part2 <- 
#   dat_glances %>% 
#   filter(part_nr == 2) %>% 
#   group_by(id) %>% 
#   summarise(time_start_s.min_part2 = min(time_start_s))


## Join data
dat_glances.parts <- dat_glances
# dat_glances.parts <- left_join(dat_glances.parts, time_max_part1)
# dat_glances.parts <- left_join(dat_glances.parts, time_min_part2)


## Correct NA in time_start_s.min_part2 (in case there was no second part)
# dat_glances.parts <- 
#   dat_glances.parts %>% 
#   mutate(time_start_s.min_part2 = 
#            ifelse(is.na(time_start_s.min_part2), 
#                   0,
#                   time_start_s.min_part2))


## Load action times for correct interface nr times
dat_actiontimes <- dbGetSrc("dbconn_study4", "t_videoannot_actions_times")

dat_actiontimes.part_times <- 
  dat_actiontimes %>% 
  select(id, part_nr, interface_nr,
         time_end_s.max_part1,
         time_start_s.min_part2) %>% 
  group_by(id, part_nr) %>% 
  summarise(time_end_s.max_part1 = min(time_end_s.max_part1),
            time_start_s.min_part2 = min(time_start_s.min_part2))

## Merge data
dat_glances.parts <- 
  left_join(dat_glances.parts,
            dat_actiontimes.part_times)



## Correct new time variable by adding ending time information from part 1
dat_glances.parts <- 
  dat_glances.parts %>% 
  group_by(id) %>% 
  # mutate(time_start_s.v2 = 
  #          ifelse(part_nr != 1,
  #                 time_start_s + time_end_s.max_part1,
  #                 time_start_s),
  #        time_end_s.v2 = 
  #          ifelse(part_nr != 1,
  #                 time_start_s + time_end_s.max_part1 + dur_s,
  #                 time_end_s))
  mutate(time_start_s.v2 =
           ifelse(part_nr != 1,
                  time_start_s + time_end_s.max_part1,
                  time_start_s),
         time_end_s.v2 = 
           ifelse(part_nr != 1,
                  time_start_s + time_end_s.max_part1 + dur_s,
                  time_end_s))



# Case exclusion #2: Part selection ---------------------------------------
# 
# ## Create filter variable
# dat_glances$use_partx <- dat_glances$part_nr
# 
# ## 4002
# rowfinder = which(dat_glances$id == 4002 & dat_glances$interface_nr == 4)
# dat_glances$use_partx[rowfinder] <- 2
# 
# ## 4027
# rowfinder = which(dat_glances$id == 4027 & dat_glances$interface_nr == 5)
# dat_glances$use_partx[rowfinder] <- 2
# 
# ## 4037
# rowfinder = which(dat_glances$id == 4037 & dat_glances$interface_nr == 6)
# dat_glances$use_partx[rowfinder] <- 2
# 
# ## Filter for corresponding parts
# dat_glances <- 
#   dat_glances %>% 
#   filter(part_nr == use_partx) %>% 
#   data.frame()



# Clean dataset #2 --------------------------------------------------------

dat_glances.parts <- 
  dat_glances.parts %>% 
  select(id, interface_nr, part_nr,
         time_end_s.max_part1, time_start_s.min_part2,
         glance_seqid,
         time_start_s, time_end_s, dur_s,
         time_start_s.v2, time_end_s.v2) %>% 
  arrange(id, part_nr, time_start_s.v2) 
  #group_by(id, interface_nr) #%>% 
  ## It makes no sense to archive interface times
  ## ... as dataset does not contain the action column
  ## ... which would have given complete information on interface times
  # mutate(time_start_s.v2.min_interface_nr = min(time_start_s.v2),
  #        time_end_s.v2.max_interface_nr = max(time_end_s.v2))



# Identify action blocks --------------------------------------------------

dat_glances.parts.seq <-
  dat_glances.parts %>% 
  #select(id, part_nr, interface_nr, time_start_s.v2, time_end_s.v2, dur_s, action) %>% 

  group_by(id, interface_nr) %>%
  mutate(time_start_s.v2.lead = lead(time_start_s.v2, default = first(time_start_s.v2) )) %>% 
  mutate(time_end_s.v2.lead   = lead(time_end_s.v2,   default = first(time_end_s.v2))) %>% 
  
  mutate(time_end_s.v3 = 
           ifelse( as.character(time_start_s.v2.lead) == as.character(time_end_s.v2) &  #0, time_start_s.v2.lead - time_end_s.v2))#& 
                     time_end_s.v2.lead > time_end_s.v2,
                   time_end_s.v2.lead,
                   time_end_s.v2)) %>% 
  data.frame()



# Aggregate times ---------------------------------------------------------

dat_glances.parts.seq.final <-
  dat_glances.parts.seq %>%
  mutate(time_end_s.v3 = as.character(time_end_s.v3)) %>% 
  group_by(id, interface_nr, time_end_s.v3) %>%
  summarise(part_nr = max(part_nr),
            glance_seqid = max(glance_seqid),
            time_start_s = min(time_start_s),
            time_end_s = max(time_end_s),
            dur_s = sum(dur_s),
            time_start_s.v2 = min(time_start_s.v2)) %>% 
            ## Reasons for not doing see above
            # time_start_s.v2.min_interface_nr = min(time_start_s.v2.min_interface_nr),
            # time_end_s.v2.max_interface_nr = max(time_end_s.v2.max_interface_nr)) %>%
  group_by(id, interface_nr) %>%
  ## It makes no sense to number all glances within on interface_nr
  ## ... as there is no assignment to distinct tasks
  mutate(#glance_nr = row_number(),
         time_end_s.v3 = as.numeric(time_end_s.v3)) %>%
  ## New column order
  select(id,
         part_nr,
         interface_nr,
         ## Reasons for not doing see above
         #time_start_s.v2.min_interface_nr, time_end_s.v2.max_interface_nr,
         #glance_nr,
         glance_seqid,
         time_start_s, time_end_s, dur_s,
         time_start_s.v2, time_end_s.v3) %>%
  data.frame()



# Export to database ------------------------------------------------------

dbWriteTable(dbconn_study4,
             "t_videoannot_glance_times",
             dat_glances.parts.seq.final,
             overwrite = T,
             row.names = F)



