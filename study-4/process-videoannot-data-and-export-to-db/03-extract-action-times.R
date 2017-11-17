
# Load data ---------------------------------------------------------------

dat_actions <- dbGetSrc("dbconn_study4", "t_videoannot")



# Pre-adjustment: 4017 ----------------------------------------------------

## Fifth interface is missing third task in ID 4017
## See notes (take values from third single block of glance annotation)
## Solution: Create dummy action

## Copy example colum
vp417_1_5 <- head( dat_actions %>% filter(id == 4017 & part_nr == 2), 1 )


## Change times
vp417_1_5$time_start_timestamp = "00:01:20.000"
vp417_1_5$time_end_timestamp = "00:01:20.001"
vp417_1_5$dur_timestamp = "00:00:00.001"

vp417_1_5$time_start_s <- conv.timestamp2sec(vp417_1_5$time_start_timestamp)
vp417_1_5$time_end_s <- conv.timestamp2sec(vp417_1_5$time_end_timestamp)
vp417_1_5$time_dur_s <- conv.timestamp2sec(vp417_1_5$dur_timestamp)

vp417_1_5$time_start_ms <- vp417_1_5$time_start_s * 1000
vp417_1_5$time_start_ms <- vp417_1_5$time_start_s * 1000
vp417_1_5$time_start_ms <- vp417_1_5$time_start_s * 1000

vp417_1_5$action <- 1
vp417_1_5$interface_nr <- 5 


## Change column order
vp417_1_5 <- vp417_1_5[, colnames(dat_actions)]


## Complete data
dat_actions <- 
  rbind(dat_actions, vp417_1_5) %>% 
  arrange(id, interface_nr, time_start_s)



# Pre-adjustment: 4023 ----------------------------------------------------

## Fifth interface is missing third task in ID 4023
## See notes (take values from third single block of glance annotation)
## Solution: Take values from the corresponding (but not for action annotated glance)

## Copy example colum
vp423_1_5 <- head( dat_actions %>% filter(id == 4023 & part_nr == 1), 1 )


## Change times
vp423_1_5$time_start_timestamp = "00:40:11.814"
vp423_1_5$time_end_timestamp = "00:40:15.215"
vp423_1_5$dur_timestamp = "00:00:03.401"

vp423_1_5$time_start_s <- conv.timestamp2sec(vp423_1_5$time_start_timestamp)
vp423_1_5$time_end_s <- conv.timestamp2sec(vp423_1_5$time_end_timestamp)
vp423_1_5$time_dur_s <- conv.timestamp2sec(vp423_1_5$dur_timestamp)

vp423_1_5$time_start_ms <- vp423_1_5$time_start_s * 1000
vp423_1_5$time_start_ms <- vp423_1_5$time_start_s * 1000
vp423_1_5$time_start_ms <- vp423_1_5$time_start_s * 1000

vp423_1_5$action <- 1
vp423_1_5$interface_nr <- 5 


## Change column order
vp423_1_5 <- vp423_1_5[, colnames(dat_actions)]


## Complete data
dat_actions <- 
  rbind(dat_actions, vp423_1_5) %>% 
  arrange(id, interface_nr, time_start_s)



# Adjust data -------------------------------------------------------------

## Sort data
dat_actions<-
  dat_actions%>% 
  arrange(id, part_nr, time_start_s)

## Correct part number
dat_actions<- 
  dat_actions %>% 
  mutate(part_nr = ifelse(is.na(part_nr), 1, part_nr)) #%>% 
# mutate(action = ifelse(is.na(action), 0, 1),
#        glance = ifelse(is.na(glance), 0, 1))



# Case exlusion #1 --------------------------------------------------------

dat_actions <- 
  dat_actions %>% 
  filter(id != 4005) %>% 
  ## These following filters absolutely make sense
  ## ... as the "erste Fahrt" contains all relevant information
  ## "zweite Fahrt" contains only information beginning at interface-nr. 4
  filter(!grepl("Vp421-1", file)) %>% 
  #filter(!grepl("Vp421-erste Fahrt", file))
  filter(!grepl("Vp421-zweite Fahrt", file)) 




# Part problem ------------------------------------------------------------

dat_actions$use_partx <- dat_actions$part_nr
## 4002
rowfinder = which(dat_actions$id == 4002 & dat_actions$interface_nr == 4)
dat_actions$use_partx[rowfinder] <- 2

## 4027
rowfinder = which(dat_actions$id == 4027 & dat_actions$interface_nr == 5)
dat_actions$use_partx[rowfinder] <- 2

## 4037
rowfinder = which(dat_actions$id == 4037 & dat_actions$interface_nr == 6)
dat_actions$use_partx[rowfinder] <- 2

dat_actions <- 
  dat_actions %>% 
  filter(part_nr == use_partx) 



# Create new time variable ------------------------------------------------

## Find maximum time in part 1
time_max_part1 <- 
  dat_actions %>% 
  filter(part_nr == 1) %>% 
  group_by(id) %>% 
  summarise(time_end_s.max_part1 = max(time_end_s))

## Find minimum time in part 2
time_min_part2 <- 
  dat_actions %>% 
  filter(part_nr == 2) %>% 
  group_by(id) %>% 
  summarise(time_start_s.min_part2 = min(time_start_s))

## Join data
dat_actions.parts <- dat_actions
dat_actions.parts <- left_join(dat_actions.parts, time_max_part1)
dat_actions.parts <- left_join(dat_actions.parts, time_min_part2)

## Correct NA in time_start_s.min_part2
dat_actions.parts<- 
  dat_actions.parts%>% 
  mutate(time_start_s.min_part2 = 
           ifelse(is.na(time_start_s.min_part2),
                  0,
                  time_start_s.min_part2))

dat_actions.parts<- 
  dat_actions.parts %>% 
  group_by(id) %>% 
  # mutate(time_start_s.v2 = ifelse(part_nr != 1, 
  #                                 time_start_s - time_start_s.min_part2 + time_end_s.max_part1,
  #                                 time_start_s),
  #        time_end_s.v2 = ifelse(part_nr != 1, 
  #                               time_end_s - time_start_s.min_part2 + time_end_s.max_part1,
  #                               time_end_s))
mutate(time_start_s.v2 =
         ifelse(part_nr != 1,
                time_start_s + time_end_s.max_part1,
                time_start_s),
       time_end_s.v2 = 
         ifelse(part_nr != 1,
                time_start_s + time_end_s.max_part1 + dur_s,
                time_end_s))





# Clean data.frame --------------------------------------------------------

dat_actions.parts <- 
  dat_actions.parts %>% 
  # select(-file, -filepath, -info) %>% 
  # select(-time_start_timestamp, -time_end_timestamp, -dur_timestamp) %>% 
  # select(-time_start_ms, -time_end_ms, -dur_ms) %>% 
  select(id, interface_nr, part_nr, 
         time_end_s.max_part1,
         time_start_s.min_part2,
         time_end_s.max_part1, time_start_s.min_part2,
         action,
         time_start_s, time_end_s, dur_s,
         time_start_s.v2, time_end_s.v2) %>% 
  group_by(id, interface_nr) %>% 
  mutate(time_start_s.v2.min_interface_nr = min(time_start_s.v2),
         time_end_s.v2.max_interface_nr = max(time_end_s.v2))



# Aggregate data per interface --------------------------------------------

dat_actions.aggr_interface <- 
  dat_actions.parts %>% 
  group_by(id, interface_nr) %>% 
  summarise(interface_dur_s = max(time_end_s.v2) - min(time_start_s.v2))




# Identify action blocks --------------------------------------------------

## Create row_numbers
dat_actions.parts.seq <-
  dat_actions.parts %>% 
  #select(id, part_nr, interface_nr, time_start_s.v2, time_end_s.v2, dur_s, action) %>% 
  arrange(id, part_nr, time_start_s.v2) %>% 
  filter(action == 1) %>% 

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

dat_actions.parts.seq.final <-
  dat_actions.parts.seq %>%
  mutate(time_end_s.v3 = as.character(time_end_s.v3)) %>% 
  group_by(id, interface_nr, time_end_s.v3) %>%
  summarise(part_nr = max(part_nr),
            time_end_s.max_part1 = max(time_end_s.max_part1),
            time_start_s.min_part2 = max(time_start_s.min_part2),
            time_start_s.min_part2,
            time_start_s = min(time_start_s),
            time_end_s = max(time_end_s),
            dur_s = sum(dur_s),
            time_start_s.v2 = min(time_start_s.v2),
            time_start_s.v2.min_interface_nr = min(time_start_s.v2.min_interface_nr),
            time_end_s.v2.max_interface_nr = max(time_end_s.v2.max_interface_nr)) %>%
  group_by(id, interface_nr) %>%
  arrange(id, part_nr, time_start_s.v2) %>% 
  mutate(task_nr = row_number(),
         time_end_s.v3 = as.numeric(time_end_s.v3)) %>%
  ## New column order
  select(id,
         part_nr,
         time_end_s.max_part1,
         time_start_s.min_part2,
         interface_nr, task_nr,
         time_start_s.v2.min_interface_nr, time_end_s.v2.max_interface_nr,
         time_start_s, time_end_s, dur_s,
         time_start_s.v2, time_end_s.v3) %>%
  data.frame()

#View(dat_actions.parts.seq.final %>% group_by(id, interface_nr) %>% summarise(n = n()))

# Export to database ------------------------------------------------------

dbWriteTable(dbconn_study4,
             "t_videoannot_actions_times",
             dat_actions.parts.seq.final,
             overwrite = T,
             row.names = F)
