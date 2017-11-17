
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
  filter(glance == 1)



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
         glance,
         time_start_s, time_end_s, dur_s,
         time_start_s.v2, time_end_s.v2) %>% 
  group_by(id, interface_nr) #%>% 
  ## It makes no sense to archive interface times
  ## ... as dataset does not contain the action column
  ## ... which would have given complete information on interface times
  # mutate(time_start_s.v2.min_interface_nr = min(time_start_s.v2),
  #        time_end_s.v2.max_interface_nr = max(time_end_s.v2))



# Identify action blocks --------------------------------------------------

## Create row_numbers
dat_glances.parts.seq <-
  dat_glances.parts %>% 
  #select(id, part_nr, interface_nr, time_start_s.v2, time_end_s.v2, dur_s, action) %>% 
  arrange(id, part_nr, time_start_s.v2) %>% 
  filter(glance == 1) %>% 
  
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
         time_start_s, time_end_s, dur_s,
         time_start_s.v2, time_end_s.v3) %>%
  data.frame()



# Export to database ------------------------------------------------------

dbWriteTable(dbconn_study4,
             "t_videoannot_glance_times",
             dat_glances.parts.seq.final,
             overwrite = T,
             row.names = F)






# Load task data ----------------------------------------------------------

dat2add_actions <- dbGetSrc("dbconn_study4", "t_videoannot_actions_times")



# Add action information --------------------------------------------------

test <- 
  left_join(dat2add_actions,
            dat_glances,
            by = c("id", "interface_nr")) %>% 
  data.frame()


test2 <- 
  test %>% 
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
         time_end_s.v3.action = time_end_s.v3,
         
         glance,
         time_start_s.glance = time_start_s.y,
         time_end_s.glance = time_end_s.y,
         dur_s.glance = dur_s.y,
         time_start_s.v2.glance = time_start_s.v2.y,
         time_end_s.v2.glance = time_end_s.v2
)

# View(test2 %>% filter(id == 4002))

test3 <- 
  test2 %>% 
  group_by(id, interface_nr, task_nr) %>% 
  mutate(tasky = ifelse(time_start_s.glance >= (time_start_s.action - 1) &
                          time_end_s.glance <= (time_end_s.action + 2),
                        task_nr,
                        NA)) %>% 
  # mutate(tasky = as.numeric(ifelse(is.na(tasky) &
  #                                    time_start_s.glance >= (time_start_s.action - 10) &
  #                                    time_end_s.glance <= (time_end_s.action + 1),
  #                                  task_nr * 10,
  #                                  tasky))) %>% 
  mutate(tasky = as.numeric(ifelse(is.na(tasky) &
                                     time_start_s.glance >= (time_start_s.action - 1) &
                                     time_end_s.glance >= (time_end_s.action + 1) &
                                     time_end_s.glance <= (time_end_s.action + 10),
                                     #time_end_s.glance <= (time_end_s.action + 10),
                                   task_nr * 10 + 9,
                                   tasky))) %>%
  mutate(timestamp_start = conv.sec2timestamp(time_start_s.glance))

# 
# View(test2 %>% filter(id == 4002) %>% 
#        select(id, part_nr, interface_nr, block_task_nr, 
#               time_start_s.y, time_end_s.y, dur_s.y, 
#               glance, 
#               time_start_s.x, time_end_s.x, 
#               time_start_s.v2, time_end_s.v2, dur_s.x,
#               blocky))


cond <- dbGetSrc("dbconn_study4", "t_procedure")
cond.aggr <- 
  cond %>% 
  filter(block > 0 & block < 9 & block_task != 1) %>% 
  group_by(id, interface_nr = block, block_task) %>% 
  summarise(condition = max(condition),
            task = max(task)) %>% 
  ungroup() %>% 
  mutate(id = substr(as.character(id), 2, 3)) %>% 
  mutate(id = as.numeric(id) + 4000,
         task_nr = block_task - 1)

test4 <- left_join(cond.aggr, test3)

test4 <- 
  test4 %>% 
  mutate(itype = ifelse(grepl("g", condition), "gesture", "touch")) %>% 
  mutate(ilevel = ifelse(grepl("s", condition), "simple", "complex")) %>% 
  mutate(scenario = ifelse(grepl("s", condition), "city", "motorway")) 


test5 <- 
  test4 %>% 
  filter(!is.na(tasky)) %>% 
  arrange(id, interface_nr, task_nr)



# Filter (Lisa) -----------------------------------------------------------

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

test5_backup <- test5

insvisible(
lapply(filterlist, function(x) {
  print(x)
  rowfinder <- which(test6$id == x[1] & 
                       test6$interface_nr == x[2] &
                       test6$task_nr == x[3])
  print(rowfinder)
  #print(length(rowfinder))
  #print(test6[rowfinder, c("id", "interface_nr", "condition", "task_nr", "dur_s.glance")] %>% data.frame())
  if (length(rowfinder) > 0)
    test5 <<- test5[-rowfinder, ]
  #return(test6[-rowfinder, ])
})
)

# Visualisation: Average glance duration ----------------------------------

glanceExplorer <- function(dat2proc, group, keyval, varinterest) {
  
  keyval <- paste("x.", keyval, sep = "")
  
  dat2proc.aggr <- 
    dat2proc %>% 
    group_by_(.dots = group) %>% 
    summarise_(x.n = "n()",
              x.sum = paste("sum(", varinterest, ")"),
              x.avg = paste("mean(", varinterest, ")"),
              #x.med = paste("median(", varinterest, ")"),
              x.sd = paste("sd(", varinterest, ")"),
              x.min = paste("min(", varinterest, ")"),
              x.max = paste("max(", varinterest, ")"))
  
  print(dat2proc.aggr)
  
  plotdat <- ggplot() + 
    geom_bar(data = dat2proc.aggr, 
             aes_string(x = group, 
                        y = keyval,
                        fill = group), 
             stat = "identity") +
    labs(x = group,
         y = paste(varinterest, keyval, sep = "."))
  
  plot(plotdat)
}




glanceExplorer(test5, "task", "avg", "dur_s.glance")
glanceExplorer(test5, "interface_nr", "avg", "dur_s.glance")
glanceExplorer(test5, "condition", "avg", "dur_s.glance")
glanceExplorer(test5, "itype", "avg", "dur_s.glance")
glanceExplorer(test5, "ilevel", "avg", "dur_s.glance")
glanceExplorer(test5, "scenario", "avg", "dur_s.glance")

glanceExplorer(test5, "task", "n", "dur_s.glance")




test5.aggr <- 
  test5 %>% 
  group_by(id, condition, itype, ilevel, scenario, interface_nr, task, task_nr) %>% 
  summarise(glance_n = n(),
            glance_s.sum = sum(dur_s.glance),
            glance_s.avg = mean(dur_s.glance),
            glance_s.sd = sd(dur_s.glance),
            glance_s.min = min(dur_s.glance),
            glance_s.max = max(dur_s.glance)) %>% 
  arrange(id, interface_nr, task_nr)


glanceExplorer(test5.aggr, "task", "avg", "glance_n")
glanceExplorer(test5.aggr, "interface_nr", "avg", "glance_n")
glanceExplorer(test5.aggr, "condition", "avg", "glance_n")
glanceExplorer(test5.aggr, "itype", "avg", "glance_n")
glanceExplorer(test5.aggr, "ilevel", "avg", "glance_n")
glanceExplorer(test5.aggr, "scenario", "avg", "glance_n")


glanceExplorer(test5.aggr, "task", "avg", "glance_s.sum")
glanceExplorer(test5.aggr, "itype", "avg", "glance_s.sum")
glanceExplorer(test5.aggr, "itype", "avg", "glance_s.avg")
glanceExplorer(test5.aggr, "condition", "avg", "glance_s.avg")



## Mit Excel-Tabelle vergleichen
test5 %>% 
  group_by(id, condition) %>% 
  summarise(glance_n = n(),
            glance_s.sum = sum(dur_s.glance),
            glance_s.avg = mean(dur_s.glance),
            glance_s.sd = sd(dur_s.glance),
            glance_s.min = min(dur_s.glance),
            glance_s.max = max(dur_s.glance)) %>% 
  arrange(id, condition) %>%
  data.frame














# PLAYGROUND --------------------------------------------------------------



# Count glances -----------------------------------------------------------

## Create row_numbers
dat_glances.glances_seq <-
  dat_glances %>% 
  select(id, part_nr, interface_nr, time_start_s.v2, time_end_s.v2, dur_s, glance, action) %>% 
  arrange(id, part_nr, time_start_s.v2) %>% 
  #filter(glance == 1) %>% 
  group_by(id, interface_nr) %>%
  mutate(time_end_s.v2.diff_temp = time_end_s.v2 - lag(time_end_s.v2)) %>% 
  group_by(id, interface_nr, glance) %>%
  ## Correction for action-to-action errors
  ungroup() %>% 
  mutate(time_end_s.v3 = ifelse(lead(time_start_s.v2) == time_end_s.v2, lead(time_end_s.v2), time_end_s.v2)) %>% 
  group_by(id, interface_nr, glance) %>%
  mutate(time_end_s.v3.diff_temp.cummax = ifelse(is.na(glance), 0, cummax(time_end_s.v3)))

dat_glances.glances_seq.n <- 
  dat_glances.glances_seq %>% 
  group_by(id, interface_nr) %>% 
  summarise(glances_n = n_distinct(time_end_s.v3.diff_temp.cummax) - 1)

dat_glances.glances_seq.dur <- 
  dat_glances.glances_seq %>% 
  group_by(id, interface_nr) %>% 
  filter(glance == 1 & time_end_s.v3.diff_temp.cummax != 0) %>% 
  mutate(glances_n = n_distinct(time_end_s.v3.diff_temp.cummax)) %>% 
  group_by(id, interface_nr, time_end_s.v3.diff_temp.cummax) %>% 
  summarise(glances_n = max(glances_n), # Just Info (same for each sequence)
            dur_s = max(dur_s))   

dat_glances.glances_seq.dur.summary <- 
  dat_glances.glances_seq.dur %>% 
  group_by(id, interface_nr) %>% 
  summarise(glances_n = max(glances_n), 
            dur_s.sum = sum(dur_s),
            dur_s.mean = mean(dur_s),
            dur_s.min = min(dur_s),
            dur_s.max = max(dur_s))

test_neu <- 
  left_join(expand.grid(id = c(4002:4004,4006:4012,4014:4038), block = 1:8) %>% arrange(id),
            dat_glances.glances_seq.dur.summary)
test <- dbGetSrc("dbconn_study4", "t_glances_lisa")


# cond <- dbGetSrc("dbconn_study4", "t_procedure")
# cond <- 
#   cond %>% 
#   filter(block > 0 & block < 9) %>% 
#   group_by(id, interface_nr = block) %>% 
#   summarise(condition = max(condition)) %>% 
#   ungroup() %>% 
#   mutate(id = substr(as.character(id), 2, 3)) %>% 
#   mutate(id = as.numeric(id) + 4000)

cond <- dbGetSrc("dbconn_study4", "t_procedure")
cond.aggr <- 
  cond %>% 
  filter(block > 0 & block < 9 & block_task != 1) %>% 
  group_by(id, interface_nr = block, block_task) %>% 
  summarise(condition = max(condition),
            task = max(task)) %>% 
  ungroup() %>% 
  mutate(id = substr(as.character(id), 2, 3)) %>% 
  mutate(id = as.numeric(id) + 4000,
         block_task_nr = block_task - 1)

test2 <- 
  left_join(cond.aggr, dat_glances.glances_seq.dur.summary)



# Visualisation -----------------------------------------------------------

## Condition
test3 <- 
  test2 %>% 
  filter(!id %in% c(4005)) %>% 
  group_by(condition) %>% 
  summarise(dur_s.mean = mean(dur_s.mean, na.rm = T),
            glances_n.mean = mean(glances_n, na.rm = T))

ggplot() +
  geom_bar(data = test3, aes(x = condition, y = dur_s.mean, fill = condition), stat = "identity") #+
  #facet_grid(.~interface_nr)

ggplot() +
  geom_bar(data = test3, aes(x = condition, y = glances_n.mean, fill = condition), stat = "identity")


## Interface-Nr.
test3 <- 
  test2 %>% 
  filter(!id %in% c(4005)) %>% 
  group_by(interface_nr) %>% 
  summarise(dur_s.mean = mean(dur_s.mean, na.rm = T),
            glances_n.mean = mean(glances_n, na.rm = T))

ggplot() +
  geom_bar(data = test3, aes(x = interface_nr, y = dur_s.mean, fill = interface_nr), stat = "identity") #+
#facet_grid(.~interface_nr)

ggplot() +
  geom_bar(data = test3, aes(x = interface_nr, y = glances_n.mean, fill = interface_nr), stat = "identity")



# Inference ---------------------------------------------------------------



test2 <- 
  test2 %>% 
  mutate(itype = substr(condition, 1, 1)) %>% 
  mutate(scenario = substr(condition, 2, 2)) %>% 
  mutate(ilevel = substr(condition, 3, 3))

test5<- test2
test5$dur_s.mean[is.na(test5$dur_s.mean)] <- 0
test5$glances_n[is.na(test5$glances_n)] <- 0

model_glance_dur <- 
  ezANOVA(data = test5%>% data.frame(), 
          dv = dur_s.mean, 
          wid = id, 
          within = .(itype, ilevel, scenario), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_glance_dur$ANOVA


model_glance_n <- 
  ezANOVA(data = test5%>% data.frame(), 
          dv = glances_n, 
          wid = id, 
          within = .(itype, ilevel, scenario), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_glance_n$ANOVA
