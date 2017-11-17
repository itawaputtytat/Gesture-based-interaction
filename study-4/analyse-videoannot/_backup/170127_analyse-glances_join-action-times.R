
# Load glance data --------------------------------------------------------

dat_glances <- dbGetSrc("dbconn_study4", "t_videoannot")



# Adjust data -------------------------------------------------------------

## Sort data
dat_glances <-
  dat_glances %>% 
  arrange(id, part_nr, time_start_s) 

## Correct part number
dat_glances <- 
  dat_glances %>% 
  mutate(part_nr = ifelse(is.na(part_nr), 1, part_nr)) #%>% 
  # mutate(action = ifelse(is.na(action), 0, 1),
  #        glance = ifelse(is.na(glance), 0, 1))



# Create new time variable ------------------------------------------------

## Find maximum time in part 1
time_max_part1 <- 
  dat_glances %>% 
  filter(part_nr == 1) %>% 
  group_by(id) %>% 
  summarise(time_end_s.max_part1 = max(time_end_s))

## Find minimum time in part 2
time_min_part2 <- 
  dat_glances %>% 
  filter(part_nr == 2) %>% 
  group_by(id) %>% 
  summarise(time_start_s.min_part2 = min(time_start_s))

## Join data
dat_glances <- left_join(dat_glances, time_max_part1)
dat_glances <- left_join(dat_glances, time_min_part2)

## Correct NA in time_start_s.min_part2
dat_glances <- 
  dat_glances %>% 
  mutate(time_start_s.min_part2 = ifelse(is.na(time_start_s.min_part2),
                                         0,
                                         time_start_s.min_part2))

dat_glances <- 
  dat_glances %>% 
  group_by(id) %>% 
  mutate(time_start_s.v2 = ifelse(part_nr != 1, 
                                  time_start_s - time_start_s.min_part2 + time_end_s.max_part1,
                                  time_start_s),
         time_end_s.v2 = ifelse(part_nr != 1, 
                                time_end_s - time_start_s.min_part2 + time_end_s.max_part1,
                                time_end_s))



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
