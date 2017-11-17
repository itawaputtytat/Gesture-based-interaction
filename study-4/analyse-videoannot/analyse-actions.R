

# Prepare data ------------------------------------------------------------

dat_actions <- dbGetSrc("dbconn_study4", "t_videoannot_actions_times")

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

test <- left_join(cond.aggr, 
                  dat_actions,
                  by = c("id", "interface_nr", "block_task_nr"))

test$dur_s.v2 <- test$time_end_s - test$time_start_s

test$dur_s.v3 <- as.numeric(codeOutliersZ(test$dur_s.v2))




# Vis Boxplots with outliers ----------------------------------------------

plot(
  ggplot() + 
    geom_boxplot(data = test, 
                 aes(x = task, 
                     y = dur_s.v2, 
                     fill = task)) +
    stat_boxplot(geom ='errorbar', size = 0.25)
)

plot(
  ggplot() + 
    geom_boxplot(data = test, 
                 aes(x = condition, 
                     y = dur_s.v2, 
                     fill = task)) +
    stat_boxplot(geom ='errorbar', size = 0.25)
)

plot(
  ggplot() + 
    geom_boxplot(data = test, 
                 aes(x = interface_nr, 
                     y = dur_s.v2, 
                     fill = task)) +
    stat_boxplot(geom ='errorbar', size = 0.25)
)

# Visualisation: Boxplots -------------------------------------------------

plot(
ggplot() + 
  geom_boxplot(data = test, 
               aes(x = task, 
                   y = dur_s.v3, 
                   fill = task)) +
  stat_boxplot(geom ='errorbar', size = 0.25)
)

plot(
ggplot() + 
  geom_boxplot(data = test, 
               aes(x = condition, 
                   y = dur_s.v3, 
                   fill = task)) +
  stat_boxplot(geom ='errorbar', size = 0.25)
)

plot(
  ggplot() + 
    geom_boxplot(data = test, 
                 aes(x = interface_nr, 
                     y = dur_s.v3, 
                     fill = task)) +
    stat_boxplot(geom ='errorbar', size = 0.25)
)



# Visualisation: Bar ------------------------------------------------------




## Block task
test2 <- 
  test %>% 
  filter(!id %in% c(4005)) %>% 
  group_by(task) %>% 
  summarise(dur_s.mean = mean(dur_s, na.rm = T))

ggplot() + 
  geom_bar(data = test2, 
           aes(x = task, 
               y = dur_s.mean, 
               fill = task), 
           stat = "identity")



## Interface-Nr.
test2 <- 
  test %>% 
  filter(!id %in% c(4005)) %>% 
  group_by(interface_nr) %>% 
  summarise(dur_s.mean = mean(dur_s, na.rm = T))

#plotdat <- 
  ggplot() + 
  geom_bar(data = test2, 
           aes(x = as.factor(interface_nr), 
               y = dur_s.mean, 
               fill = as.factor(interface_nr)), 
           stat = "identity") +
  coord_cartesian(ylim = c(0, 10),
                  xlim = c(0.25, 8.75),
                  expand = F)
#plot(plotdat)


## Condition
test2 <- 
  test %>% 
  filter(!id %in% c(4005)) %>% 
  group_by(condition) %>% 
  summarise(dur_s.mean = mean(dur_s, na.rm = T))

plotdat <- 
  ggplot() + 
  geom_bar(data = test2, 
           aes(x = condition, 
               y = dur_s.mean, 
               fill = condition), 
           stat = "identity") +
  coord_cartesian(ylim = c(0, 13),
                  xlim = c(0.25, 8.75),
                  expand = F)
plot(plotdat)
