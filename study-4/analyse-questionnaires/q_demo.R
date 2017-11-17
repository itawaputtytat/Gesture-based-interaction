dat_demo <- dbGetSrc("dbconn_study4", "t_q_demo")
dat_demo %>% 
  filter(id != 4013) %>% 
  summarise(age.avg = mean(age, na.rm = T),
            age.sd = sd(age, na.rm = T))

dat_demo %>% 
  filter(id != 4013) %>% 
  select(age) %>% 
  summary()
