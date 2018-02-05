
test <-
  data.frame(time_s = seq(0, 20, 0.1)) %>% 
  mutate(preceding_speed_ms = 0) %>% 
  mutate(preceding_speed_ms = 10 * 0.1) %>% 
  mutate(preceding_speed_ms = cumsum(preceding_speed_ms)) %>% 
  rowwise() %>% 
  mutate(preceding_speed_ms = min(preceding_speed_ms, 50/3.6)) %>% 
  ungroup() %>% 
  mutate(preceding_s_diff = preceding_speed_ms * 0.1) %>% 
  #mutate(preceding_s = 0) %>% 
  mutate(preceding_s = cumsum(preceding_s_diff)) %>% 
  mutate(preceding_dist = preceding_s)

plot(test$time_s, test$preceding_speed_ms, type = "l", ylim = c(0, 20))
plot(test$time_s, test$preceding_dist, type = "l")
