
# Settings ----------------------------------------------------------------

sett_analyse <- c()
sett_analyse$db_conn_name <- dbFindConnObj("Study-5", output = F)
sett_analyse$db_name_q <- "Questionnaires"
sett_analyse$db_conn_name_q <- dbFindConnObj(sett_analyse$db_name_q, output = F)

sett_quest <- c()
sett_quest$trust <- 
  dbGetSrc(sett_analyse$db_conn_name_q, 
           "Trust-in-automated-systems_Jian_1998")
sett_quest$trust$item_code <- sprintf("trust_%02d", sett_quest$trust$itemnr)



# Retrieve data -----------------------------------------------------------

dat_trust_gestures_simple <- 
  dbReadTable(get(sett_analyse$db_conn_name), "t_q_trust_gestures_simple")
dat_trust_gestures_complex <- 
  dbReadTable(get(sett_analyse$db_conn_name), "t_q_trust_gestures_complex")
dat_trust_touch_simple <- 
  dbReadTable(get(sett_analyse$db_conn_name), "t_q_trust_touch_simple")
dat_trust_touch_complex <- 
  dbReadTable(get(sett_analyse$db_conn_name), "t_q_trust_touch_complex")



# Row-bind data -----------------------------------------------------------

dat_trust <- 
  rbind(data.frame(dat_trust_gestures_simple, condition = "gestures_simple", stringsAsFactors = F),
        data.frame(dat_trust_gestures_complex, condition = "gestures_complex", stringsAsFactors = F),
        data.frame(dat_trust_touch_simple, condition = "touch_simple", stringsAsFactors = F),
        data.frame(dat_trust_touch_complex, condition = "touch_complex", stringsAsFactors = F))



# Compute summary ---------------------------------------------------------

dat_trust_summary <- 
  dat_trust %>% 
  #select(-subject_id, -na_counter) %>% 
  select(-subject_id) %>% 
  group_by(condition) %>% 
  gather(key = "item_code", value = "response", -"condition") %>% 
  group_by(condition, item_code) %>% 
  summarise(response_mean = mean(response, na.rm = T),
            response_sd = sd(response, na.rm = T))



# Visualize item profile --------------------------------------------------

dat_trust_summary <- 
  left_join(dat_trust_summary,
            sett_quest$trust %>% 
              select(item_code, item_eng)) %>% 
  ## Keep original order of items
  mutate(item_eng = factor(item_eng, levels = unique(item_eng))) 


# Viz ---------------------------------------------------------------------



plot_item_profile <- 
  plotItemProfile(dat_trust_summary,
                  col_name_items = "item_eng",
                  col_name_group = "condition",
                  value_range = c(0, 10))

plot(plot_item_profile)

plot_item_profile <-
  plot_item_profile + 
  labs(x = NULL,
       y = "Response mean") +
  theme_bw() + 
  theme(legend.justification = c(1, 1), 
        legend.position = c(0.999, 0.999),
        legend.background = element_rect(color = "black",
                                         size = 0.2)) +
  scale_linetype_manual(name = "Data Source",
                        values = c("solid", "solid", "solid", "solid")) +
  scale_size_manual(name = "Data Source",
                    values = rep(0.35, 4)) +
  scale_color_manual(name = "Data Source",
                     values = c("green3", "red2", "grey50", "blue2")) + 
  # scale_linetype_manual(name = "Data Source",
  #                       values = c("solid", "longdash", "dashed")) +
  # scale_shape_manual(name = "Data Source",
  #                    values = c(NA, NA, NA)) +
  # scale_size_manual(name = "Data Source",
  #                   values = rep(0.35, 3)) + 
  # scale_alpha_manual(name = "Data Source",
  #                    values = rep(0.35, 3)) + 
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black")) + 
  theme(legend.key.size = unit(0.2, "cm"),
        legend.margin = margin(rep(0.1, 4), unit='cm'),
        legend.title = element_text(size = 6, color = "black"),
        legend.text = element_text(size = 6, color = "black"))


plot(plot_item_profile)