
# Settings ----------------------------------------------------------------

sett_analyse <- c()
sett_analyse$db_name <- "GBI_Study-5"
sett_analyse$db_conn_name <- dbFindConnObj(sett_analyse$db_name, output = F)
sett_analyse$db_name_q <- "Questionnaires"
sett_analyse$db_conn_name_q <- dbFindConnObj(sett_analyse$db_name_q, output = F)

sett_quest <- c()
sett_quest$nasatlx <- dbGetSrc(sett_analyse$db_conn_name_q, "NASA-TLX_Hart_1988")
sett_quest$nasatlx$item_code <- sprintf("nasatlx_%02d", sett_quest$nasatlx$item_id)



# Retrieve data -----------------------------------------------------------

dat_nasatlx_gesture_simple <- dbReadTable(get(sett_analyse$db_conn_name), "t_q_nasatlx_gestures_simple")
dat_nasatlx_gesture_complex <- dbReadTable(get(sett_analyse$db_conn_name), "t_q_nasatlx_gestures_complex")
dat_nasatlx_touch_simple <- dbReadTable(get(sett_analyse$db_conn_name), "t_q_nasatlx_touch_simple")
dat_nasatlx_touch_complex <- dbReadTable(get(sett_analyse$db_conn_name), "t_q_nasatlx_touch_complex")



# Row-bind data -----------------------------------------------------------

dat_nasatlx <- 
  rbind(
    data.frame(dat_nasatlx_gesture_simple, group = "Gesture", level = "simple", stringsAsFactors = F),
    data.frame(dat_nasatlx_gesture_complex, group = "Gesture", level = "complex", stringsAsFactors = F),
    data.frame(dat_nasatlx_touch_simple, group = "Touch", level = "simple", stringsAsFactors = F),
    data.frame(dat_nasatlx_touch_complex, group = "Touch", level = "complex", stringsAsFactors = F)
  )




# Compute summary ---------------------------------------------------------

dat_nasatlx_summary <- 
  dat_nasatlx %>% 
  select(-subject_id) %>% 
  group_by(group) %>% 
  gather(key = "item_code", value = "response", -"group", -"level") %>% 
  group_by(group, item_code) %>% 
  summarise(response_mean = mean(response, na.rm = T),
            response_sd = sd(response, na.rm = T))



# Visualize item profile --------------------------------------------------

dat_nasatlx_summary <- 
  left_join(dat_nasatlx_summary,
            sett_quest$nasatlx %>% 
              select(item_code, item_text_eng)) %>% 
  ## Keep original order of items
  #mutate(item_text_eng = factor(item_text_eng, levels = unique(item_text_eng))) 
  mutate(item_text_eng = factor(item_text_eng, levels = sett_quest$nasatlx$item_text_eng)) %>% 
  mutate(item_text_eng = breakStringToLines(item_text_eng, max_nchar = 55)) #max_nlines = 2)) %>% 

plot_item_profile_nasatlx <- 
  plotItemProfile(dat_nasatlx_summary,
                  col_name_items = "item_text_eng",
                  col_name_group = "group",
                  value_range = c(0, 20))

plot(plot_item_profile_nasatlx)



# Post-process item profile -----------------------------------------------

plot_item_profile_nasatlx <- 
  plot_item_profile_nasatlx +
  scale_x_discrete(limits = rev(dat_nasatlx_summary$item_text_eng),
                   position = "top") +
  #scale_y_continuous(breaks = c(-1, seq(0, 20, 5))) +
  ggtitle(label = "NASA-TLX",
          subtitle = "") + 
  labs(x = NULL,
       y = "Response mean") +
  theme_bw() + 
  theme(
    legend.position=c(0,1), 
    legend.justification=c(0.13, 0),
    legend.direction="horizontal",
    legend.background = element_blank()
  ) +
  scale_color_manual(name = "Interaction type",
                     values = c("firebrick3", "dodgerblue4")) + 
  scale_linetype_manual(name = "Interaction type",
                        values = c("solid", "dashed")) +
  scale_shape_manual(name = "Interaction type",
                     values = c(20, 20)) +
  scale_size_manual(name = "Interaction type",
                    values = rep(0.35, 3)) + 
  scale_alpha_manual(name = "Interaction type",
                     values = rep(0.35, 3)) + 
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black")) + 
  theme(legend.key.size = unit(0.25, "cm"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 6, color = "black"))

plot(plot_item_profile_nasatlx)

ggsave(filename = "nasatlx_item-profile.png", 
       plot = plot_item_profile_nasatlx,
       path = "plots",
       width = 10,
       height = 6,
       units = "cm",
       dpi = 600)
