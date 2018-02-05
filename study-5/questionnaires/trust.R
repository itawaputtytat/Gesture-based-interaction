
# Settings ----------------------------------------------------------------

sett_analyse <- c()
sett_analyse$db_name <- "GBI_Study-5"
sett_analyse$db_conn_name <- dbFindConnObj(sett_analyse$db_name, output = F)
sett_analyse$db_name_q <- "Questionnaires"
sett_analyse$db_conn_name_q <- dbFindConnObj(sett_analyse$db_name_q, output = F)

sett_quest <- c()
sett_quest$trust <- dbGetSrc(sett_analyse$db_conn_name_q, "Trust-in-automated-systems_Jian_1998")
sett_quest$trust$item_code <- sprintf("trust_%02d", sett_quest$trust$item_id)



# Retrieve data -----------------------------------------------------------

dat_trust_gesture_simple <- dbReadTable(get(sett_analyse$db_conn_name), "t_q_trust_gestures_simple")
dat_trust_gesture_complex <- dbReadTable(get(sett_analyse$db_conn_name), "t_q_trust_gestures_complex")
dat_trust_touch_simple <- dbReadTable(get(sett_analyse$db_conn_name), "t_q_trust_touch_simple")
dat_trust_touch_complex <- dbReadTable(get(sett_analyse$db_conn_name), "t_q_trust_touch_complex")



# Row-bind data -----------------------------------------------------------

dat_trust <- 
  rbind(
    data.frame(dat_trust_gesture_simple, group = "Gesture", level = "simple", stringsAsFactors = F),
    data.frame(dat_trust_gesture_complex, group = "Gesture", level = "complex", stringsAsFactors = F),
    data.frame(dat_trust_touch_simple, group = "Touch", level = "simple", stringsAsFactors = F),
    data.frame(dat_trust_touch_complex, group = "Touch", level = "complex", stringsAsFactors = F)
  )




# Compute summary ---------------------------------------------------------

dat_trust_summary <- 
  dat_trust %>% 
  select(-subject_id) %>% 
  group_by(group) %>% 
  gather(key = "item_code", value = "response", -"group", -"level") %>% 
  group_by(group, item_code) %>% 
  summarise(response_mean = mean(response, na.rm = T),
            response_sd = sd(response, na.rm = T))



# Visualize item profile --------------------------------------------------

dat_trust_summary <- 
  left_join(dat_trust_summary,
            sett_quest$trust %>% 
              select(item_code, item_text_eng)) %>% 
  ## Keep original order of items
  mutate(item_text_eng = factor(item_text_eng, levels = unique(item_text_eng))) 

plot_item_profile_trust <- 
  plotItemProfile(dat_trust_summary,
                  col_name_items = "item_text_eng",
                  col_name_group = "group",
                  value_range = c(1, 7))

plot(plot_item_profile_trust)



# Post-process item profile -----------------------------------------------

plot_item_profile_trust <- 
  plot_item_profile_trust +
  scale_x_discrete(limits = rev(unique(dat_trust_summary$item_text_eng)),
                   position = "top") +
  scale_y_continuous(breaks = c(1:7)) +
  ggtitle(label = "Trust",
          subtitle = "") + 
  labs(x = NULL,
       y = "Response mean") +
  theme_bw() + 
  theme(#legend.justification = c(0, 0), 
    #legend.position = c(0.002, 0.002),
    legend.position=c(0,1), 
    legend.justification=c(0.13, 0),
    legend.direction="horizontal",
    #legend.position = "top",
    #legend.background = element_rect(color = "black", size = 0.2)
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
        #legend.margin = margin(rep(0.1, 4), unit='cm'),
        #legend.margin = margin(c(-0.5, 0.7, -0.25, 0), unit='cm'),
        #legend.margin=margin(t = 0, l = 0, unit='cm'),
        legend.key = element_blank(),
        #legend.title = element_text(size = 5, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 6, color = "black"))
# theme(legend.key.size = unit(0.2, "cm"),
#       legend.margin = margin(rep(0.1, 4), unit='cm'),
#       legend.title = element_text(size = 6, color = "black"),
#       legend.text = element_text(size = 6, color = "black"))

plot(plot_item_profile_trust)

ggsave(filename = "trust_item-profile.png", 
       plot = plot_item_profile_trust,
       path = "plots",
       width = 10,
       height = 7,
       units = "cm",
       dpi = 600)
