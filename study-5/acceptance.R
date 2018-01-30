
# Settings ----------------------------------------------------------------

sett_analyse <- c()
sett_analyse$db_name <- "GBI_Study-5"
sett_analyse$db_conn_name <- dbFindConnObj(sett_analyse$db_name, output = F)
sett_analyse$db_name_q <- "Questionnaires"
sett_analyse$db_conn_name_q <- dbFindConnObj(sett_analyse$db_name_q, output = F)

sett_quest <- c()
sett_quest$acc <- dbGetSrc(sett_analyse$db_conn_name_q, "Acceptance_Van-der-Laan_1997")
sett_quest$acc$item_code <- sprintf("acceptance_%02d", sett_quest$acc$item_id)



# Retrieve data -----------------------------------------------------------

dat_acc_gesture_simple <- dbReadTable(get(sett_analyse$db_conn_name), "t_q_acceptance_gestures_simple")
dat_acc_gesture_complex <- dbReadTable(get(sett_analyse$db_conn_name), "t_q_acceptance_gestures_complex")
dat_acc_touch_simple <- dbReadTable(get(sett_analyse$db_conn_name), "t_q_acceptance_touch_simple")
dat_acc_touch_complex <- dbReadTable(get(sett_analyse$db_conn_name), "t_q_acceptance_touch_complex")



# Row-bind data -----------------------------------------------------------

dat_acc <- 
  rbind(
    data.frame(dat_acc_gesture_simple, group = "Gesture", level = "simple", stringsAsFactors = F),
    data.frame(dat_acc_gesture_complex, group = "Gesture", level = "complex", stringsAsFactors = F),
    data.frame(dat_acc_touch_simple, group = "Touch", level = "simple", stringsAsFactors = F),
    data.frame(dat_acc_touch_complex, group = "Touch", level = "complex", stringsAsFactors = F)
  )

col_finder <- sett_quest$acc$item_code[sett_quest$acc$is_reversed]
dat_acc[, col_finder] <- dat_acc[, col_finder] + 3
dat_acc[, col_finder] <- recodeItems(dat_acc[, col_finder], col_finder, 5)
dat_acc[, col_finder] <- dat_acc[, col_finder] - 3



# Compute summary ---------------------------------------------------------

dat_acc_summary <- 
  dat_acc %>% 
  select(-subject_id) %>% 
  group_by(group) %>% 
  gather(key = "item_code", value = "response", -"group", -"level") %>% 
  group_by(group, item_code) %>% 
  summarise(response_mean = mean(response, na.rm = T),
            response_sd = sd(response, na.rm = T))



# Visualize item profile --------------------------------------------------

dat_acc_summary <- 
  left_join(dat_acc_summary,
            sett_quest$acc %>% 
              select(item_code, item_text_eng)) %>% 
  ## Keep original order of items
  mutate(item_text_eng = factor(item_text_eng, levels = unique(item_text_eng))) 

plot_item_profile_acceptance <- 
  plotItemProfile(dat_acc_summary,
                  col_name_items = "item_text_eng",
                  col_name_group = "group",
                  value_range = c(-2, 2))

plot(plot_item_profile_acceptance)



# Post-process item profile -----------------------------------------------

plot_item_profile_acceptance <- 
  plot_item_profile_acceptance +
  scale_x_discrete(limits = rev(unique(dat_acc_summary$item_text_eng)),
                   position = "top") +
  ggtitle(label = "Acceptance",
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

plot(plot_item_profile_acceptance)

ggsave(filename = "acceptance_item-profile.png", 
       plot = plot_item_profile_acceptance,
       path = "plots",
       width = 8,
       height = 5,
       units = "cm",
       dpi = 600)