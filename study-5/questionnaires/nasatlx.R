
# Settings ----------------------------------------------------------------

sett_analyse <- c()
sett_analyse$db_conn_name <- dbFindConnObj("Study-5", output = F)
sett_analyse$db_name_q <- "Questionnaires"
sett_analyse$db_conn_name_q <- dbFindConnObj(sett_analyse$db_name_q, output = F)



# Obtain additional information for questionnaire -------------------------

initQuestSettings("nasatlx", 
                  dbFindConnObj("Questionnaires"), 
                  "NASA-TLX_Hart_1988",
                  lang = "eng")

sett_q$nasatlx$item_col_names_reversed <- 
  c(T, T, F, T, T, F, T, F, T)



# Retrieve data -----------------------------------------------------------

dat_nasatlx_gestures_simple <- 
  dbReadTable(get(sett_analyse$db_conn_name), "t_q_nasatlx_gestures_simple")
dat_nasatlx_gestures_complex <- 
  dbReadTable(get(sett_analyse$db_conn_name), "t_q_nasatlx_gestures_complex")
dat_nasatlx_touch_simple <- 
  dbReadTable(get(sett_analyse$db_conn_name), "t_q_nasatlx_touch_simple")
dat_nasatlx_touch_complex <- 
  dbReadTable(get(sett_analyse$db_conn_name), "t_q_nasatlx_touch_complex")



# Row-bind data -----------------------------------------------------------

dat_nasatlx <- 
  rbind(data.frame(dat_nasatlx_gestures_simple, condition = "gestures_simple", stringsAsFactors = F),
        data.frame(dat_nasatlx_gestures_complex, condition = "gestures_complex", stringsAsFactors = F),
        data.frame(dat_nasatlx_touch_simple, condition = "touch_simple", stringsAsFactors = F),
        data.frame(dat_nasatlx_touch_complex, condition = "touch_complex", stringsAsFactors = F))

# 
# col_finder <- sett_q$nasatlx$item_col_names[sett_q$nasatlx$item_col_names_reversed]
# dat_nasatlx[, col_finder] <- dat_nasatlx[, col_finder] * -1


# Compute summary ---------------------------------------------------------

dat_nasatlx_summary <- 
  dat_nasatlx %>% 
  #select(-subject_id, -na_counter) %>% 
  select(-subject_id) %>% 
  group_by(condition) %>% 
  gather(key = "item_code", value = "response", -"condition") %>% 
  group_by(condition, item_code) %>% 
  summarise(response_mean = mean(response, na.rm = T),
            response_sd = sd(response, na.rm = T))



# Visualize item profile --------------------------------------------------

dat_nasatlx_summary <- 
  left_join(dat_nasatlx_summary,
            sett_q$nasatlx$db_data_lang %>% 
              mutate(item_code = paste_("nasatlx", sprintf("%02d", item_id))) %>% 
              select(item_code,
                     item_text)) %>% 
  ## Keep original order of items
  mutate(item_text = factor(item_text, levels = unique(item_text))) 


# Viz ---------------------------------------------------------------------



plot_item_profile <- 
  plotItemProfile(dat_nasatlx_summary,
                  col_name_items = "item_text",
                  col_name_group = "condition",
                  value_range = c(
                    sett_q$nasatlx$scale_values_min, 
                    sett_q$nasatlx$scale_values_max))

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