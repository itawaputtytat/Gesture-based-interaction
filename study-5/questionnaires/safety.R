
# Settings ----------------------------------------------------------------

sett_analyse <- c()
sett_analyse$db_name <- "GBI_Study-5"
sett_analyse$db_conn_name <- dbFindConnObj(sett_analyse$db_name, output = F)
sett_analyse$db_name_q <- "Questionnaires"
sett_analyse$db_conn_name_q <- dbFindConnObj(sett_analyse$db_name_q, output = F)

sett_quest <- c()
sett_quest$safety <- 
  dbGetSrc(sett_analyse$db_conn_name_q, 
           "Susceptibility-to-driver-distraction_Feng_2014_DE") %>% 
  arrange(item_id)
sett_quest$safety$item_code <- sprintf("safety_%02d", sett_quest$safety$item_id)



# Retrieve data -----------------------------------------------------------

dat_safety <- dbReadTable(get(sett_analyse$db_conn_name), "t_q_safety")



# Row-bind data -----------------------------------------------------------

dat_safety <- 
  cbind(dat_safety, group = "Overall")



# Compute summary ---------------------------------------------------------

dat_safety_summary <- 
  dat_safety %>% 
  select(-subject_id) %>% 
  group_by(group) %>% 
  gather(key = "item_code", value = "response", -"group") %>% 
  group_by(group, item_code) %>% 
  summarise(response_mean = mean(response, na.rm = T),
            response_sd = sd(response, na.rm = T))



# Visualize item profile --------------------------------------------------

dat_safety_summary <- 
  left_join(dat_safety_summary,
            sett_quest$safety %>% 
              select(item_code, item_text_eng)) %>% 
  ## Keep original order of items
  mutate(item_text_eng = factor(item_text_eng, levels = sett_quest$safety$item_text_eng)) %>% 
  mutate(item_text_eng = breakStringToLines(item_text_eng, max_nchar = 35)) #max_nlines = 2)) %>% 
  #mutate(item_text_eng = gsub("_", "\n", item_text_eng) )
  # mutate(item_text_eng = gsub("_", " ", item_text_eng) )

plot_item_profile_safety <- 
  plotItemProfile(dat_safety_summary,
                  col_name_items = "item_text_eng",
                  col_name_group = "group",
                  value_range = c(1, 5))

plot(plot_item_profile_safety)



# Post-process item profile -----------------------------------------------

plot_item_profile_safety <- 
  plot_item_profile_safety +
  scale_x_discrete(limits = rev(dat_safety_summary$item_text_eng),
                   position = "top") +
  scale_y_continuous(breaks = c(1:5)) +
  ggtitle(label = "Impression of driving safely (Distraction engagement)",
          subtitle = "I have the impression of driving safely when ...") + 
  labs(x = NULL,
       y = "Response mean") +
  theme_bw() + 
  theme(#legend.justification = c(0, 0), 
    #legend.position = c(0.002, 0.002),
    legend.background = element_rect(color = "black",
                                     size = 0.2)) +
  scale_color_manual(name = "Interaction type",
                     values = c("black", "dodgerblue4")) + 
  scale_linetype_manual(name = "Interaction type",
                        values = c("solid", "dashed")) +
  scale_shape_manual(name = "Interaction type",
                     values = c(20, 20)) +
  scale_size_manual(name = "Interaction type",
                    values = rep(0.35, 3)) + 
  scale_alpha_manual(name = "Interaction type",
                     values = rep(0.35, 3)) + 
  guides(color = F, linetype = F, shape = F, size = F, alpha = F) +
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black")) + 
  theme(legend.key.size = unit(0.2, "cm"),
        legend.margin = margin(rep(0.1, 4), unit='cm'),
        legend.title = element_text(size = 6, color = "black"),
        legend.text = element_text(size = 6, color = "black"))

plot(plot_item_profile_safety)

ggsave(filename = "safety_item-profile.png", 
       plot = plot_item_profile_safety,
       path = "plots",
       width = 8,
       height = 6,
       units = "cm",
       dpi = 600)
