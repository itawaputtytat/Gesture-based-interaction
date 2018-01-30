
# Settings ----------------------------------------------------------------

sett_analyse <- c()
sett_analyse$db_name <- "GBI_Study-5"
sett_analyse$db_conn_name <- dbFindConnObj(sett_analyse$db_name, output = F)
sett_analyse$db_name_q <- "Questionnaires"
sett_analyse$db_conn_name_q <- dbFindConnObj(sett_analyse$db_name_q, output = F)

initQuestSettings("evaluation", 
                  sett_analyse$db_conn_name_q,
                  "HMI-evaluation_Graichen_2015",
                  "sett_quest",
                  max_nchar = 45)



# Retrieve data -----------------------------------------------------------

dat_evaluation_gesture <- 
  dbGetSrc(sett_analyse$db_conn_name, 
           "t_q_evaluation_gestures",
           add_col_db_src_name = T)

dat_evaluation_touch <- 
  dbGetSrc(sett_analyse$db_conn_name, 
           "t_q_evaluation_touch",
           add_col_db_src_name = T)



# Data manipulation -------------------------------------------------------

## Row-bind data
dat_evaluation <- 
  bind_rows(dat_evaluation_gesture, 
            dat_evaluation_touch)



# Compute summary ---------------------------------------------------------

## Reshape data to long format
col_finder <- setdiff(names(dat_evaluation), sett_quest$evaluation$col_names)
dat_evaluation_long <- 
  reshapeLong(dat_evaluation, 
              col_names_to_keep = col_finder)

## Compute summary                
col_finder <- c("db_src_name", "col_name")
dat_evaluation_long_summary <- 
  computeSummary(dat_evaluation_long,
                 col_names_group = col_finder)

## Add item texts
dat_evaluation_long_summary <- 
  left_join(dat_evaluation_long_summary,
            sett_quest$evaluation$item_texts_with_line_breaks) %>% 
  mutate(db_src_name = 
           factor(db_src_name, 
                  levels = unique(dat_evaluation_long_summary$db_src_name),
                  labels = c("Gesture", "Touch")))



# Visualize item profile --------------------------------------------------

plot_item_profile_evaluation <- 
  plotItemProfile(dat_evaluation_long_summary,
                  col_name_items = "item_text",
                  col_name_group = "db_src_name",
                  value_range = c(1, 5))

plot(plot_item_profile_evaluation)



# Post-process item profile -----------------------------------------------

sett_plot <- c()
sett_plot$title <- "System evaluation"
sett_plot$title_size <- 7
sett_plot$title_face <- "bold"
sett_plot$axis_x_title <- NULL
sett_plot$axis_y_title <- "Response mean"
sett_plot$axis_title_size <- 6
sett_plot$axis_text_size <- 6
sett_plot$axis_text_color <- "black"
sett_plot$legend_title <- "Interaction type:"
sett_plot$legend_text_size <- 6
sett_plot$legend_text_color <- "black"


plot_item_profile_evaluation <- 
  plot_item_profile_evaluation +
  scale_x_discrete(position = "top") +
  scale_color_manual(name = sett_plot$legend_title,
                     values = c("firebrick3", "dodgerblue4")) + 
  scale_linetype_manual(name = sett_plot$legend_title,
                        values = c("solid", "dashed")) +
  scale_shape_manual(name = sett_plot$legend_title,
                     values = c(20, 20)) +
  scale_size_manual(name = sett_plot$legend_title,
                    values = rep(0.35, 3)) + 
  scale_alpha_manual(name = sett_plot$legend_title,
                     values = rep(0.35, 3)) +

  ggtitle(label = sett_plot$title) + 
  labs(x = sett_plot$axis_x_title,
       y = sett_plot$axis_y_title) +
  theme_bw() + 
  theme(title = element_text(size = sett_plot$title_size, 
                             face = sett_plot$title_face )) + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = sett_plot$axis_title_size)) + 
  theme(axis.text.x = element_text(size = sett_plot$axis_text_size, 
                                   color = sett_plot$axis_text_color),
        axis.text.y = element_text(size = sett_plot$axis_text_size, 
                                   color = sett_plot$axis_text_color)) + 
  theme(legend.justification = c(0, 1), 
        legend.position = "top",
        legend.direction = "horizontal",
        legend.margin = margin(c(-0.1, 0, -0.25, 0.35), unit = 'cm')) +
  theme(legend.key.size = unit(0.25, "cm"),
        legend.key = element_blank(),
        legend.title = element_text(size = sett_plot$legend_text_size, 
                                    color = sett_plot$legend_text_color),
        #legend.title = element_blank(),
        legend.text = element_text(size = sett_plot$legend_text_size, 
                                   color = sett_plot$legend_text_color),
        legend.background = element_blank())

ggsave(filename = "test.png", 
       plot = plot_item_profile_evaluation,
       path = "plots",
       width = 8,
       height = 5,
       units = "cm",
       dpi = 600)



plot(plot_item_profile_evaluation)

ggsave(filename = "evaluation_item-profile.png", 
       plot = plot_item_profile_evaluation,
       path = "plots",
       width = 8,
       height = 5,
       units = "cm",
       dpi = 600)
