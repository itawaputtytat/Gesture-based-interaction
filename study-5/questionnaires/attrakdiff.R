
# Settings ----------------------------------------------------------------

sett_analyse <- c()
sett_analyse$db_name <- "GBI_Study-5"
sett_analyse$db_conn_name <- dbFindConnObj(sett_analyse$db_name, output = F)
sett_analyse$db_name_q <- "Questionnaires"
sett_analyse$db_conn_name_q <- dbFindConnObj(sett_analyse$db_name_q, output = F)

sett_quest <- c()
sett_quest$attrakdiff <- dbGetSrc(sett_analyse$db_conn_name_q, "AttrakDiff_Hassenzahl_2003")
sett_quest$attrakdiff$item_code <- sprintf("attrakdiff_%02d", sett_quest$attrakdiff$item_id)



# Retrieve data -----------------------------------------------------------

dat_attrakdiff_gesture <- dbReadTable(get(sett_analyse$db_conn_name), "t_q_attrakdiff_gestures")
dat_attrakdiff_touch <- dbReadTable(get(sett_analyse$db_conn_name), "t_q_attrakdiff_touch")



# Row-bind data -----------------------------------------------------------

dat_attrakdiff <- 
  rbind(
    data.frame(dat_attrakdiff_gesture, group = "Gesture", stringsAsFactors = F),
    data.frame(dat_attrakdiff_touch, group = "Touch", stringsAsFactors = F)
  )

col_finder <- sett_quest$attrakdiff$item_code[sett_quest$attrakdiff$is_reversed]
dat_attrakdiff[, col_finder] <- dat_attrakdiff[, col_finder] + 4
dat_attrakdiff[, col_finder] <- recodeItems(dat_attrakdiff[, col_finder], col_finder, 7)
dat_attrakdiff[, col_finder] <- dat_attrakdiff[, col_finder] - 4


# Compute summary ---------------------------------------------------------

dat_attrakdiff_summary <- 
  dat_attrakdiff %>% 
  select(-subject_id) %>% 
  group_by(group) %>% 
  gather(key = "item_code", value = "response", -"group") %>% 
  group_by(group, item_code) %>% 
  summarise(response_mean = mean(response, na.rm = T),
            response_sd = sd(response, na.rm = T))


# Visualize item profile --------------------------------------------------

dat_attrakdiff_summary <- 
  left_join(dat_attrakdiff_summary,
            sett_quest$attrakdiff %>% 
              select(item_code, item_text_eng)) %>% 
  ## Keep original order of items
  mutate(item_text_eng = factor(item_text_eng, levels = unique(item_text_eng))) 

plot_item_profile_attrakdiff <- 
  plotItemProfile(dat_attrakdiff_summary,
                  col_name_items = "item_text_eng",
                  col_name_group = "group",
                  value_range = c(-3, 3))

plot(plot_item_profile_attrakdiff)



# Vizualize matrix --------------------------------------------------------

dat_attrakdiff_summary_by_scale <- 
  dat_attrakdiff %>% 
  group_by(group) %>% 
  gather(key = "item_code", value = "response", -"subject_id", -"group") %>% 
  left_join(sett_quest$attrakdiff[, c("item_code", "subscale_name_eng_abbr")]) %>% 
  mutate(scale = substr(subscale_name_eng_abbr, 1, 2)) %>% 
  group_by_("group", "scale") %>%
  summarise(id_n = length(unique(subject_id)),
            score_mean = mean(response, na.rm = T),
            score_sd   = sd(response, na.rm = T)) %>%
  mutate(score_error = qnorm(0.975) * score_sd / sqrt(id_n)) %>% 
  gather("measure", "value", -"group", -"scale") %>% 
  mutate(measure_v2 = paste_(scale, measure)) %>% 
  select(group, measure_v2, value) %>% 
  spread(measure_v2, value)

source("resources/plotMatrix_attrakdiff_template.R")

plotdat_attrakdiff_matrix <- 
  plotMatrix_attrakdiff_template + 
  
  # Plot data for confidence intervals for each condition
  # geom_rect(data = dat_attrakdiff_summary_by_scale,
  #           aes_string(xmin = "pq_score_mean - pq_score_error",
  #                      xmax = "pq_score_mean + pq_score_error",
  #                      ymin = "hq_score_mean - hq_score_error",
  #                      ymax = "hq_score_mean + hq_score_error",
  #                      fill = "group",
  #                      colour = "group"),
  #           fill = colset4groupvar,
  #           size = 0.25,
  #           alpha = c(rep(0.25, length(colset4groupvar))) ) +
  geom_rect(data = dat_attrakdiff_summary_by_scale,
          aes_string(xmin = "PQ_score_mean - PQ_score_error",
                     xmax = "PQ_score_mean + PQ_score_error",
                     ymin = "HQ_score_mean - HQ_score_error",
                     ymax = "HQ_score_mean + HQ_score_error",
                     fill = "group",
                     colour = "group"),
          size = 0.25,
          alpha = c(rep(0.25, 2)) ) +
  
  # Plot mean scores for each condition
  geom_point(data = dat_attrakdiff_summary_by_scale, 
             aes_string(x = "PQ_score_mean",
                        y = "HQ_score_mean",
                        colour = "group",
                        shape = "group"),
             size = 1) +
  scale_fill_manual("Interaction type", values = c("firebrick3", "dodgerblue4")) + 
  scale_color_manual("Interaction type", values = c("firebrick3", "dodgerblue4")) + 
  scale_shape_manual("Interaction type", values = c(16, 17))
  # scale_colour_manual("Interaction type\n[97.5 % CI]",
  #                     values = c("firebrick3", "dodgerblue4"), 
  #                     labels = createLabels4PlotLegend(name4df, groupvar)) + 
  # scale_shape_manual("Interaction type\n[97.5 % CI]",
  #                    values = shapeset4groupvar, 
  #                    labels = createLabels4PlotLegend(name4df, groupvar))

plotdat_attrakdiff_matrix <- 
  plotdat_attrakdiff_matrix +
  ggtitle(label = "AttrakDiff",
          subtitle = "") + 
  labs(x = "Pragmatic Quality (PQ)",
       y = "Hedonic Quality (HQ)") +
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black")) + 
  theme(
    legend.position=c(0,1), 
    legend.justification=c(0.13, 0.2),
    legend.direction="horizontal",
    legend.background = element_blank()
  ) +
  theme(legend.key.size = unit(c(0.25), "cm"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 6, color = "black"))

ggsave(filename = "attrakdiff_matrix.png", 
       plot = plotdat_attrakdiff_matrix,
       path = "plots",
       width = 15 / 2,
       height = 5,
       units = "cm",
       dpi = 600)


# Post-process item profile -----------------------------------------------

plot_item_profile_attrakdiff_v2 <- 
  plot_item_profile_attrakdiff_v2 +
  #scale_y_continuous(breaks = c(-1, seq(0, 20, 5))) +
  ggtitle(label = "Attrak-Diff item profile",
          subtitle = "Comparison of Gesture vs. Touch") + 
  labs(x = NULL,
       y = "Response mean") +
  theme_bw() + 
  theme(#legend.justification = c(0, 0), 
    #legend.position = c(0.002, 0.002),
    legend.background = element_rect(color = "black",
                                     size = 0.2)) +
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
  theme(legend.key.size = unit(0.2, "cm"),
        legend.margin = margin(rep(0.1, 4), unit='cm'),
        legend.title = element_text(size = 6, color = "black"),
        legend.text = element_text(size = 6, color = "black"))

plot(plot_item_profile_attrakdiff)

ggsave(filename = "attrakdiff_item-profile.png", 
       plot = plot_item_profile_attrakdiff_v2,
       path = "plots",
       width = 14,
       height = 8,
       units = "cm",
       dpi = 600)


plot_item_profile_attrakdiff_v2 <- 
  plot_item_profile_attrakdiff  +
  
  geom_hline(aes(yintercept = 0),
             colour = "black",
             size = 0.5) + 
  
  geom_vline(aes(xintercept = 21.5),
             colour = "grey60",
             size = 0.5) +
  
  geom_vline(aes(xintercept = 14.5),
             colour = "grey60",
             size = 0.5) +
  
  geom_vline(aes(xintercept = 7.5),
             colour = "grey60",
             size = 0.5) +
  
  annotate("rect",
           xmin = 28+0.45,
           xmax = 28-6.45,
           ymin = -1,
           ymax = 0,
           fill = "green1",
           alpha = 1) +
  annotate("rect",
           xmin = 21+0.45,
           xmax = 21-6.45,
           ymin = -1,
           ymax = 0,
           fill = "deepskyblue1",
           alpha = 1) +
  annotate("rect",
           xmin = 14+0.45,
           xmax = 14-6.45,
           ymin = -1,
           ymax = 0,
           fill = "cadetblue2",
           alpha = 1) +
  annotate("rect",
           xmin = 7+0.45,
           xmax = 7-6-0.45,
           ymin = -1,
           ymax = 0,
           fill = "yellow1",
           alpha = 1) +
  
  geom_text(aes(x = 28-0.5, y = -0.75), 
            label = "PQ", fontface = "bold", hjust = 0, vjust = 0, angle = 0) +
  geom_text(aes(x = 21-0.5, y = -0.75), 
            label = "HQI", fontface = "bold", hjust = 0, vjust = 0.6, angle = 0) +
  geom_text(aes(x = 14-0.5, y = -0.75), 
            label = "HQS", fontface = "bold", hjust = 0, vjust = 0.6, angle = 0) +
  geom_text(aes(x = 7-0.5, y = -0.75), 
            label = "ATT", fontface = "bold", hjust = 0, vjust = 0, angle = 0)
