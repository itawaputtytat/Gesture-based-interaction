
# Settings ----------------------------------------------------------------

sett_analyse <- c()
sett_analyse$db_name <- "GBI_Study-5"
sett_analyse$db_conn_name <- dbFindConnObj(sett_analyse$db_name, output = F)
sett_analyse$db_name_q <- "Questionnaires"
sett_analyse$db_conn_name_q <- dbFindConnObj(sett_analyse$db_name_q, output = F)

initQuestSettings("ssq", 
                  dbFindConnObj("Questionnaires"), 
                  "SSQ_Kennedy_1993", 
                  sett_name = "sett_quest",
                  max_nchar = 99)


# Retrieve data -----------------------------------------------------------

dat_ssq_t1 <- 
  dbGetSrc(sett_analyse$db_conn_name, 
           "t_q_ssq_t1",
           add_col_db_src_name = T)

dat_ssq_t2 <- 
  dbGetSrc(sett_analyse$db_conn_name, 
           "t_q_ssq_t2",
           add_col_db_src_name = T)



# Remove cases with no SSQ data -------------------------------------------

## By counting missing values in each row
dat_ssq_t1 <- 
  dat_ssq_t1 %>%
  mutate(na_counter = rowSums(is.na(.[sett_quest$ssq$col_names]))) %>% 
  filter(na_counter != 16)

dat_ssq_t2 <- 
  dat_ssq_t2 %>%
  mutate(na_counter = rowSums(is.na(.[sett_quest$ssq$col_names]))) %>% 
  filter(na_counter != 16)



# Replace missing values in single values ---------------------------------

## Initialise parameters (without) imputating
## Select only necessary variables
imp_model_init = mice(dat_ssq_t1, maxit = 0)
imp_model_init$predictorMatrix[, "subject_id"] <- 0

imp_model_t1 <- 
  mice(dat_ssq_t1, 
       m = 50, 
       seed = 42, 
       method = "pmm", 
       predictorMatrix = imp_model_init$predictorMatrix,
       printFlag = F)

dat_ssq_t1_imp <- mice::complete(imp_model_t1)



# Row-bind data -----------------------------------------------------------

dat_ssq_t1_t2 <- 
  rbind(data.frame(dat_ssq_t1_imp, stringsAsFactors = F),
        data.frame(dat_ssq_t2, stringsAsFactors = F))



# Exclude subjects --------------------------------------------------------

## Two participants reported sickness symptomes prior to the test phase
dat_ssq_t1_t2 <-
  dat_ssq_t1_t2 %>%
  filter(!subject_id %in% c(550, 509))



# Compute summary ---------------------------------------------------------

## Reshape data to long format
col_finder <- setdiff(names(dat_ssq_t1_t2), sett_quest$ssq$col_names)
dat_ssq_t1_t2_long <- 
  reshapeLong(dat_ssq_t1_t2, 
              col_names_to_keep = col_finder)

## Compute summary                
col_finder <- c("db_src_name", "col_name")
dat_ssq_t1_t2_long_summary <- 
  computeSummary(dat_ssq_t1_t2_long,
                 col_names_group = col_finder)

## Add item texts
dat_ssq_t1_t2_long_summary <- 
  left_join(dat_ssq_t1_t2_long_summary,
            sett_quest$ssq$item_texts) %>% 
  mutate(item_text = 
           factor(item_text, 
                  level = sett_quest$ssq$item_texts$item_text)) %>% 
  mutate(db_src_name = 
           factor(db_src_name, 
                  levels = unique(dat_ssq_t1_t2_long_summary$db_src_name),
                  labels = c("T1", "T2"))) 



# Visualize item profile --------------------------------------------------

plot_item_profile_ssq <- 
  plotItemProfile(dat_ssq_t1_t2_long_summary,
                  col_name_items = "item_text",
                  col_name_group = "db_src_name",
                  value_range = c(0, 3))

plot(plot_item_profile_ssq)



# Add data from Karl et al. (2013) ----------------------------------------

## Add data from Karl et al. (2013)
dat_ssq_items_summary_Karl_2013 <- 
  dbGetSrc(db_conn_4, 
           "t_q_ssq_Karl_2013_summary",
           add_col_db_src_name = T)

dat_ssq_items_summary_Karl_2013 <- 
  dat_ssq_items_summary_Karl_2013 %>% 
  select(db_src_name,
         col_name = item_code,
         mean = complex_mean,
         sd = complex_sd) %>% 
  left_join(sett_quest$ssq$item_texts) %>% 
  mutate(item_text = 
           factor(item_text, 
                  level = sett_quest$ssq$item_texts$item_text))

dat_ssq_items_summary_Karl_2013_GBI <- 
  bind_rows(dat_ssq_items_summary_Karl_2013,
            dat_ssq_t1_t2_long_summary %>% 
              select(db_src_name,
                     col_name,
                     mean,
                     sd,
                     item_text)) %>% 
  mutate(db_src_name =
           factor(db_src_name,
                  levels = c("T1", "T2", "t_q_ssq_Karl_2013_summary"),
                  labels = c("T1", "T2", "Karl et al. (2013)")))



# Visualize item profile with data from Karl et al. (2013) ----------------

plot_item_profile_Karl_2013 <- 
  plotItemProfile(dat_ssq_items_summary_Karl_2013_GBI,
                  col_name_values = "mean",
                  col_name_items = "item_text",
                  col_name_group = "db_src_name",
                  value_range = c(0, 3))

plot(plot_item_profile_Karl_2013)



# Postprocess item profile for printing -----------------------------------

plot_item_profile_Karl_2013_post <- 
  plot_item_profile_Karl_2013 + 
  ggtitle(label = "Mean responses for SSQ",
          subtitle = "Comparison of T1, T2, and Karl et al. (2013)") + 
  labs(x = NULL,
       y = "Response mean") +
  theme_bw() + 
  theme(legend.justification = c(1, 1), 
        legend.position = c(0.999, 0.999),
        legend.background = element_rect(color = "black",
                                         size = 0.2)) +
  scale_color_manual(name = "Data Source",
                     values = c("green3", "red2", "grey50")) + 
  scale_linetype_manual(name = "Data Source",
                     values = c("solid", "longdash", "dashed")) +
  scale_shape_manual(name = "Data Source",
                     values = c(NA, NA, NA)) +
  scale_size_manual(name = "Data Source",
                    values = rep(0.35, 3)) + 
  scale_alpha_manual(name = "Data Source",
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

plot(plot_item_profile_Karl_2013_post)

ggsave(filename = "ssq_item-profile.png", 
       plot = plot_item_profile_Karl_2013_post,
       path = "plots",
       width = 7.9,
       #height = 5.3,
       height = 6,
       units = "cm",
       dpi = 600)



# Compute scores ----------------------------------------------------------

# ## Get unique scales
# sett_analyse$subscales <- strsplit(sett_quest$ssq$db_data_lang$subscale_name, split = ";")
# sett_analyse$subscales <- unlist(sett_analyse$subscales)
# sett_analyse$subscales <- sub(" ", "", sett_analyse$subscales)
# sett_analyse$subscales <- tolower(unique(sett_analyse$subscales))

#invisible(
# lapply(sett_analyse$subscales, function(subscale) {
#   col_finder <- grep(subscale, tolower(sett_quest$ssq$db_data_lang$subscale_name))
#   col_finder <- sprintf("ssq_%02d", col_finder)
#   print(col_finder)
#   dat_ssq_t1_t2 <<-
#     computeScores(dat_ssq_t1_t2, col_finder, fun = "sum")
#     #computeScores(dat_ssq_t1_t2, col_finder, col_name_prefix = subscale, fun = "sum")
# })#)
col_finder <- grep("Nausea", sett_quest$ssq$db_data_lang$subscale_name)
col_finder <- sett_quest$ssq$col_names[col_finder]
dat_ssq_t1_t2$nausea <- rowSums(dat_ssq_t1_t2[, col_finder])
col_finder <- grep("Oculomotor", sett_quest$ssq$db_data_lang$subscale_name)
col_finder <- sett_quest$ssq$col_names[col_finder]
dat_ssq_t1_t2$oculomotor <- rowSums(dat_ssq_t1_t2[, col_finder])
col_finder <- grep("Disorientation", sett_quest$ssq$db_data_lang$subscale_name)
col_finder <- sett_quest$ssq$col_names[col_finder]
dat_ssq_t1_t2$disorientation <- rowSums(dat_ssq_t1_t2[, col_finder])

## Weight scores according to Kennedy (1991)
dat_ssq_t1_t2$n_weighted <- dat_ssq_t1_t2$nausea * 9.54
dat_ssq_t1_t2$o_weighted <- dat_ssq_t1_t2$oculomotor * 7.58
dat_ssq_t1_t2$d_weighted <- dat_ssq_t1_t2$disorientation * 13.92


## Compute total severity
sett_analyse$subscales <- grep("weighted", names(dat_ssq_t1_t2), value = T)
#sett_analyse$subscales <- grep("!outlier", sett_analyse$subscales, value = T)
dat_ssq_t1_t2$ts <- rowSums(dat_ssq_t1_t2[, sett_analyse$subscales]) * 3.74




# Code outliers -----------------------------------------------------------

dat_ssq_t1_t2 <- 
  dat_ssq_t1_t2 %>% 
  group_by(db_src_name) %>% 
  mutate(n_weighted_is_outlier = codeOutliersZ(n_weighted),
         o_weighted_is_outlier = codeOutliersZ(o_weighted),
         d_weighted_is_outlier = codeOutliersZ(d_weighted),
         ts_is_outlier = codeOutliersZ(ts))



# Reshape data to long format ---------------------------------------------

# sett_analyse$subscales_weighted <- 
#   paste(substring(sett_analyse$subscales, 1, 1), "weighted", sep = "_")
sett_analyse$subscales_weighted <- 
  grep("weighted", names(dat_ssq_t1_t2), value = T)

sett_analyse$subscales_weighted <- 
  grep("outlier", sett_analyse$subscales_weighted, value = T, invert = T)

dat_ssq_t1_t2_scores_long <- 
  gather(dat_ssq_t1_t2 %>% 
           select_(.dots = c("subject_id", 
                             "db_src_name", 
                             sett_analyse$subscales_weighted, "ts")), 
         key = "subscale", value = "score", 
         c(sett_analyse$subscales_weighted, "ts")) %>% 
  group_by(db_src_name, subscale) %>% 
  mutate(is_outlier_z = codeOutliersZ(score)) %>% 
  ungroup() %>% 
  mutate(db_src_name = 
           factor(db_src_name, 
                  levels = c("t_q_ssq_t1", "t_q_ssq_t2"), 
                  labels = c("T1", "T2"))) %>% 
  mutate(subscale =
           factor(subscale, 
                  levels = c("n_weighted",
                             "o_weighted",
                             "d_weighted",
                             "ts"),
                  labels = c("Nausea",
                             "Oculomotor\ndisturbances",
                             "Disorientation",
                             "Total severity")))

# Compute summary for scores ----------------------------------------------

dat_ssq_t1_t2_scores_summary <- 
  dat_ssq_t1_t2_scores_long %>% 
  #filter(!is_outlier_z) %>% 
  group_by(db_src_name, subscale) %>% 
  summarise(score_mean = mean(score, na.rm = T),
            score_sd = sd(score, na.rm = T),
            score_median = median(score, na.rm = T))



# Visualize scores using boxplots -----------------------------------------

plot_ssq_scores <- 
  ggplot() + 
  geom_boxplot(data = dat_ssq_t1_t2_scores_long %>% 
                 filter(subscale != "Total severity"),
               aes_string(x = "db_src_name",
                          y = "score",
                          #color = "subscale",
                          fill = "subscale"),
               alpha = 0.5,
               size = 0.2,
               fatten = 2.5,
               outlier.colour = NA) + 
  # geom_hline(data = dat_ssq_t1_t2_scores_summary %>% 
  #              filter(subscale != "Total severity"),
  #            aes_string(yintercept = "score_mean",
  #                       group = "subscale",
  #                       color = "subscale"),
  #            size = 0.35,
  #            alpha = 0.) +
  geom_hline(data = dat_ssq_t1_t2_scores_summary %>% 
               filter(subscale != "Total severity"),
             aes_string(yintercept = "score_mean",
                        group = "subscale",
                        color = "subscale",
                        linetype = "db_src_name"),
             size = 0.35) +
  # stat_boxplot(data = dat_ssq_t1_t2_scores_long,
  #              aes_string(x = "tx",
  #                         y = "score",
  #                         olor = "subscale"),
  #              geom ='errorbar',
  #              lwd = 0.35) + 
  geom_point(data = dat_ssq_t1_t2_scores_long %>% 
               filter(subscale != "Total severity"),
               aes_string(x = "db_src_name",
                          y = "score",
                          #shape = "is_outlier_z",
                          color = "subscale"),
             shape = 16,
             alpha = 0.5,
             size = 0.35,
             position = position_jitter(w = 0.1, h = 0)) + 
  facet_grid(.~subscale) +
  coord_cartesian(ylim = c(0, 100))

plot(plot_ssq_scores)



# Postprocess boxplots ----------------------------------------------------

sett_plot <- c()
sett_plot$legend_text_size <- 6
sett_plot$legend_text_color <- "black"

plot_ssq_scores_post <- 
  plot_ssq_scores + 
  scale_color_manual(values = c("green3", "orange2", "steelblue3", "red3")) +
  scale_fill_manual(values = c("green3", "orange2", "steelblue3", "red3")) +
  scale_linetype_manual(name = "Score means: ", values = c("dashed", "solid")) +
  ggtitle(label = "SSQ Scores") +
  labs(y = "Score") + 
  guides(fill = FALSE, color = FALSE) +
  # guides(fill = F, shape = F,
  #        color = guide_legend(override.aes=list(shape=NA))) +
  theme_bw() + 
  # theme(legend.position="top",
  #       legend.background = element_rect(color = "black",
  #                                        size = 0.2)) +
  theme(legend.justification = c(0, 1),
        legend.position = c(0.001, 0.999),
        legend.background = element_rect(color = "black",
                                         size = 0.2)) +
  # theme(legend.justification = c(0, 1), 
  #       legend.position = "top",
  #       legend.direction = "horizontal",
  #       legend.margin = margin(c(-0.1, 0, -0.25, 0.01), unit = 'cm')) +
  theme(legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.25, "cm"),
        legend.key = element_blank(),
        legend.title = element_text(size = sett_plot$legend_text_size, 
                                    color = sett_plot$legend_text_color),
        #legend.title = element_blank(),
        legend.text = element_text(size = sett_plot$legend_text_size, 
                                   color = sett_plot$legend_text_color),
        legend.background = element_blank()) +
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black")) + 
  theme(legend.position = "top",
        legend.background = element_rect(color = NA),
        legend.key.height = unit(-0.2, "cm"),
        legend.key.width = unit(0.39, "cm"),
        #legend.margin = margin(c(0.1, 0.1, 0.1, 0.1), unit='cm'),
        legend.margin = margin(c(0.07, 0, -0.15, 0), unit='cm'),
        legend.title = element_text(size = 6, color = "black"),
        legend.text = element_text(size = 6, color = "black", face = "bold")) +
  theme(strip.text.x = element_text(size = 6, face = "bold")) 
  
plot(plot_ssq_scores_post)

ggsave(filename = "ssq_boxplots.png", 
       plot = plot_ssq_scores_post,
       path = "plots",
       width = 7.9,
       #height = 5.3,
       height = 6,
       units = "cm",
       dpi = 600)




# Reliability -------------------------------------------------------------

library(psych)

dat_temp <- 
  dat_ssq_t1_t2 %>% 
  filter(db_src_name == "t_q_ssq_t1")
psych::alpha(dat_temp[, sprintf("ssq_%02d", 1:16)], check.keys = F)

dat_temp <- 
  dat_ssq_t1_t2 %>% 
  filter(db_src_name == "t_q_ssq_t2")
psych::alpha(dat_temp[, sprintf("ssq_%02d", 1:16)], check.keys = F)

subscales_temp <- c("Nausea", "Oculomotor", "Disorientation")

lapply(subscales_temp, function(subscale) {
  col_finder <- grep(subscale, sett_quest$ssq$db_data_lang$subscale_name)
  col_finder <- sprintf("ssq_%02d", col_finder)
  print(col_finder)
  dat_temp <- 
    dat_ssq_t1_t2 %>% 
    filter(db_src_name == "t_q_ssq_t1")
  psych::alpha(dat_temp[, col_finder], check.keys = F)
})

lapply(subscales_temp, function(subscale) {
  col_finder <- grep(subscale, sett_quest$ssq$db_data_lang$subscale_name)
  col_finder <- sprintf("ssq_%02d", col_finder)
  dat_temp <- 
    dat_ssq_t1_t2 %>% 
    filter(db_src_name == "t_q_ssq_t2")
  psych::alpha(dat_temp[, col_finder], check.keys = F)
})




# Inference statistics ----------------------------------------------------

col_finder <- 
  !names(dat_ssq_t1_t2) %in% c(sett_quest$ssq$item_code, "na_counter")
col_finder <- names(dat_ssq_t1_t2)[col_finder]

dat_ssq_t1_t2_wide <- 
  dat_ssq_t1_t2 %>% 
  select_(.dots = col_finder) %>% 
  gather(key = "col_name", value = "value", -"subject_id", -"db_src_name") %>% 
  mutate(col_name_new = paste(db_src_name, col_name, sep = "_")) %>% 
  ungroup() %>% 
  select(-c(db_src_name, col_name)) %>% 
  spread(key = col_name_new, value)




# Effect size function ----------------------------------------------------

rFromWilcox <- function(wilcoxModel, N) {
  z <- qnorm(wilcoxModel$p.value/2)
  r <- z/sqrt(N)
  cat(wilcoxModel$data.name, "Effect Size, r = ", r)
}



# Nausea ------------------------------------------------------------------

dat_ssq_t1_t2_wide_complete <- dat_ssq_t1_t2_wide

# dat_ssq_t1_t2_wide_complete <- 
#   dat_ssq_t1_t2_wide_complete %>% 
#   filter(!t1_n_weighted_is_outlier,
#          !t2_n_weighted_is_outlier)

dat_ssq_t1_t2_wide_complete <- 
  dat_ssq_t1_t2_wide_complete[complete.cases(dat_ssq_t1_t2_wide_complete), ]

# t.test(dat_ssq_t1_t2_wide_complete$t1_n_weighted,
#        dat_ssq_t1_t2_wide_complete$t2_n_weighted,
#        paired = T)

wilcox_model_nausea <-
  wilcox.test(dat_ssq_t1_t2_wide_complete$t_q_ssq_t1_n_weighted, 
              dat_ssq_t1_t2_wide_complete$t_q_ssq_t2_n_weighted, 
              paired = T,
              correct = F) 

print(wilcox_model_nausea)

print(rFromWilcox(wilcox_model_nausea, nrow(dat_ssq_t1_t2_wide_complete)))

mean(dat_ssq_t1_t2_wide_complete$t_q_ssq_t1_n_weighted)
mean(dat_ssq_t1_t2_wide_complete$t_q_ssq_t2_n_weighted)
sd(dat_ssq_t1_t2_wide_complete$t_q_ssq_t1_n_weighted)
sd(dat_ssq_t1_t2_wide_complete$t_q_ssq_t2_n_weighted)



# Oculomotor --------------------------------------------------------------


dat_ssq_t1_t2_wide_complete <- dat_ssq_t1_t2_wide

# dat_ssq_t1_t2_wide_complete <-
#   dat_ssq_t1_t2_wide_complete %>%
#   filter(!t1_o_weighted_is_outlier,
#          !t2_o_weighted_is_outlier)

dat_ssq_t1_t2_wide_complete <- 
  dat_ssq_t1_t2_wide_complete[complete.cases(dat_ssq_t1_t2_wide_complete), ] %>% 
  data.frame()

# t.test(dat_ssq_t1_t2_wide_complete$t1_o_weighted,
#        dat_ssq_t1_t2_wide_complete$t2_o_weighted,
#        paired = T)

print(wilcox_model_oculomotor <- 
  wilcox.test(dat_ssq_t1_t2_wide_complete$t_q_ssq_t1_o_weighted, 
            dat_ssq_t1_t2_wide_complete$t_q_ssq_t2_o_weighted, 
            paired = T,
            correct = F))

print(rFromWilcox(wilcox_model_oculomotor, nrow(dat_ssq_t1_t2_wide_complete)))

mean(dat_ssq_t1_t2_wide_complete$t_q_ssq_t1_o_weighted)
mean(dat_ssq_t1_t2_wide_complete$t_q_ssq_t2_o_weighted)
sd(dat_ssq_t1_t2_wide_complete$t_q_ssq_t1_o_weighted)
sd(dat_ssq_t1_t2_wide_complete$t_q_ssq_t2_o_weighted)



# Desorientation ----------------------------------------------------------

dat_ssq_t1_t2_wide_complete <- dat_ssq_t1_t2_wide

# dat_ssq_t1_t2_wide_complete <- 
#   dat_ssq_t1_t2_wide_complete %>% 
#   filter(!t1_d_weighted_is_outlier,
#          !t2_d_weighted_is_outlier)

dat_ssq_t1_t2_wide_complete <- 
  dat_ssq_t1_t2_wide_complete[complete.cases(dat_ssq_t1_t2_wide_complete), ]

# t.test(dat_ssq_t1_t2_wide_complete %>% pull(t1_d_weighted),
#        dat_ssq_t1_t2_wide_complete %>% pull(t2_d_weighted),
#        paired = T)

print(wilcox_model_disorientation <- 
  wilcox.test(dat_ssq_t1_t2_wide_complete$t_q_ssq_t1_d_weighted, 
              dat_ssq_t1_t2_wide_complete$t_q_ssq_t2_d_weighted, 
              paired = T,
              correct = F))

print(rFromWilcox(wilcox_model_disorientation, nrow(dat_ssq_t1_t2_wide_complete)))

mean(dat_ssq_t1_t2_wide_complete$t_q_ssq_t1_d_weighted)
mean(dat_ssq_t1_t2_wide_complete$t_q_ssq_t2_d_weighted)
sd(dat_ssq_t1_t2_wide_complete$t_q_ssq_t1_d_weighted)
sd(dat_ssq_t1_t2_wide_complete$t_q_ssq_t2_d_weighted)


# Total severity ----------------------------------------------------------

dat_ssq_t1_t2_wide_complete <- dat_ssq_t1_t2_wide

# dat_ssq_t1_t2_wide_complete <- 
#   dat_ssq_t1_t2_wide_complete %>% 
#   filter(!t1_ts_is_outlier,
#          !t2_ts_is_outlier)

dat_ssq_t1_t2_wide_complete <- 
  dat_ssq_t1_t2_wide_complete[complete.cases(dat_ssq_t1_t2_wide_complete), ]

t.test(dat_ssq_t1_t2_wide_complete %>% pull(t_q_ssq_t1_ts),
       dat_ssq_t1_t2_wide_complete %>% pull(t_q_ssq_t2_ts),
       paired = T)


print(wilcox_model_ts <- 
  wilcox.test(dat_ssq_t1_t2_wide_complete$t_q_ssq_t1_ts, 
              dat_ssq_t1_t2_wide_complete$t_q_ssq_t2_ts, 
              paired = T,
              correct = F))

print(rFromWilcox(wilcox_model_ts, nrow(dat_ssq_t1_t2_wide_complete)))

mean(dat_ssq_t1_t2_wide_complete$t_q_ssq_t1_ts)
mean(dat_ssq_t1_t2_wide_complete$t_q_ssq_t2_ts)
sd(dat_ssq_t1_t2_wide_complete$t_q_ssq_t1_ts)
sd(dat_ssq_t1_t2_wide_complete$t_q_ssq_t2_ts)






# fsdf+ -------------------------------------------------------------------




# ## Get range of values
# sett_plot <- c()
# sett_plot$y_range <- sett_q$scale[1]
# sett_plot$y_range <- strsplit(sett_plot$y_range, split = ",")
# sett_plot$y_range <- unlist(sett_plot$y_range)
# sett_plot$y_range <- as.numeric(sett_plot$y_range)
# sett_plot$y_range <- range(sett_plot$y_range)