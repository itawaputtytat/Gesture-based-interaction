
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)

sett_proc <- c()
#sett_proc$df_name <- "dat_study5_t_adtf_sxx_exx_exx_full_tti_rnd1_intrpld"
sett_proc$df_name <- "dat_study5_t_adtf_sxx_exx_exx_full_intrpld"
#sett_proc$col_name_am <- "sxx_exx_dti_m_rnd1"
#sett_proc$col_name_am <- "sxx_exx_tti_s_rnd1"
sett_proc$col_name_am <- "sxx_exx_tti_s"
sett_proc$col_name_am <- "sxx_exx_dti_m"
sett_proc$col_name_dti <- "sxx_exx_dti_m"
sett_proc$col_name_am_start <- paste0(sett_proc$col_name_am, "_start")
sett_proc$col_name_am_end <- paste0(sett_proc$col_name_am, "_end")
sett_proc$col_name_id    <- "case"
sett_proc$colname_group <- "interaction_type"
sett_proc$crit_am <- 0
sett_proc$crit_am_last_acc_before_braking <- 0
sett_proc$plot <- F

sett_plot <- c()
sett_plot$xlim_min <- sett_query$am_limit1
sett_plot$xlim_max <- sett_query$am_limit2
sett_plot$fill <- c("red2", "white", "green2")



# Code pedal activity -----------------------------------------------------

col_finder <- 
  paste0("condition_scenario_t", 
         substr(sett_query$sxx_exx, start = 3, 3))

dat_pedal <- 
  get(sett_proc$df_name) %>% 
  filter(is_usable) %>% 
  data.frame()#%>% 
  #mutate(condition_scenario = col_finder)

unique_cases <- unique(dat_pedal$case)

codePedalActivity(dat_pedal,
                  colname_acc_pedal_pos = "acc_pedal_pos_perc",
                  colname_brake_status = "brake_pressure_status",
                  colname_brake_press = "brake_pressure_bar")



# Summarise pedal activity ------------------------------------------------

dat_pedal_summary <- 
  dat_pedal %>% 
  ## Reduce pedal activity to starting arrival measure
  group_by_("sxx_exx",
            sett_proc$col_name_id,
            sett_proc$colname_group,
            "pedal_act_id",
            "subject_id") %>%
  summarise_(.dots = c(
    setNames(list(interp(~ min(var), var = as.name("tta_s")) ), 
             "tta_s_min"),
    setNames(list(interp(~ min(var), var = as.name("time_s")) ), 
             "time_s_min"),
    setNames(list(interp(~ min(var), var = as.name("pedal_act")) ), 
             "pedal_act"),
    setNames(list(interp(~ min(var), 
                         var = as.name(sett_proc$col_name_am)) ), 
             sett_proc$col_name_am_start),
    setNames(list(interp(~ max(var), 
                         var = as.name(sett_proc$col_name_am)) ), 
             sett_proc$col_name_am_end)
  )) %>% 
  ## Arrange by id and arrival measure
  arrange_(sett_proc$col_name_id, 
           sett_proc$col_name_am_start) %>% 
  ## Enumerate each type of pedal activity
  group_by_(sett_proc$col_name_id) %>% 
  mutate(pedal_act_nr = row_number()) 

## For lazyeval see:
## https://stackoverflow.com/questions/26003574/r-dplyr-mutate-use-dynamic-variable-names
## https://stackoverflow.com/questions/39252405/using-dplyr-summarise-in-r-with-dynamic-variable

## Merge data and pedal activity numeration
dat_pedal <- 
  left_join(dat_pedal,
            dat_pedal_summary %>% 
              select_(sett_proc$col_name_id, 
                      "pedal_act_id", 
                      "pedal_act_nr"))



# Visualize pedal activity ------------------------------------------------

plot_pedalActSeq <- function(dat, 
                             varname_x,
                             varname_y,
                             varname_facet_row,
                             sett_pl = sett_plot,
                             fill_nr_to_grey = NULL) {
  
  if (!is.null(fill_nr_to_grey))
    sett_pl$fill[fill_nr_to_grey] <- "grey" 
  
  dat <- 
    dat %>% 
    mutate_(.dots = setNames(list(
      interp(~ factor(v), v = as.name(varname_y))),
      varname_y))
  
  plot_dat <- 
    ggplot() +
    geom_tile(data = dat,
              aes_string(x = varname_x,
                         y = varname_y,
                         fill = "factor(pedal_act)",
                         alpha = "pedal_int_perc")) +
    facet_grid(as.formula(paste(varname_facet_row, "~.")), 
               scales = "free", 
               space = "free",
               drop = T) +
    scale_fill_manual(values = sett_pl$fill) #+ 
  # guides(fill = FALSE, 
  #        alpha = FALSE) + 
  # scale_x_continuous(expand = c(0, 0)) +
  # #scale_y_discrete(expand = c(0, 0)) +
  # scale_y_discrete(expand = c(0, 0), breaks = NULL) +
  # coord_cartesian(xlim = c(sett_pl$xlim_min, 
  #                          sett_pl$xlim_max)) + 
  # theme_bw() + 
  # theme(panel.grid = element_blank()) + 
  # theme(strip.text.y = element_text(angle=0)) 
  
}

plot_pedal <- 
  plot_pedalActSeq(dat_pedal, 
                   varname_x = sett_proc$col_name_am,
                   varname_y = sett_proc$col_name_id,
                   varname_facet_row = "interaction_type",
                   sett_plot)

if (sett_proc$plot) {
  windows(); plot(plot_pedal)
}



# Find last braking activity before threshold -----------------------------

dat_pedal_summary_last_braking_before_threshold <- 
  dat_pedal_summary %>% 
  filter(pedal_act == -1) %>% 
  group_by_(sett_proc$col_name_id) %>% 
  filter_(paste(sett_proc$col_name_am_start, "<=", sett_proc$crit_am)) %>% 
  filter(pedal_act_nr == max(pedal_act_nr)) %>% 
  rename(pedal_act_nr_last_braking_before_threshold = pedal_act_nr)

## Merge with full data
dat_pedal <-
  left_join(dat_pedal,
            dat_pedal_summary_last_braking_before_threshold %>% 
              select_(sett_proc$col_name_id, 
                      "pedal_act_nr_last_braking_before_threshold"),
            by = sett_proc$col_name_id)

## Order sequences
dat_pedal_summary_last_braking_before_threshold_order <- 
  dat_pedal_summary_last_braking_before_threshold %>% 
  arrange_(sett_proc$colname_group, 
           sett_proc$col_name_am_start) %>% 
  ungroup() %>% 
  select_(sett_proc$col_name_id) %>% 
  pull() %>% 
  rev()

## Capture missing cases
finder <- 
  unique_cases %in% 
  dat_pedal_summary_last_braking_before_threshold_order

dat_pedal_summary_last_braking_before_threshold_order <- 
  c(dat_pedal_summary_last_braking_before_threshold_order,
    unique_cases[!finder])

dat_pedal[, sett_proc$col_name_id] <- 
  factor(dat_pedal[, sett_proc$col_name_id], 
         levels = dat_pedal_summary_last_braking_before_threshold_order)



# Visualize last braking activity before threshold ------------------------

plot_pedal_last_braking_before_threshold <- 
  plot_pedalActSeq(dat_pedal, 
                   varname_x = sett_proc$col_name_am,
                   varname_y = sett_proc$col_name_id,
                   varname_facet_row = sett_proc$colname_group,
                   sett_plot,
                   fill_nr_to_grey = 1)

plot_pedal_last_braking_before_threshold <- 
  plot_pedal_last_braking_before_threshold + 
  geom_tile(data = 
              dat_pedal %>% 
              filter(pedal_act_nr == pedal_act_nr_last_braking_before_threshold),
            aes_string(x = sett_proc$col_name_am,
                       y = sett_proc$col_name_id),
            #fill = "factor(pedal_act)"),
            fill = "red2")

plot_title_txt <- 
  paste(paste0(sett_plot$file_name_prefix, ": "),
        "Last braking activity before threshold")

plot_pedal_last_braking_before_threshold <- 
  plot_pedal_last_braking_before_threshold + 
  ggtitle(plot_title_txt)

if (sett_proc$plot) {
  windows(); plot(plot_pedal_last_braking_before_threshold)
}



# Find first acceleration after braking -----------------------------------

dat_pedal_summary_first_acc_after_braking <-
  left_join(dat_pedal_summary,
            dat_pedal_summary_last_braking_before_threshold %>%
              select_(sett_proc$col_name_id,
                      "pedal_act_nr_last_braking_before_threshold")) %>% 
  filter(pedal_act == 1) %>% 
  group_by_(sett_proc$col_name_id) %>% 
  filter(pedal_act_nr > pedal_act_nr_last_braking_before_threshold) %>% 
  filter(pedal_act_nr == min(pedal_act_nr)) %>% 
  rename(pedal_act_nr_first_acc_after_braking = pedal_act_nr)

## Merge with full data
dat_pedal <-
  left_join(dat_pedal,
            dat_pedal_summary_first_acc_after_braking %>% 
              select_(sett_proc$col_name_id, 
                      "pedal_act_nr_first_acc_after_braking"),
            by = sett_proc$col_name_id)

## Order sequences
dat_pedal_summary_first_acc_after_braking_order <- 
  dat_pedal_summary_first_acc_after_braking %>% 
  arrange_(sett_proc$colname_group, 
           sett_proc$col_name_am_start) %>% 
  ungroup() %>% 
  select_(sett_proc$col_name_id) %>% 
  pull() %>% 
  rev()

## Capture missing cases
finder <- 
  unique_cases %in% 
  dat_pedal_summary_first_acc_after_braking_order

dat_pedal_summary_first_acc_after_braking_order <- 
  c(dat_pedal_summary_first_acc_after_braking_order,
    unique_cases[!finder])

dat_pedal[, sett_proc$col_name_id] <- 
  factor(dat_pedal[, sett_proc$col_name_id], 
         levels = dat_pedal_summary_first_acc_after_braking_order)



# Visualize first acceleration after braking ------------------------------

plot_pedal_first_acc_after_braking <- 
  plot_pedalActSeq(dat_pedal, 
                   varname_x = sett_proc$col_name_am,
                   varname_y = sett_proc$col_name_id,
                   varname_facet_row = sett_proc$colname_group,
                   sett_plot,
                   fill_nr_to_grey = 1)

plot_pedal_first_acc_after_braking <- 
  plot_pedal_first_acc_after_braking + 
  geom_tile(data = 
              dat_pedal %>% 
              filter(pedal_act_nr == pedal_act_nr_first_acc_after_braking),
            aes_string(x = sett_proc$col_name_am,
                       y = sett_proc$col_name_id),
            #fill = "factor(pedal_act)"),
            fill = "green2")

plot_title_txt <- 
  paste(paste0(sett_plot$file_name_prefix, ": "),
        "First acceleration activity before threshold")

plot_pedal_first_acc_after_braking <- 
  plot_pedal_first_acc_after_braking + 
  ggtitle(plot_title_txt)

if (sett_proc$plot) {
  windows(); plot(plot_pedal_first_acc_after_braking)
}



# Find last acceleration activity before braking --------------------------

dat_pedal_summary_last_acc_before_braking <-
  left_join(dat_pedal_summary,
            dat_pedal_summary_last_braking_before_threshold %>%
              select_(sett_proc$col_name_id,
                      "pedal_act_nr_last_braking_before_threshold")) %>% 
  filter(pedal_act == 1) %>% 
  group_by_(sett_proc$col_name_id) %>% 
  filter_(paste(sett_proc$col_name_am_start, "<=", sett_proc$crit_am, "&",
           "pedal_act_nr", "<", "pedal_act_nr_last_braking_before_threshold")) %>% 
  filter(pedal_act_nr == max(pedal_act_nr)) %>% 
  rename(pedal_act_nr_last_acc_before_braking = pedal_act_nr)

## Merge with full data
dat_pedal <-
  left_join(dat_pedal,
            dat_pedal_summary_last_acc_before_braking %>% 
              select_(sett_proc$col_name_id, 
                      "pedal_act_nr_last_acc_before_braking"),
            by = sett_proc$col_name_id)

## Order sequences
dat_pedal_summary_last_acc_before_braking_order <- 
  dat_pedal_summary_last_acc_before_braking %>% 
  arrange_(sett_proc$colname_group, 
           sett_proc$col_name_am_end) %>% 
  ungroup() %>% 
  select_(sett_proc$col_name_id) %>% 
  pull() %>% 
  rev()

## Capture missing cases
finder <- 
  unique_cases %in% 
  dat_pedal_summary_last_acc_before_braking_order

dat_pedal_summary_last_acc_before_braking_order <- 
  c(dat_pedal_summary_last_acc_before_braking_order,
    unique_cases[!finder])

dat_pedal[, sett_proc$col_name_id] <- 
  factor(dat_pedal[, sett_proc$col_name_id], 
         levels = dat_pedal_summary_last_acc_before_braking_order)



# Visualize last acceleration activity before braking ---------------------

plot_pedal_last_acc_before_braking <- 
  plot_pedalActSeq(dat_pedal, 
                   varname_x = sett_proc$col_name_am,
                   varname_y = sett_proc$col_name_id,
                   varname_facet_row = sett_proc$colname_group,
                   sett_plot,
                   fill_nr_to_grey = 1)

plot_pedal_last_acc_before_braking <- 
  plot_pedal_last_acc_before_braking + 
  geom_tile(data = 
              dat_pedal %>% 
              filter(pedal_act_nr == pedal_act_nr_last_acc_before_braking),
            aes_string(x = sett_proc$col_name_am,
                       y = sett_proc$col_name_id),
            #fill = "factor(pedal_act)"),
            fill = "green2")

plot_title_txt <- 
  paste(paste0(sett_plot$file_name_prefix, ": "),
        "Last acceleration activity before braking")

plot_pedal_last_acc_before_braking <- 
  plot_pedal_last_acc_before_braking + 
  ggtitle(plot_title_txt)

if (sett_proc$plot) {
  windows(); plot(plot_pedal_last_acc_before_braking)
}



# Find beginning of braking sequence --------------------------------------

dat_pedal_summary_braking <-
  left_join(dat_pedal_summary,
            dat_pedal_summary_last_acc_before_braking %>%
              select_(sett_proc$col_name_id,
                      "pedal_act_nr_last_acc_before_braking")) %>% 
  filter(pedal_act == -1) %>%
  filter_(paste(sett_proc$col_name_am_start, "<=", sett_proc$crit_am)) %>% 
  group_by_(sett_proc$col_name_id) %>% 
  ## In case there is no preceding acceleration activity
  mutate(pedal_act_nr_last_acc_before_braking = 
           pedal_act_nr_last_acc_before_braking) %>% 
  mutate(pedal_act_nr_last_acc_before_braking = 
           ifelse(is.na(pedal_act_nr_last_acc_before_braking),
                  -999,
                  pedal_act_nr_last_acc_before_braking)) %>% 
  filter(pedal_act_nr > pedal_act_nr_last_acc_before_braking) %>% 
  mutate(pedal_act_nr_braking_first = min(pedal_act_nr))

dat_pedal_summary_braking_first <- 
  dat_pedal_summary_braking %>% 
  group_by_(sett_proc$col_name_id) %>% 
  filter(pedal_act_nr == min(pedal_act_nr))

## Merge with full data
dat_pedal <-
  left_join(dat_pedal,
            dat_pedal_summary_braking_first %>% 
              select_(sett_proc$col_name_id, 
                      "pedal_act_nr_braking_first"),
            by = sett_proc$col_name_id)

## Order sequences
dat_pedal_summary_braking_first_order <- 
  dat_pedal_summary_braking_first %>% 
  group_by_(sett_proc$col_name_id) %>% 
  arrange_(sett_proc$colname_group, 
           sett_proc$col_name_am_start) %>% 
  ungroup() %>% 
  select_(sett_proc$col_name_id) %>% 
  pull() %>% 
  rev()

## Capture missing cases
finder <- 
  unique_cases %in% 
  dat_pedal_summary_braking_first_order

dat_pedal_summary_braking_first_order <- 
  c(unique_cases[!finder],
    dat_pedal_summary_braking_first_order)

dat_pedal[, sett_proc$col_name_id] <- 
  factor(dat_pedal[, sett_proc$col_name_id], 
         levels = dat_pedal_summary_braking_first_order)



# Visualize beginning of braking sequence ---------------------------------

plot_pedal_braking_first <- 
  plot_pedalActSeq(dat_pedal, 
                   varname_x = sett_proc$col_name_am,
                   varname_y = sett_proc$col_name_id,
                   varname_facet_row = sett_proc$colname_group,
                   sett_plot,
                   fill_nr_to_grey = 3)

plot_pedal_braking_first <- 
  plot_pedal_braking_first + 
  geom_tile(data = 
              dat_pedal %>% 
              filter(pedal_act_nr == pedal_act_nr_braking_first),
            aes_string(x = sett_proc$col_name_am,
                       y = sett_proc$col_name_id),
            #fill = "factor(pedal_act)"),
            fill = "red2")

plot_title_txt <- 
  paste(paste0(sett_plot$file_name_prefix, ": "),
        "Beginning of braking sequence")

plot_pedal_braking_first <- 
  plot_pedal_braking_first + 
  ggtitle(plot_title_txt)

if (sett_proc$plot) {
  windows(); plot(plot_pedal_braking_first)
}



# Compute evolution of releasing acceleration pedal -----------------------

dat_pedal_release_acc_before_braking_evo <-
  dat_pedal_summary_last_acc_before_braking %>%
  select_(sett_proc$col_name_id,
          sett_proc$colname_group,
          sett_proc$col_name_am_end) %>% 
  mutate_(.dots = setNames(
    list(interp(~ replace(var, is.na(var), min(var, na.rm = T)),
                var = as.name(sett_proc$col_name_am_end))),
    sett_proc$col_name_am_end)) %>% 
  group_by_(sett_proc$colname_group) %>%
  arrange_(sett_proc$col_name_am_end) %>%
  mutate(percentage = row_number() / max(row_number()) * 100) %>% 
  group_by_(sett_proc$colname_group, 
            sett_proc$col_name_am_end) %>% 
  summarise(percentage = max(percentage)) %>% 
  data.frame()


dat_pedal_release_acc_before_braking_evo <- 
  lapply(unique(dat_pedal_release_acc_before_braking_evo[, sett_proc$colname_group]), function(x) {
    
    dat <- dat_pedal_release_acc_before_braking_evo
    row_finder <- dat[, sett_proc$colname_group] == x
    dat <- dat[row_finder, ]
    
    dat <-
      intrpldf(dat %>% data.frame(),
               colname4ref = sett_proc$col_name_am_end,
               min = min(unique(dat_pedal[, sett_proc$col_name_am])),
               max = max(unique(dat_pedal[, sett_proc$col_name_am])),
               stepsize = 0.1,
               colnames2excl = "percentage",
               replace_preceding = T)
    
    dat$percentage <- na.locf(dat$percentage, na.rm = F)
    
    ## Replace NA with min percentage
    dat <-
      dat %>%
      group_by_(sett_proc$colname_group) %>%
      mutate(percentage =
               replace(percentage,
                       is.na(percentage),
                       min(percentage, na.rm = T)))
    
    return(dat)
    
  }) %>% bind_rows()



# Visualise evolution of releasing acceleration pedal ---------------------

plot_pedal_release_acc_before_braking_evo <- 
  ggplot() +
  geom_line(data = dat_pedal_release_acc_before_braking_evo,
            aes_string(x = sett_proc$col_name_am_end,
                       y = "percentage",
                       colour = sett_proc$colname_group)) + 
  # facet_grid(as.formula(paste(sett_proc$colname_group, "~.")), 
  #            scales = "free", 
  #            space = "free") +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(sett_plot$xlim_min, sett_plot$xlim_max),
                  ylim = c(0, 100)) + 
  ggtitle(paste(paste0(sett_plot$file_name_prefix, ": "),
                "Evolution of releasing acceleration pedal")) + 
  theme_bw() + 
  theme(strip.text.y = element_text(angle=0))

if (sett_proc$plot) {
  # windows(); plot(plot_pedal_release_acc_before_braking_evo)
}



# Compute evolution of first breaking activity ----------------------------

dat_pedal_braking_first_evo <-
  dat_pedal_summary_braking_first %>%
  group_by_(sett_proc$colname_group) %>%
  arrange_(sett_proc$colname_group, sett_proc$col_name_am_start) %>%
  select_(sett_proc$col_name_id, 
          sett_proc$colname_group, 
          sett_proc$col_name_am_start) %>% 
  mutate(percentage = row_number() / max(row_number()) * 100) %>% 
  group_by_(sett_proc$colname_group, 
            sett_proc$col_name_am_start) %>% 
  summarise(percentage = max(percentage)) %>% 
  data.frame()

dat_pedal_braking_first_evo <- 
  lapply(unique(dat_pedal_braking_first_evo[, sett_proc$colname_group]), function(x) {
    
    dat <- dat_pedal_braking_first_evo
    row_finder <- dat[, sett_proc$colname_group] == x
    dat <- dat[row_finder, ]
    
    dat <-
      intrpldf(dat %>% data.frame(),
               colname4ref = sett_proc$col_name_am_start,
               min = min(unique(dat_pedal[, sett_proc$col_name_am])),
               max = max(unique(dat_pedal[, sett_proc$col_name_am])),
               stepsize = 0.1,
               colnames2excl = "percentage",
               replace_preceding = T)
    
    dat$percentage <- na.locf(dat$percentage, na.rm = F)
    dat$percentage[is.na(dat$percentage)] <- 0
    
    return(dat)
    
  }) %>% bind_rows()



# Visualise evolution of first braking activity ---------------------------

plot_pedal_braking_first_evo <- 
  ggplot() +
  geom_line(data = dat_pedal_braking_first_evo,
            aes_string(x = sett_proc$col_name_am_start,
                       y = "percentage",
                       colour = sett_proc$colname_group)) + 
  # facet_grid(as.formula(paste(sett_proc$colname_group, "~.")), 
  #            scales = "free", 
  #            space = "free") +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(sett_plot$xlim_min, sett_plot$xlim_max),
                  ylim = c(0, 100)) + 
  ggtitle(paste(paste0(sett_plot$file_name_prefix, ": "),
                "Evolution of first brake press")) + 
  theme_bw() + 
  theme(strip.text.y = element_text(angle=0)) 

if (sett_proc$plot) {
  # windows(); plot(plot_pedal_braking_first_evo)
}



# Combine evolution of last rolling and braking activity ------------------

plot_pedal_evo <- 
  ggplot() +
  geom_line(data = dat_pedal_release_acc_before_braking_evo,
            aes_string(x = sett_proc$col_name_am_end,
                       y = "percentage",
                       colour = sett_proc$colname_group),
            linetype = "dashed") +
  geom_line(data = dat_pedal_braking_first_evo,
            aes_string(x = sett_proc$col_name_am_start,
                       y = "percentage",
                       colour = sett_proc$colname_group)) +
  # facet_grid(as.formula(paste(sett_proc$colname_group, "~.")), 
  #            scales = "free", 
  #            space = "free") +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(sett_plot$xlim_min, sett_plot$xlim_max),
                  ylim = c(0, 100)) + 
  ggtitle(paste(paste0(sett_plot$file_name_prefix, ": "),
                "Evolution of pedal activity")) + 
  theme_bw() + 
  theme(strip.text.y = element_text(angle=0)) #+
#coord_cartesian(xlim = c(-50, 5))

if (sett_proc$plot) {
  windows(); plot(plot_pedal_evo)
}