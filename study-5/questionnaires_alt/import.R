sett_dat <- c()
sett_dat$file_dir <- "../../Studie-5_Pro-VIL/Daten/Fragebogen_xlsx"
sett_dat$file_name <- "nasa_geste_einfach.xlsx"
sett_dat$file_path <- file.path(sett_dat$file_dir, sett_dat$file_name)
sett_dat$n_items <- 6
sett_dat$col_names <- 
  c("subject_id", 
    paste_("nasatlx", sprintf("%02d", 1:sett_dat$n_items)) )

sett_dat$db_name <- dbFindConnObj("Study-5")
sett_dat$src_name <- "t_q_nasatlx_gestures_simple"

dat <- 
  openxlsx::read.xlsx(sett_dat$file_path, 
                      colNames = F, 
                      na.strings = "       .")

##Acceptance
#dat[, 2:ncol(dat)] <- dat[, 2:ncol(dat)] - 3

colnames(dat) <- sett_dat$col_names

dbWriteTable(get(sett_dat$db_name), 
             name = sett_dat$src_name,
             dat,
             row.names = F,
             overwrite = T)