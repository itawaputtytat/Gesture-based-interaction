
# Settings ----------------------------------------------------------------

sett_import <- c()
sett_import$db_name <- "GBI_Study-5"
sett_import$db_conn_name <- dbFindConnObj(sett_import$db_name, output = F)
sett_import$dir_name <- "../../Studie-5_Pro-VIL/04_Daten/Fragebogen_xlsx/"
sett_import$file_names <- list.files(sett_import$dir_name)

library(readxl)



# Procedure ---------------------------------------------------------------

## Set file path
## Read data
## Replace column names
## Write data to database



# Simulator sickness ------------------------------------------------------

## Pre
sett_import$file_path <- 
  file.path(sett_import$dir_name, 
            "demo.xlsx")

dat <- read_excel(sett_import$file_path, col_names = T, na = ".")
#dat[, c(2:29)] <- dat[, c(2:29)] - 4

colnames(dat) <- c("subject_id", "age", "sex", "study_program", 
                   "experienced_with_gbi", "interest_in_gbi")
dat$sex[dat$sex == 1] <- "m"
dat$sex[dat$sex == 2] <- "f"
dat$experienced_with_gbi[dat$experienced_with_gbi == 1] <- T
dat$experienced_with_gbi[dat$experienced_with_gbi == 2] <- F
dat$experienced_with_gbi <- as.logical(dat$experienced_with_gbi)
dat <- dat[!is.na(dat$subject_id), ]

dbWriteTable(get(sett_import$db_conn_name), 
             "t_q_demography", 
             dat, overwrite = T, row.names = F)

# ## Post
# sett_import$file_path <- 
#   file.path(sett_import$dir_name,
#             "security_touch.xlsx")
# 
# dat <- read_excel(sett_import$file_path, col_names = F, na = ".")
# #dat[, c(2:29)] <- dat[, c(2:29)] - 4
# 
# colnames(dat) <- c("subject_id", sprintf("safety_%02d", 1:(ncol(dat)-1)))
# 
# dbWriteTable(get(sett_import$db_conn_name), 
#              "t_q_safety_touch", 
#              dat, overwrite = T, row.names = F)