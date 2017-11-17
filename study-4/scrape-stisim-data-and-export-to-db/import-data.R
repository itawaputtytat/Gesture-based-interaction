
# Objective ---------------------------------------------------------------

## Extract BSAV data


# Preparatory settings ----------------------------------------------------

## Using stringr und fread from data.table (1.9.7) makes code fast
library(stringr)
## Needed for fread with argument "fill"
# install.packages("data.table", 
#                  repos = "https://Rdatatable.github.io/data.table", 
#                  type = "source")
library(data.table)




# File settings -----------------------------------------------------------

## Use for single data exploration
# show_plot <- T
# dat2import <- c()
# dat2import$subject <- 419
# dat2import$type <- "Geste"
# dat2import$type_lvl <- "einfach"
# dat2import$scenario <- "Autobahn"
# dat2import$pattern <-
#   paste(dat2import$subject,
#         dat2import$scenario,
#         dat2import$type,
#         dat2import$type_lvl, sep = "_")
# dat2import$filelist <- list.files("analysis_study4/_data_stisim")
# finder <- str_detect(dat2import$filelist, dat2import$pattern)
# dat2import$filename <- dat2import$filelist[finder]

## Keep this line for import-operator.R
path <- file.path("analysis_study4/_data_stisim", dat2import$filename)

# path <- file.choose()
# path_split <- unlist(str_split(path, "\\\\"))
# filename <- path_split[length(path_split)]
#path <- "../../Studie4_Fahrsimulator/Daten/STISIM/438/438_Stadt_Geste_einfach_5.Dat"



# Import data -------------------------------------------------------------

#ptm <- proc.time()

dat_raw <- readChar(path, file.info(path)$size)
dat_raw <- unlist(str_split(dat_raw, "\r\n"))



# Extract meta information ------------------------------------------------

meta <- c()
meta$date <- str_replace(dat_raw[1], " Date: ", "")
meta$time <- str_replace(dat_raw[2], " Time: ", "")
meta$id <- str_replace(dat_raw[3], " ID:   ", "")
meta$id <- as.numeric(meta$id)
meta$name <- str_replace(dat_raw[8], " Name: ", "")
meta$run <- str_replace(dat_raw[9], " Run #: ", "")
meta$run <- as.numeric(meta$run)



# Extract driving data ----------------------------------------------------

## Find beginning and ending of driving data
pos_start <- which(str_detect(dat_raw, "Block")) + 2
pos_end <- min(which(str_detect(dat_raw, "Time to collision")) - 3)

## Extract data from raw data
dat_extract <- dat_raw[pos_start:pos_end]
dat_extract <- str_split(dat_extract, " ")
dat_extract <- lapply(dat_extract, function(x) x[-which(x == "")] )

## Find maximum number of columns
col_n <- sapply(dat_extract, length)
col_n_max <- max(col_n)

## Read extracted data as table and produce data.frame
## Workaround: Adding a line with maximum number of columns
dat_text <- lapply(dat_extract, function(x) paste(unlist(x), collapse = ";"))
dat_text <- paste(dat_text, collapse = "\n")
dummy_row <- paste(rep(";", col_n_max - 1), collapse = "")
dat_text <- paste(dummy_row, dat_text, sep = "\n")
dat_df <- fread(dat_text, sep = ";", dec = ",", fill = TRUE, data.table = F)
dat_df <- dat_df[-1, ]



# Extract BSAV line -------------------------------------------------------

## Extract BSAV line, split by string and unlist into array
bsav_rowfinder <- which(str_detect(dat_raw, "BSAV"))
bsav_raw <- dat_raw[bsav_rowfinder]
bsav_split <- str_split(bsav_raw, ",")
bsav_split <- unlist(bsav_split)

## Clean extracted line: Remove tabulators and spaces
bsav_clean <- str_replace_all(bsav_split, "\\t", "")
bsav_clean <- str_replace_all(bsav_clean, " ", "")

## Extract BSAV variable numbers (starts at sixth position)
bsav_codes <- bsav_clean[-c(1:5)]

## Recode as numeric
bsav_codes[str_detect(bsav_codes, "#")] <- 46
bsav_codes <- as.numeric(bsav_codes)



# Create column names -----------------------------------------------------

## Get BSAV variable names from file
path4bsavnames <- "analysis_study4/stisim-data-to-db/bsav-code_variable.csv"
dat4bsav <- read.table(path4bsavnames, sep = ";", header = T, stringsAsFactors = F)

## Create "safe" variable names
bsav_varnames <- dat4bsav$name[bsav_codes + 1]
## (further adjustments for sub-variables and multiple occurences to be made)
## ... using list
bsav_varnames_list <- as.list(bsav_varnames)

## Additional adjustments: 
## Replace correspending list elements with new arrays for OM, 46 and 19

## Create column names for "49" relates variables: OM
bsav_varnames_om <- paste("om49_var", 1:22, sep = "")
listpos_finder <- which(bsav_varnames_list == "om49")
bsav_varnames_list[[listpos_finder]] <- bsav_varnames_om

## Create column names for "46" related variables: Specific road objects
obj46_posfinder <- which(grepl(c("46"), bsav_clean))
obj46_nr <- str_replace(bsav_clean[obj46_posfinder], "46#", "")
listpos_finder <- which(bsav_varnames_list == "specobj46")

if (obj46_nr[1] == 46 & length(obj46_nr) == 1) {
  cat("Irregular BSAV-46 \n")
  bsav_varnames_list[listpos_finder] <- NULL
} else {
  bsav_varnames_specobj46_sub <- c("lodist_m", "latpos_m", "speed_ms")
  ## Extract corresponding vehicle numbers from clean/raw data
  bsav_varnames_specobj46 <- paste("specobj46", obj46_nr, sep = "_")
  bsav_varnames_specobj46 <- 
    lapply(bsav_varnames_specobj46, function(x)
      paste(x, bsav_varnames_specobj46_sub, sep = "_"))
  bsav_varnames_list[listpos_finder] <- bsav_varnames_specobj46
}

## Create column names for "19" related variables: Road objects
## Finder must not work using grepl,in case of "46#19"
## !!! Assumes that 19 stands on last position !!!
obj19_posfinder <- which(bsav_clean == 19)
bsav_varnames <- unlist(bsav_varnames_list)

if (length(obj19_posfinder) == 0) {
  cat("No BSAV-19 \n")
} else {
  bsav_varnames_obj19_sub <- c("id", "lodist_m", "latpos_m", "speed_ms")
  ## Original column will be replaced by multiple columns
  bsav_varnames <- bsav_varnames[-length(bsav_varnames)]
  ## ... by the amount of remaining columns
  ## ... devided by columns needed for 19 (which is 4)
  obj19_col_n <- ncol(dat_df) - length(bsav_varnames)
  obj19_col_n <- obj19_col_n / length(bsav_varnames_obj19_sub)
  bsav_varnames_obj19 <- paste("obj19", 1:obj19_col_n, sep = "_")
  bsav_varnames_obj19 <-
    lapply(bsav_varnames_obj19, function(x) 
             paste(x, bsav_varnames_obj19_sub, sep = "_"))
  
  bsav_varnames <- c(bsav_varnames, unlist(bsav_varnames_obj19))
}



# Rename columns ----------------------------------------------------------

colnames(dat_df) <- bsav_varnames

# stoptime <- proc.time() - ptm
# cat("Elapsed:", stoptime[3], "\n")



# Variable adjustments ----------------------------------------------------

# ## Re-compute speed using longitudinal acceleration
# library(dplyr)
# dat_df <-
#   dat_df %>%
#   data.frame() %>%
#   mutate(time_s_temp = eltime01 - lag(eltime01, default = 0)) %>%
#   mutate(speed_new = lonacc02 * time_s_temp) %>%
#   mutate(speed_new2 = cumsum(speed_new))

#plot(dat_df$eltime01, type = "l")
#plot(dat_df$time_s_temp, type = "l")
#plot(dat_df$lonacc02, type = "l")

#plot(dat_df$spedmh23, type = "l")
#plot(dat_df$speed_new, type = "l")
#plot(dat_df$speed_new2 * -1, type = "l")
#lines(dat_df$speed_new2 * 3.6 * 1.60934, col = "red")

if(show_plot == T)
  plot(dat_df$lodist06, dat_df$spedmh23, type = "l", main = dat2import$filename)
