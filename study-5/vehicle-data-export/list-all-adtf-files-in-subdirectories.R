
# List all ADTF files -----------------------------------------------------

dat_list <- 
  list.files(path = "F:/_DATEN_ADTF/", 
             pattern = ".dat$", 
             recursive = TRUE)

dat_list <- paste0("./", dat_list)

cat(dat_list, sep = "\n")



# List all csv files ------------------------------------------------------

dat_list <- c()
dat_list$filename <- 
  unlist(list.files(path = "F:/_DATEN_ADTF/", 
             pattern = ".csv$", 
             recursive = TRUE,
             full.names = T))

dat_list$file_size <- file.size(dat_list$filename)

dat_list <- 
  do.call(cbind, 
          lapply(dat_list, 
                 function(x) 
                   data.frame(x, row.names = NULL, stringsAsFactors = FALSE)))
dat_list <- setNames(dat_list, c("filename", "file_size"))

dat_list$filename_short <-
  lapply(dat_list$filename, 
         function(x) {
           temp <- (gregexpr("/", x))
           temp <- max(unlist(temp))
           filename_short <- substr(x, temp+1, nchar(x))
         }
  )


library(openxlsx)
write.xlsx(dat_list, "adtf_csv-exports.xlsx")


        