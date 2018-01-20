
# List all ADTF files -----------------------------------------------------

dat_list <-  
  list.files(path = "E:/_DATEN_ADTF/", 
             pattern = ".dat$", 
             recursive = TRUE)

dat_list <-  paste0("./", dat_list)

cat(dat_list, sep = "\n")



# Get meta info on subject and trip ---------------------------------------

dat_meta <-  
  dbGetSrc(dbFindConnObj("GBI"), 
           "t_adtf_file_assignment_v3_commented_v2_events") %>% 
  select(subject_id, 
         file_name, 
         file_size, 
         trip_nr_clean) %>% 
  filter(file_size > 0) %>% 
  filter(subject_id != 0) %>% 
  mutate(file_name_raw = sub("_export.csv", "", file_name)) %>% 
  mutate(trip_nr_clean = ifelse(trip_nr_clean == "0", "NA", trip_nr_clean)) %>% 
  mutate(file_name_prefix = 
           paste_(paste0("s", subject_id), 
                  paste0("trip", trip_nr_clean),
                  file_name_raw))



# Loop through each raw file name and create batch ------------------------

dir_batch <-  
  file.path("D:", 
            "Projekte", 
            "Gesture-based-interaction", 
            "Studie-5_Pro-VIL",
            "05_Datenverarbeitung", 
            "ADTF-Videos", "")

#dir_adtf <- "C:\\Program Files\\ADTF\\2.11.0\\bin\\"
#dir_compr <- "../addons/adtf-compression-toolbox/bin/video_compression_support.srv"

stream <-  
  c("video1",
    "video2")

#prop_codec <- "ffds"
prop_codec <- "xmp4"
prop_const_framerate <- "true"
prop_quality <- "100"
#in Bytes
prop_split_size=10737418240

dir_input <- "E:\\_DATEN_ADTF\\"
#dir_input <- "I:\\URBAN_MV_VIE_UniBw_Versuch2013\\04_Daten\\_ADTF_Rohdaten\\"
dir_output <- "D:\\Projekte\\Gesture-based-interaction\\Studie-5_Pro-VIL\\04_Daten\\Videos_ADTF"

streamNames <- 
  c("Mittelkonsole",
    "Fahrer")



#for(i in 1:nrow(dat_meta)) {
for(i in 1:nrow(dat_meta[1:2])) {
  
  ## Find correct directory
  dir_dat <-  
    lapply(list(dat_list), 
           function(x) grep(dat_meta$file_name_raw[i], x, value = T))
  
  batch <- c()
  dir_input_temp <- dir_input
  dir_input_temp <- gsub("\\./", "", paste0(dir_input_temp, dir_dat))
  dir_input_temp <- gsub("/", "\\\\", dir_input_temp)
  
  for(j in 1:length(stream)) {
    batch[j] <- 
      paste("@echo off\ncmd /k \"C: & cd Program Files (x86)\\ADTF\\2.13.2\\bin & adtf_datexporter.exe -service ../addons/adtf-compression-toolbox/bin/video_compression_support.srv -export ", stream[j], 
            " -sinkid adtf.export.compressed_video -property codec=", prop_codec, 
            " -property const_framerate=", prop_const_framerate, 
            " -property quality=", prop_quality, 
            " -output ", dir_output, "\\Videos_", streamNames[j], "\\", dat_meta$file_name_prefix[i], "_", streamNames[j], ".avi ", dir_input_temp, "\"", sep="")
    
    setwd(file.path(dir_batch, paste0("Batch_", streamNames[j])))
    
    write.table(batch[j], file = paste(dat_meta$file_name_prefix[i], "_", streamNames[j], ".bat", sep=""), row.names=F, col.names=F, quote=F)
  }
}


#für Master-Batch-Aufruf
#for(i in 1:nrow(dat_meta)) {
for(i in 1:nrow(dat_meta[1:2])) {
  
  ## Find correct directory
  dir_dat <-  
    lapply(list(dat_list), 
           function(x) grep(dat_meta$file_name_raw[i], x, value = T))
  
  batch <- c()
  dir_input_temp <- dir_input
  dir_input_temp <- gsub("\\./", "", paste0(dir_input_temp, dir_dat))
  dir_input_temp <- gsub("/", "\\\\", dir_input_temp)
  
  for(j in 1:length(stream)) {
    batch[j] <- 
      paste("@echo off\ncmd /c \"C: & cd Program Files (x86)\\ADTF\\2.13.2\\bin & adtf_datexporter.exe -service ../addons/adtf-compression-toolbox/bin/video_compression_support.srv -export ", stream[j], 
            " -sinkid adtf.export.compressed_video -property codec=", prop_codec, 
            " -property const_framerate=", prop_const_framerate, 
            " -property quality=", prop_quality, 
            " -output ", dir_output, "\\Videos_", streamNames[j], "\\", dat_meta$file_name_prefix[i], "_", streamNames[j], ".avi ", dir_input_temp, "\"",
            "\nEXIT", sep="")
    
    setwd(file.path(dir_batch, paste0("Batch_", streamNames[j], "_MASTER")))
    
    write.table(batch[j], file = paste(dat_meta$file_name_prefix[i], "_", streamNames[j], ".bat", sep=""), row.names=F, col.names=F, quote=F)
  }
}
