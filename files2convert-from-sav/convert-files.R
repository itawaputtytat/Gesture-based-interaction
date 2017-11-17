library(foreign)
library(xlsx)

q2convert <- "driving-errors"

filepath <- file.path("files2convert-from-sav", q2convert)
filelist <- list.files(filepath)
filelist <- filelist[grepl("sav", filelist)]

for(f in filelist) {
  
  dat <- read.spss(file.path(filepath, f),
                   use.value.labels = F, to.data.frame = T)
  
  dat <- data.frame(id = c(4001:4038), dat)
  
  write.xlsx(dat, paste(file.path(filepath, "xls", f), ".xls", sep = ""), row.names = F)
  
}



#dat <- as.data.set(spss.system.file(file.path(filepath, f)))
