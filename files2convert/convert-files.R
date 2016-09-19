library(foreign)
library(xlsx)

q2convert <- "attrakdiff"

filepath <- file.path("files2convert", q2convert)
filelist <- list.files(filepath)
filelist <- filelist[grepl("sav", filelist)]

for(f in filelist) {
  
  dat <- read.spss(file.path(filepath, f),
                   use.value.labels = F, to.data.frame = T)
  
  dat <- data.frame(id = c(4001:4038), dat)
  
  write.xlsx(dat, paste(file.path(filepath, "xls", f), ".xls", sep = ""), row.names = F)
  
}