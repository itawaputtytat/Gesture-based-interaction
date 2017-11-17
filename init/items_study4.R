
# Initialise objects ------------------------------------------------------

items <- c()
name4dbconn <- "dbconn_study4"



# Acceptance --------------------------------------------------------------

catWSepLine("Labels for questionnaire: Acceptance")

items$labels$acc <- dbGetSrc("t_q_labels_acc", name4dbconn)
items$labels$acc <- items$labels$acc %>% arrange(itemnr)
items$labels$acc <- items$labels$acc[, paste("item_", set4lang, sep = "")]

items$names$acc <- 
  paste("acc", sprintf("%02d", seq(1:length(items$labels$acc))), sep = "_")



# attrakdiff 2 ------------------------------------------------------------

catWSepLine("Labels for questionnaire: attrakdiff 2")

items$labels$attrakdiff <- dbGetSrc("t_q_labels_attrakdiff", name4dbconn)
items$labels$attrakdiff <- items$labels$attrakdiff %>% arrange(itemnr)
items$labels$attrakdiff <- items$labels$attrakdiff[, paste("item", set4lang, sep = "_")]

items$names$attrakdiff <- 
  paste("attrakdiff", sprintf("%02d", seq(1:length(items$labels$attrakdiff))), sep = "_")

## New order of items to be consistent with resource pictures
itemnr_attrakdiff_pq  <- c(1:4, 6, 5, 28)
itemnr_attrakdiff_hqi <- c(7:13)
itemnr_attrakdiff_hqs <- c(14:19, 27)
itemnr_attrakdiff_hq  <- c(itemnr_attrakdiff_hqi,
                           itemnr_attrakdiff_hqs)
itemnr_attrakdiff_att <- c(20:26)

itemnr_attrakdiff_ordered <- 
  c(itemnr_attrakdiff_pq,
    itemnr_attrakdiff_hqi,
    itemnr_attrakdiff_hqs,
    itemnr_attrakdiff_att)



# NASA-TLX ----------------------------------------------------------------

catWSepLine("Labels for questionnaire: NASA-TLX")

items$labels$nasatlx <- dbGetSrc("t_q_labels_nasatlx", name4dbconn)
items$labels$nasatlx <- items$labels$nasatlx %>% arrange(itemnr)
items$labels$nasatlx <- items$labels$nasatlx[, paste("item_", set4lang, sep = "")]

items$names$nasatlx <- 
  paste("nasatlx", sprintf("%02d", seq(1:length(items$labels$nasatlx))), sep = "_")

items$names$nasatlx[4] <- paste(items$names$nasatlx[4], "i", sep = "")



# Feedback ----------------------------------------------------------------

catWSepLine("Labels for questionnaire: Feedback")

items$labels$fb <- dbGetSrc("t_q_labels_fb", name4dbconn)
items$labels$fb <- items$labels$fb %>% arrange(itemnr)
items$labels$fb <- items$labels$fb[, paste("item_", set4lang, sep = "")]

items$names$fb <- 
  paste("fb", sprintf("%02d", seq(1:length(items$labels$db))), sep = "_")



# Naturalness -------------------------------------------------------------

catWSepLine("Labels for questionnaire: Naturalness")

items$labels$nat <- dbGetSrc("t_q_labels_nat", name4dbconn)
items$labels$nat <- items$labels$nat %>% arrange(itemnr)
items$labels$nat <- items$labels$nat[, paste("item_", set4lang, sep = "")]

items$names$nat <- 
  paste("nat", sprintf("%02d", seq(1:length(items$labels$nat))), sep = "_")

