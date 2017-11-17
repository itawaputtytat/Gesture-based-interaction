
# Initialise objects ------------------------------------------------------

set4items <- c()
name4dbconn <- "dbconn_study1to3"



# Acceptance --------------------------------------------------------------

catWSepLine("Labels for questionnaire: Acceptance")

set4items$labels$acc <- dbGetSrc(name4dbconn, "t_q_labels_acc")
set4items$labels$acc <- set4items$labels$acc %>% arrange(itemnr)
set4items$labels$acc <- set4items$labels$acc[, paste("item_", set4lang, sep = "")]

set4items$names$acc <- 
  paste("acc", sprintf("%02d", seq(1:length(set4items$labels$acc))), sep = "_")



# attrakdiff 2 ------------------------------------------------------------

catWSepLine("Labels for questionnaire: Attrakdiff 2")

set4items$labels$attrakdiff <- dbGetSrc(name4dbconn, "t_q_labels_attrakdiff")
set4items$labels$attrakdiff <- set4items$labels$attrakdiff %>% arrange(itemnr)
set4items$labels$attrakdiff <- set4items$labels$attrakdiff[, paste("item", set4lang, sep = "_")]

set4items$names$attrakdiff <- 
  paste("attrakdiff", sprintf("%02d", seq(1:length(set4items$labels$attrakdiff))), sep = "_")

## New order of items to be consistent with resource pictures
set4items$nrs$attrakdiff_pq  <- c(1:4, 6, 5, 28)
set4items$nrs$attrakdiff_hqi <- c(7:13)
set4items$nrs$attrakdiff_hqs <- c(14:19, 27)
set4items$nrs$attrakdiff_hq  <- c(set4items$nrs$attrakdiff_hqi,
                           set4items$nrs$attrakdiff_hqs)
set4items$nrs$attrakdiff_att <- c(20:26)

set4items$nrs$attrakdiff_ordered <- 
  c(set4items$nrs$attrakdiff_pq,
    set4items$nrs$attrakdiff_hqi,
    set4items$nrs$attrakdiff_hqs,
    set4items$nrs$attrakdiff_att)



# NASA-TLX ----------------------------------------------------------------

catWSepLine("Labels for questionnaire: NASA-TLX")

set4items$labels$nasatlx <- dbGetSrc(name4dbconn, "t_q_labels_nasatlx")
set4items$labels$nasatlx <- set4items$labels$nasatlx %>% arrange(itemnr)
set4items$labels$nasatlx <- set4items$labels$nasatlx[, paste("item_", set4lang, sep = "")]

set4items$names$nasatlx <- 
  paste("nasatlx", sprintf("%02d", seq(1:length(set4items$labels$nasatlx))), sep = "_")

set4items$names$nasatlx[4] <- paste(set4items$names$nasatlx[4], "i", sep = "")



# Feedback ----------------------------------------------------------------

catWSepLine("Labels for questionnaire: Feedback")

set4items$labels$fb <- dbGetSrc(name4dbconn, "t_q_labels_fb")
set4items$labels$fb <- set4items$labels$fb %>% arrange(itemnr)
set4items$labels$fb <- set4items$labels$fb[, paste("item_", set4lang, sep = "")]

set4items$names$fb <- 
  paste("fb", sprintf("%02d", seq(1:length(set4items$labels$db))), sep = "_")



# Naturalness -------------------------------------------------------------

catWSepLine("Labels for questionnaire: Naturalness")

set4items$labels$nat <- dbGetSrc(name4dbconn, "t_q_labels_nat")
set4items$labels$nat <- set4items$labels$nat %>% arrange(itemnr)
set4items$labels$nat <- set4items$labels$nat[, paste("item_", set4lang, sep = "")]

set4items$names$nat <- 
  paste("nat", sprintf("%02d", seq(1:length(set4items$labels$nat))), sep = "_")




# Safety ------------------------------------------------------------------

catWSepLine("Labels for questionnaire: Safety")

set4items$labels$safety <- dbGetSrc("dbconn_study4", "t_q_labels_safety")
set4items$labels$safety <- set4items$labels$safety %>% arrange(itemnr)
set4items$labels$safety <- set4items$labels$safety[, paste("item_", set4lang, sep = "")]

set4items$names$safety <- 
  paste("safety", sprintf("%02d", seq(1:length(set4items$labels$safety))), sep = "_")



# Evaluation --------------------------------------------------------------

catWSepLine("Labels for questionnaire: Evaluation")

set4items$labels$eval <- dbGetSrc("dbconn_study4", "t_q_labels_eval")
set4items$labels$eval <- set4items$labels$eval %>% arrange(itemnr)
set4items$labels$eval <- set4items$labels$eval[, paste("item_", set4lang, sep = "")]

set4items$names$eval <- 
  paste("eval", sprintf("%02d", seq(1:length(set4items$labels$eval))), sep = "_")
