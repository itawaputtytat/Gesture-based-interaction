
# Acceptance --------------------------------------------------------------

cat("* Labels for questionnaire: Acceptance \n")

itemlabels$acc <- dbGetSrc("t_q_labels_acc")
itemlabels$acc <- itemlabels$acc %>% arrange(itemnr)
itemlabels$acc <- itemlabels$acc[, paste("item_", setlanguage, sep = "")]

itemsets$acc <- 
  paste("acc", sprintf("%02d", seq(1:length(itemlabels$acc))), sep = "_")



# AttrakDiff 2 ------------------------------------------------------------

cat("* Labels for questionnaire: AttrakDiff 2 \n")

itemlabels$attrakdiff <- dbGetSrc("t_q_labels_attrakdiff")
itemlabels$attrakdiff <- itemlabels$attrakdiff %>% arrange(itemnr)
itemlabels$attrakdiff <- itemlabels$attrakdiff[, paste("item_", setlanguage, sep = "")]

itemsets$attrakdiff <- 
  paste("attrakdiff", sprintf("%02d", seq(1:length(itemlabels$attrakdiff))), sep = "_")

## New ordering of items to be consistent with resource pictures
itemnr_attrakdiff_pq  <- c(1:4, 6, 5, 28)
itemnr_attrakdiff_hqi <- c(7:13)
itemnr_attrakdiff_hqs <- c(14:19, 27)
itemnr_attrakdiff_hq  <- c(itemnr_attrakdiff_hqi,
                           itemnr_attrakdiff_hqs)
itemnr_attrakdiff_att <- c(20:26)

itemnr_attrakdiff_ordered <- c(itemnr_attrakdiff_pq,
                               itemnr_attrakdiff_hqi,
                               itemnr_attrakdiff_hqs,
                               itemnr_attrakdiff_att)



# NASA-TLX ----------------------------------------------------------------

cat("* Labels for questionnaire: NASA-TLX \n")

itemlabels$nasatlx <- dbGetSrc("t_q_labels_nasatlx")
itemlabels$nasatlx <- itemlabels$nasatlx %>% arrange(itemnr)
itemlabels$nasatlx <- itemlabels$nasatlx[, paste("item_", setlanguage, sep = "")]

itemsets$nasatlx <- 
  paste("nasatlx", sprintf("%02d", seq(1:length(itemlabels$nasatlx))), sep = "_")

itemsets$nasatlx[4] <- paste(itemsets$nasatlx[4], "i", sep = "")



# Feedback ----------------------------------------------------------------

cat("* Labels for questionnaire: Feedbacl \n")

itemlabels$fb <- dbGetSrc("t_q_labels_fb")
itemlabels$fb <- itemlabels$fb %>% arrange(itemnr)
itemlabels$fb <- itemlabels$fb[, paste("item_", setlanguage, sep = "")]

itemsets$fb <- 
  paste("fb", sprintf("%02d", seq(1:length(itemlabels$db))), sep = "_")



# Naturalness -------------------------------------------------------------

cat("* Labels for questionnaire: Naturalness \n")

itemlabels$nat <- dbGetSrc("t_q_labels_nat")
itemlabels$nat <- itemlabels$nat %>% arrange(itemnr)
itemlabels$nat <- itemlabels$nat[, paste("item_", setlanguage, sep = "")]

itemsets$nat <- 
  paste("nat", sprintf("%02d", seq(1:length(itemlabels$nat))), sep = "_")

