cat("* Labels for questionnaire: Acceptance \n")

itemlabels$acc <- dbGetSrc("t_q_labels_acc")[, paste("item_", setlanguage, sep = "")]
itemsets$acc <- paste("acc", sprintf("%02d", seq(1:length(itemlabels$acc))), sep = "_")
