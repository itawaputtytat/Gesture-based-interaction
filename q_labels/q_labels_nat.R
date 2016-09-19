cat("* Labels for questionnaire: Naturalness \n")

itemlabels$nat <- dbGetSrc("t_q_labels_nat")[, paste("item_", setlanguage, sep = "")]
itemsets$nat <- paste("nat", sprintf("%02d", seq(1:length(itemlabels$nat))), sep = "_")
