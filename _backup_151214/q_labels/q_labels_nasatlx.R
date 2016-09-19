cat("* Labels for questionnaire: NASA-TLX \n")

## Variable names in data
itemsets$nasatlx <- paste("nasatlx", sprintf("%02d", seq(1:6)), sep = "_")
itemsets$nasatlx[4] <- paste(itemsets$nasatlx[4], "i", sep = "")

itemlabels$nasatlx <- data.frame(
  eng = c("mental", 
          "physical", 
          "time", 
          "success", 
          "effort", 
          "negative"),
  ger = c("mental",
          "physisch",
          "Zeit",
          "Erfolg",
          "Aufwand",
          "negativ"))