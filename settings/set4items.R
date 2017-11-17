
# Initialise objects ------------------------------------------------------

set4items <- c()
set4items$lang <- "eng" ## Set language for plot annotation in "ger" vs. "eng"
set4items$name4dbconn <- "dbconn_study1to3"



# Acceptance --------------------------------------------------------------

catWSepLine("Labels for questionnaire: Acceptance")

## Get items
set4items$acc$labels <- 
  dbGetSrc(set4items$name4dbconn, "t_q_labels_acc")

## Sort items by item number
set4items$acc$labels <- 
  set4items$acc$labels %>% 
  arrange(itemnr)

## Select items corresponding to language settings
set4items$acc$labels <- 
  set4items$acc$labels[, paste("item_", set4items$lang, sep = "")]

## Create list of variable names
set4items$acc$varnames <- 
  paste("acc", 
        sprintf("%02d", seq(1:length(set4items$acc$labels))), sep = "_")

set4items$acc$itemnrs4scales$usefulness <- c(1, 3, 5, 7, 9)
set4items$acc$itemnrs4scales$satisfying  <- c(2, 4, 6, 8)



# attrakdiff 2 ------------------------------------------------------------

catWSepLine("Labels for questionnaire: attrakdiff 2")

## Get items
set4items$attrakdiff$labels <- 
  dbGetSrc(set4items$name4dbconn, "t_q_labels_attrakdiff")

## Sort items by item number
set4items$attrakdiff$labels <- 
  set4items$attrakdiff$labels %>% 
  arrange(itemnr)

## Select items corresponding to language settings
set4items$attrakdiff$labels <- 
  set4items$attrakdiff$labels[, paste("item", set4items$lang, sep = "_")]

## Create list of variable names
set4items$attrakdiff$varnames <- 
  paste("attrakdiff", 
        sprintf("%02d", seq(1:length(set4items$attrakdiff$labels))), sep = "_")

set4items$attrakdiff$itemnrs4scales$pq <- c(1:4, 6, 5, 28)
set4items$attrakdiff$itemnrs4scales$hqi <- c(7:13)
set4items$attrakdiff$itemnrs4scales$hqs <- c(14:19, 27)
set4items$attrakdiff$itemnrs4scales$hq  <- c(set4items$attrakdiff$itemnrs4scales$hqi,
                                             set4items$attrakdiff$itemnrs4scales$hqs)
set4items$attrakdiff$itemnrs4scales$att <- c(20:26)

set4items$attrakdiff$itemnrs4scales_ordered <- 
  c(set4items$attrakdiff$itemnrs4scales$pq,
    set4items$attrakdiff$itemnrs4scales$hqi,
    set4items$attrakdiff$itemnrs4scales$hqs,
    set4items$attrakdiff$itemnrs4scales$att)



# NASA-TLX ----------------------------------------------------------------

catWSepLine("Labels for questionnaire: NASA-TLX")

## Get items
set4items$nasatlx$labels <- 
  dbGetSrc(set4items$name4dbconn, "t_q_labels_nasatlx")

## Sort items by item number
set4items$nasatlx$labels <- 
  set4items$nasatlx$labels %>% 
  arrange(itemnr)

## Select items corresponding to language settings
set4items$nasatlx$labels <- 
  set4items$nasatlx$labels[, paste("item_", set4items$lang, sep = "")]

## Create list of variable names
set4items$nasatlx$varnames <-
  paste("nasatlx", 
        sprintf("%02d", seq(1:length(set4items$nasatlx$labels))), sep = "_")

set4items$nasatlx$varnames[4] <- paste(set4items$nasatlx$varnames[4], "i", sep = "")

set4items$nasatlx$itemnrs4scales$mental_demand <- 1
set4items$nasatlx$itemnrs4scales$physical_demand <- 2
set4items$nasatlx$itemnrs4scales$temporal_demand <- 3
set4items$nasatlx$itemnrs4scales$performance <- 4
set4items$nasatlx$itemnrs4scales$effort <- 5
set4items$nasatlx$itemnrs4scales$frustration <- 6



# Feedback ----------------------------------------------------------------

# catWSepLine("Labels for questionnaire: Feedback")
# 
# set4items$labels$fb <- dbGetSrc(set4items$name4dbconn, "t_q_labels_fb")
# set4items$labels$fb <- set4items$labels$fb %>% arrange(itemnr)
# set4items$labels$fb <- set4items$labels$fb[, paste("item_", set4lang, sep = "")]
# 
# set4items$names$fb <- 
#   paste("fb", sprintf("%02d", seq(1:length(set4items$labels$db))), sep = "_")



# Naturalness -------------------------------------------------------------

# catWSepLine("Labels for questionnaire: Naturalness")
# 
# set4items$labels$nat <- dbGetSrc(set4items$name4dbconn, "t_q_labels_nat")
# set4items$labels$nat <- set4items$labels$nat %>% arrange(itemnr)
# set4items$labels$nat <- set4items$labels$nat[, paste("item_", set4lang, sep = "")]
# 
# set4items$names$nat <- 
#   paste("nat", sprintf("%02d", seq(1:length(set4items$labels$nat))), sep = "_")



# Safety comparison -------------------------------------------------------

catWSepLine("Labels for questionnaire: Safety comparison")

## Get items
set4items$safety$labels <- 
  dbGetSrc("dbconn_study4", "t_q_labels_safety")

## Sort items by item number
set4items$safety$labels <- 
  set4items$safety$labels %>% 
  arrange(itemnr)

## Select items corresponding to language settings
set4items$safety$labels <- 
  set4items$safety$labels[, paste("item", set4items$lang, sep = "_")]

## Create list of variable names
set4items$safety$varnames <-
  paste("safety", 
        sprintf("%02d", seq(1:length(set4items$safety$labels))), sep = "_")



# Evaluation --------------------------------------------------------------

catWSepLine("Labels for questionnaire: Evaluation")

## Get items
set4items$eval$labels <- 
  dbGetSrc("dbconn_study4", "t_q_labels_eval")

## Sort items by item number
set4items$eval$labels <- 
  set4items$eval$labels %>% 
  arrange(itemnr)

## Select items corresponding to language settings
set4items$eval$labels <- 
  set4items$eval$labels[, paste("item", set4items$lang, sep = "_")]

## Create list of variable names
set4items$eval$varnames <-
  paste("eval", 
        sprintf("%02d", seq(1:length(set4items$eval$labels))), sep = "_")



# TA-EG -------------------------------------------------------------------

catWSepLine("Labels for questionnaire: TA-EG")

## Get items
set4items$taeg$labels <- 
  dbGetSrc("dbconn_study4", "t_q_labels_taeg")

## Sort items by item number
set4items$taeg$labels <- 
  set4items$taeg$labels %>% 
  arrange(itemnr)

## Select items corresponding to language settings
set4items$taeg$labels <- 
  set4items$taeg$labels[, paste("item", set4items$lang, sep = "_")]

## Create list of variable names
set4items$taeg$varnames <-
  paste("taeg", 
        sprintf("%02d", seq(1:length(set4items$taeg$labels))), sep = "_")


set4items$taeg$itemnrs4scales$excitement <- c(1, 3, 8, 10, 15)
set4items$taeg$itemnrs4scales$competence <- c(4, 14, 17, 18)
set4items$taeg$itemnrs4scales$attitude_pos <- c(5, 9, 11, 12, 19)
set4items$taeg$itemnrs4scales$attitude_neg <- c(2, 6, 7, 13, 16)

set4items$taeg$itemnrs4scales_ordered <- 
  c(set4items$taeg$itemnrs4scales$excitement,
    set4items$taeg$itemnrs4scales$competence,
    set4items$taeg$itemnrs4scales$attitude_pos,
    set4items$taeg$itemnrs4scales$attitude_neg)




# Trust -------------------------------------------------------------------


catWSepLine("Labels for questionnaire: Trust in automation")

## Get items
set4items$trust$labels <- 
  dbGetSrc("dbconn_study4", "t_q_labels_trust")

## Sort items by item number
set4items$trust$labels <- 
  set4items$trust$labels %>% 
  arrange(itemnr)

## Select items corresponding to language settings
set4items$trust$labels <- 
  set4items$trust$labels[, paste("item", set4items$lang, sep = "_")]

## Create list of variable names
set4items$trust$varnames <-
  paste("trust", 
        sprintf("%02d", seq(1:length(set4items$trust$labels))), sep = "_")
