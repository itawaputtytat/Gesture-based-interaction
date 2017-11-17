
# Preparatory settings ----------------------------------------------------

set4proc <- c()

## Settings for local data
set4proc$q <- "taeg"
set4proc$name4df <- "t_q_taeg"
set4proc$name4df.imp <- paste(set4proc$name4df, ".imp", sep = "")

## Settings for database table selection
set4proc$prefix <- paste("t_q", set4proc$q, sep = "_")
set4proc$suffix <- c()
## No suffix for safety (only one measure)

## Show box plots for single items?
set4proc$plot_boxplots4items <- F 

## Settings for questionnaire
#source("preprocessing/dataPreparation.R")
outputString(paste("* Loading item labels:", set4proc$q))
set4proc$varnames4items <- set4items[[set4proc$q]]$varnames



# Data pre-processing -----------------------------------------------------

## Load and rbind data from database
assign(set4proc$name4df,
       dbGetSrc("dbconn_study4", set4proc$prefix))

## Remove subject #01 and #13
assign(set4proc$name4df, 
       get(set4proc$name4df) %>% 
         #filter(id != 4013 & id != 4001))
         filter(id != 4013))

## Code missing cases (if not already NA)
codePatternAsNA(set4proc$name4df, 99, set4proc$varnames4items)

assign(set4proc$name4df,
       cbind(get(set4proc$name4df), table = set4proc$name4df))

## Reverse complete itemsets (as scale goes from 5 to 1)
cols2reverse <- set4proc$varnames4items
dat_temp <- get(set4proc$name4df)
dat_temp[, cols2reverse] = lapply(cols2reverse,  function(x) 6 - dat_temp[, x])
assign(set4proc$name4df, dat_temp)

## Reverse item 4 (scale competence)
cols2reverse <- set4proc$varnames4items[4]
dat_temp <- get(set4proc$name4df)
dat_temp[, cols2reverse] = lapply(cols2reverse,  function(x) 6 - dat_temp[, x])
assign(set4proc$name4df, dat_temp)

# # # Reverse scores in the desired columns 
# cols2reverse <- set4items[[set4proc$q]]$itemnrs4scales$attitude_neg
# cols2reverse <- c(4, cols2reverse)
# cols2reverse <- set4proc$varnames4items[cols2reverse]
# dat_temp <- get(set4proc$name4df)
# dat_temp[, cols2reverse] = lapply(cols2reverse,  function(x) 6 - dat_temp[, x])
# assign(set4proc$name4df, dat_temp)



# Compute scores and key values -------------------------------------------

# Compute scores
computeScores(set4proc$name4df, set4proc$q) 

## Create long data formats for item profiles
## Reshape to long data format for summarizing data
reshapeDat2Long(set4proc$name4df, set4proc$varnames4items)

## Compute key values for each item
computeKeyValues(set4proc$name4df, "table")



# Handling missing values -------------------------------------------------

## Initialise helper data
dat4imp <- get(set4proc$name4df)

## Identify cases with missing value
print(dat4imp$score_pq)
rowfinder <- which(is.na(dat4imp$score_overall))

## ID
nafinder_id <- dat4imp$id[rowfinder]
## Table
nafinder_table <- dat4imp$table[rowfinder]
#dat2proc <- t_q_acc %>% filter(!id %in% id_finder)

## Impute value for missing value
impdata_temp <- 
  dat4imp %>% 
  filter(table == nafinder_table) %>% 
  select_(.dots = set4proc$varnames4items)

impdata_temp <- 
  mice(impdata_temp, 
       m = 500, 
       seed = 42, 
       method = "pmm", 
       printFlag = F)

## Complete dataset
dat4imp[, set4proc$varnames4items] <- 
  complete(impdata_temp, 1)
assign(set4proc$name4df.imp, dat4imp)



# Compute scores and key values for imputed data --------------------------

# Compute scores
computeScores(set4proc$name4df.imp, set4proc$q) 

## Create long data formats for item profiles
## Reshape to long data format for summarizing data
reshapeDat2Long(set4proc$name4df.imp, set4proc$varnames4items)

## Compute key values for each item
computeKeyValues(set4proc$name4df.imp, "table")



# Compute Mean and SD -----------------------------------------------------

taeg_means <- 
  get(set4proc$name4df.imp) %>% 
  summarise(score_excitement.avg = mean(score_excitement),
            score_competence.avg = mean(score_competence),
            score_attitude_pos.avg = mean(score_attitude_pos),
            score_attitude_neg.avg = mean(score_attitude_neg),
            score_excitement.sd = sd(score_excitement),
            score_competence.sd = sd(score_competence),
            score_attitude_pos.sd = sd(score_attitude_pos),
            score_attitude_neg.sd = sd(score_attitude_neg))



# Compute reliability (Cronbach's Alpha) ----------------------------------

## Get data
#cat("\n\n"); outputSectionTitle(t)
dat2proc <- get(set4proc$name4df) 

## Compute reliability  
colfinder <-set4items[[set4proc$q]]$varnames
rel <- psych::alpha(dat2proc[, colfinder], check.keys = T) # CHECK.KEYS = T
rel <- psych::omega(dat2proc[, colfinder], check.keys = T)

print( round(rel$total$raw_alpha, 3) )


## Original data
coll4rel <- c()
coll4omega <- c()

  ## Identify scale names
  name4scales <- names(set4items[[set4proc$q]]$itemnrs4scales)
  
  ## Compute reliability for each scale
  lapply(name4scales, function(n) {
    itemnrs <- set4items[[set4proc$q]]$itemnrs4scales[[n]]
    colfinder <- set4items[[set4proc$q]]$varnames[itemnrs]
    rel <- psych::alpha(dat2proc[, colfinder], check.key = T)
    #cat("\n")
    #catWSepLine(n)
    #print(rel)
    
    coll4rel_temp <- c()
    coll4rel_temp$table = t
    coll4rel_temp$scale = n
    coll4rel_temp$raw_alpha = round(rel$total$raw_alpha, 3)
    coll4rel <<- rbind(coll4rel, coll4rel_temp)
  })
  #cat("\n\n\n")

row.names(coll4rel) <- NULL
print(coll4rel)



# Visualisation: Item profiles --------------------------------------------

# Re-reverse scores in the desired columns 
cols2reverse <- set4items[[set4proc$q]]$itemnrs4scales$attitude_neg
cols2reverse <- c(4, cols2reverse)
cols2reverse <- set4proc$varnames4items[cols2reverse]
dat_temp <- get(set4proc$name4df.imp)
dat_temp[, cols2reverse] = lapply(cols2reverse,  function(x) 6 - dat_temp[, x])
assign(set4proc$name4df.imp, dat_temp)

# Compute scores
computeScores(set4proc$name4df, set4proc$q) 

## Create long data formats for item profiles
## Reshape to long data format for summarizing data
reshapeDat2Long(set4proc$name4df.imp, set4proc$varnames4items)

## Compute key values for each item
computeKeyValues(set4proc$name4df.imp)

plotItemProfile_taeg(set4proc$name4df.imp, "mean", groupvar = NULL)

source("resources/plotItemProfile_taeg_template.R")
source("fun/plotItemProfile_taeg.R")
plotdat <- plotItemProfile_taeg(set4proc$name4df, "mean", groupvar = NULL)
grid.draw(plotdat)
