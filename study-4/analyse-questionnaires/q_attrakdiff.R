
# Preparatory settings ----------------------------------------------------

set4proc <- c()

## Settings for local data
set4proc$q <- "attrakdiff"
set4proc$name4df <- "t_q_attrakdiff"
set4proc$name4df.imp <- paste(set4proc$name4df, ".imp", sep = "")

## Settings for database table selection
set4proc$prefix <- paste("t_q", set4proc$q, sep = "_")
set4proc$suffix <- 
  c("gestures",
    "touch")

## Show box plots for single items?
set4proc$plot_boxplots4items <- F 

## Settings for questionnaire
#source("preprocessing/dataPreparation.R")
outputString(paste("* Loading item labels:", set4proc$q))
set4proc$varnames4items <- set4items[[set4proc$q]]$varnames



# Data pre-processing -----------------------------------------------------

## Load and rbind data from database
dbGetSrc_rbind("dbconn_study4", 
               set4proc$prefix, 
               set4proc$suffix, 
               set4proc$name4df)

## Remove subject #01 and #13
assign(set4proc$name4df, 
       get(set4proc$name4df) %>% 
         #filter(id != 4013 & id != 4001))
         filter(id != 4013))

## Code missing cases (if not already NA)
codePatternAsNA(set4proc$name4df, 99, set4proc$varnames4items)

# Code new variable for interaction type
codeExpFactors(set4proc$name4df, "table", 2)



# Compute scores and key values -------------------------------------------

# Compute scores
computeScores(set4proc$name4df, set4proc$q) 

## Create long data formats for item profiles
## Reshape to long data format for summarizing data
reshapeDat2Long(set4proc$name4df, set4proc$varnames4items)

## Compute key values for each item
computeKeyValues(set4proc$name4df, "expfocus")
computeKeyValues(set4proc$name4df, "itype_generic")



# Handling missing values -------------------------------------------------

## Initialise helper data
dat4imp <- get(set4proc$name4df)

## Identify cases with missing value
print(dat4imp$score_pq)
rowfinder <- which(is.na(dat4imp$score_pq))

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
dat4imp[which(dat4imp$table == nafinder_table), set4proc$varnames4items] <- 
  complete(impdata_temp, 1)
assign(set4proc$name4df.imp, dat4imp)



# Compute scores and key values for imputed data --------------------------

# Compute scores
computeScores(set4proc$name4df.imp, set4proc$q) 

## Create long data formats for item profiles
## Reshape to long data format for summarizing data
reshapeDat2Long(set4proc$name4df.imp, set4proc$varnames4items)

## Compute key values for each item
computeKeyValues(set4proc$name4df.imp, "expfocus")
computeKeyValues(set4proc$name4df.imp, "itype_generic")



# Compute Mean and SD -----------------------------------------------------

## Original data
sapply(names(set4items[[set4proc$q]]$itemnrs4scales), function (s) {
  varname4score <- paste("score_", s, sep = "")
  by(get(set4proc$name4df), get(set4proc$name4df)$table, 
     function(x) mean(x[, varname4score], na.rm = T))
})

sapply(names(set4items[[set4proc$q]]$itemnrs4scales), function (s) {
  varname4score <- paste("score_", s, sep = "")
  by(get(set4proc$name4df), get(set4proc$name4df)$table, 
     function(x) sd(x[, varname4score], na.rm = T))
})


## Imputed data
sapply(names(set4items[[set4proc$q]]$itemnrs4scales), function (s) {
  varname4score <- paste("score_", s, sep = "")
  by(get(set4proc$name4df.imp), get(set4proc$name4df.imp)$table, 
     function(x) mean(x[, varname4score], na.rm = T))
})

sapply(names(set4items[[set4proc$q]]$itemnrs4scales), function (s) {
  varname4score <- paste("score_", s, sep = "")
  by(get(set4proc$name4df.imp), get(set4proc$name4df.imp)$table, 
     function(x) sd(x[, varname4score], na.rm = T))
})



# 
# attrakdiff_means <- 
#   get(set4proc$name4df.imp) %>% 
#   group_by(table) %>% 
#   summarise(score_usefulness.avg = mean(score_usefulness),
#             score_satisfying.avg = mean(score_satisfying),
#             score_usefulness.sd = sd(score_usefulness),
#             score_satisfying.sd = sd(score_satisfying))
# 
# attrakdiff_means.itype <-
#   get(set4proc$name4df.imp) %>% 
#   mutate(itype = ifelse(grepl("gestures", table), 
#                         "gestures",
#                         "touch")) %>% 
#   group_by(itype) %>% 
#   summarise(score_usefulness.avg = mean(score_usefulness),
#             score_satisfying.avg = mean(score_satisfying),
#             score_usefulness.sd = sd(score_usefulness),
#             score_satisfying.sd = sd(score_satisfying))
# 
# attrakdiff_means.ilevel <-
#   get(set4proc$name4df.imp) %>% 
#   mutate(ilevel = ifelse(grepl("simple", table), 
#                          "simple",
#                          "complex")) %>% 
#   group_by(ilevel) %>% 
#   summarise(score_usefulness.avg = mean(score_usefulness),
#             score_satisfying.avg = mean(score_satisfying),
#             score_usefulness.sd = sd(score_usefulness),
#             score_satisfying.sd = sd(score_satisfying))
# 
# attrakdiff_means.scenario <-
#   get(set4proc$name4df.imp) %>% 
#   mutate(scenario = ifelse(grepl("city", table), 
#                            "city",
#                            "motorway")) %>% 
#   group_by(scenario) %>% 
#   summarise(score_usefulness.avg = mean(score_usefulness),
#             score_satisfying.avg = mean(score_satisfying),
#             score_usefulness.sd = sd(score_usefulness),
#             score_satisfying.sd = sd(score_satisfying))











# Compute reliability (Cronbach's Alpha) ----------------------------------

## Original data
coll4rel <- c()

invisible( lapply(unique(get(set4proc$name4df)$table), function(t) {
  
  ## Get data
  #cat("\n\n"); outputSectionTitle(t)
  dat2proc <- get(set4proc$name4df) %>% filter(table == t)
  
  ## Identify scale names
  name4scales <- names(set4items[[set4proc$q]]$itemnrs4scales)
  
  ## Compute reliability for each scale
  lapply(name4scales, function(n) {
    itemnrs <- set4items[[set4proc$q]]$itemnrs4scales[[n]]
    colfinder <- set4items[[set4proc$q]]$varnames[itemnrs]
    rel <- psych::alpha(dat2proc[, colfinder])
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
}) )
row.names(coll4rel) <- NULL
print(coll4rel)


## Imputed data
coll4rel.imputed <- c()

invisible( lapply(unique( get(set4proc$name4df.imp)$table), function(t) {
  
  ## Get data
  #cat("\n\n"); outputSectionTitle(t)
  dat2proc <- get(set4proc$name4df.imp) %>% filter(table == t)
  
  ## Identify scale names
  name4scales <- names(set4items[[set4proc$q]]$itemnrs4scales)
  
  ## Compute reliability for each scale
  lapply(name4scales, function(n) {
    
    itemnrs <- set4items[[set4proc$q]]$itemnrs4scales[[n]]
    colfinder <- set4items[[set4proc$q]]$varnames[itemnrs]
    rel <- psych::alpha(dat2proc[, colfinder])
    #cat("\n")
    #catWSepLine(n)
    #print(rel)
    
    coll4rel_temp <- c()
    coll4rel_temp$table = t
    coll4rel_temp$scale = n
    coll4rel_temp$raw_alpha = round(rel$total$raw_alpha, 3)
    coll4rel.imputed <<- rbind(coll4rel.imputed, coll4rel_temp)
  })
  #cat("\n\n\n")
}) )
row.names(coll4rel.imputed) <- NULL
print(coll4rel.imputed)



# Homoskedasticity (Homogeneity of variances) -----------------------------

bartlett.test(get(set4proc$name4df)[, "score_pq"], get(set4proc$name4df)$table)
bartlett.test(get(set4proc$name4df)[, "score_hq"], get(set4proc$name4df)$table)
bartlett.test(get(set4proc$name4df)[, "score_att"], get(set4proc$name4df)$table)

fligner.test(get(set4proc$name4df)[, "score_pq"], factor(get(set4proc$name4df)$table))
fligner.test(get(set4proc$name4df)[, "score_hq"], factor(get(set4proc$name4df)$table))
fligner.test(get(set4proc$name4df)[, "score_att"], factor(get(set4proc$name4df)$table))



# Normality ---------------------------------------------------------------

by(get(set4proc$name4df), get(set4proc$name4df)$table, function(x) shapiro.test(x$score_pq))
by(get(set4proc$name4df), get(set4proc$name4df)$table, function(x) shapiro.test(x$score_hq))
by(get(set4proc$name4df), get(set4proc$name4df)$table, function(x) shapiro.test(x$score_att))



# Inference statistics ----------------------------------------------------

model_pq <- 
  ezANOVA(data = get(set4proc$name4df.imp), 
          dv = score_pq, 
          wid = id, 
          within = .(itype_generic), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_pq$ANOVA

model_hqs <- 
  ezANOVA(data = get(set4proc$name4df.imp), 
          dv = score_hqs, 
          wid = id, 
          within = .(itype_generic), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_hqs$ANOVA

model_hqi <- 
  ezANOVA(data = get(set4proc$name4df.imp), 
          dv = score_hqi, 
          wid = id, 
          within = .(itype_generic), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_hqi$ANOVA

model_hq <- 
  ezANOVA(data = get(set4proc$name4df.imp), 
          dv = score_hq, 
          wid = id, 
          within = .(itype_generic), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_hq$ANOVA

model_att <- 
  ezANOVA(data = get(set4proc$name4df.imp), 
          dv = score_att, 
          wid = id, 
          within = .(itype_generic), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_att$ANOVA



# Visualisation: Item profiles --------------------------------------------

plotdat <- plotItemProfile_attrakdiff(set4proc$name4df, "mean", "itype_generic")
grid.draw(plotdat)



# Visualisation: Matrix ---------------------------------------------------

## Add variable for scale identification
addScaleInfo_attrakdiff(set4proc$name4df, ".imp.long")
#addScaleInfo_attrakdiff(set4proc$name4df, ".long.stats_expfocus")
#addScaleInfo_attrakdiff(set4proc$name4df, ".long.stats_itype_generic")

## Compute mean scores for PQ and HQ
#computeMeansSD_attrakdiff(set4proc$name4df, ".long", "expfocus")
computeMeansSD_attrakdiff(set4proc$name4df, ".imp.long", "expfocus")

## Reshape again
reshapeDat2Wide_attrakdiff_stats4scales(set4proc$name4df, ".imp.long.stats4scales", "expfocus")

plotdat <- plotMatrix_attrakdiff(set4proc$name4df, ".imp.long.stats4scales_wide", "expfocus")
grid.draw(plotdat)


plotdat <- 
  plotdat +
  ggtitle(label = "AttrakDiff: HQ vs. PQ for interaction type") + 
  theme(title = element_text(size = 7, face = "bold")) +
  theme(axis.title.x = element_text(size = 7, face = "bold"),
        axis.title.y = element_text(size = 7, face = "bold")) +
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black")) +
  theme(legend.title = element_text(size = 6)) + 
  theme(legend.text = element_text(size = 6))

ggsave(filename = "attrakdiff_matrix.png", 
       plot = plotdat,
       path = "plots/PaperTBD_2017",
       width = 15 / 2,
       height = 5,
       units = "cm",
       dpi = 600)