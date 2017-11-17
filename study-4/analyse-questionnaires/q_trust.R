
# Preparatory settings ----------------------------------------------------

set4proc <- c()

## Settings for local data
set4proc$q <- "trust"
set4proc$name4df <- "t_q_trust"
set4proc$name4df.imp <- paste(set4proc$name4df, ".imp", sep = "")

## Settings for database table selection
set4proc$prefix <- paste("t_q", set4proc$q, sep = "_")
set4proc$suffix <- 
  c("city_gestures_complex",
    "city_gestures_simple",
    "city_touch_complex",
    "city_touch_simple",
    "motorway_gestures_complex",
    "motorway_gestures_simple",
    "motorway_touch_complex",
    "motorway_touch_simple")

## Show box plots for single items?
set4proc$plot_boxplots4items <- F

## Settings for questionnaire
#source("preprocessing/dataPreparation.R")
outputString(paste("* Loading item labels:", set4proc$q))
set4proc$varnames4items <- set4items[[set4proc$q]]$varnames



# Data pre-processing -----------------------------------------------------

## Load and rbind data from database
dbGetSrc_rbind("dbconn_study4", 
               set4proc$prefix, set4proc$suffix, 
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

# Reverse scores in the desired columns
cols2reverse <- set4proc$varnames4items[c(2, 3, 4, 5, 7)]
dat_temp <- get(set4proc$name4df)
dat_temp[, cols2reverse] = lapply(cols2reverse,  function(x) 8 - dat_temp[, x])
assign(set4proc$name4df, dat_temp)

# Compute scores
computeScores(set4proc$name4df, set4proc$q) 

## Create long data formats for item profiles
## Reshape to long data format for summarizing data
reshapeDat2Long(set4proc$name4df, set4proc$varnames4items)

## Compute key values for each item
computeKeyValues(set4proc$name4df, "expfocus")
computeKeyValues(set4proc$name4df, "itype_generic")
computeKeyValues(set4proc$name4df, "itype_ilevel")



# Compute Mean and SD -----------------------------------------------------

test <- by(get(set4proc$name4df), get(set4proc$name4df)$table, 
   function(x) mean(x[, "score_overall"], na.rm = T))

test <- by(get(set4proc$name4df), get(set4proc$name4df)$table, 
   function(x) sd(x[, "score_overall"], na.rm = T))



trust_means <- 
  get(set4proc$name4df) %>% 
  group_by(table) %>% 
  summarise(score.avg = mean(score_overall),
            score.sd = sd(score_overall))

trust_means.itype <-
  get(set4proc$name4df) %>% 
  mutate(itype = ifelse(grepl("gestures", table), 
                        "gestures",
                        "touch")) %>% 
  group_by(itype) %>% 
  summarise(score.avg = mean(score_overall),
            score.sd = sd(score_overall))
trust_means.itype %>% data.frame()

trust_means.ilevel <-
  get(set4proc$name4df) %>% 
  mutate(ilevel = ifelse(grepl("simple", table), 
                         "simple",
                         "complex")) %>% 
  group_by(ilevel) %>% 
  summarise(score.avg = mean(score_overall),
            score.sd = sd(score_overall))
trust_means.ilevel %>% data.frame()

trust_means.scenario <-
  get(set4proc$name4df) %>% 
  mutate(scenario = ifelse(grepl("city", table), 
                           "city",
                           "motorway")) %>% 
  group_by(scenario) %>% 
  summarise(score.avg = mean(score_overall),
            score.sd = sd(score_overall))
trust_means.scenario %>% data.frame()



# Compute reliability (Cronbach's Alpha) ----------------------------------

## Original data
coll4rel <- c()

invisible( lapply(unique(get(set4proc$name4df)$table), function(t) {
  
  ## Get data
  #cat("\n\n"); outputSectionTitle(t)
  dat2proc <- get(set4proc$name4df) %>% filter(table == t)
  
  ## Compute reliability  
  colfinder <-set4items[[set4proc$q]]$varnames
  rel <- psych::alpha(dat2proc[, colfinder])
  #cat("\n")
  #catWSepLine(n)
  #print(rel)
  
  coll4rel_temp <- c()
  coll4rel_temp$table = t
  coll4rel_temp$raw_alpha = round(rel$total$raw_alpha, 3)
  coll4rel <<- rbind(coll4rel, coll4rel_temp)
  
  #cat("\n\n\n")
}) )
row.names(coll4rel) <- NULL
print(coll4rel)





# Homoskedasticity (Homogeneity of variances) -----------------------------

bartlett.test(get(set4proc$name4df)[, "score_overall"], get(set4proc$name4df)$table)
fligner.test(get(set4proc$name4df)[, "score_overall"], factor(get(set4proc$name4df)$table))



# Normality ---------------------------------------------------------------

by(get(set4proc$name4df), get(set4proc$name4df)$table, function(x) shapiro.test(x$score_overall))



# Inference statistics ----------------------------------------------------

model_trust <- 
  ezANOVA(data = get(set4proc$name4df), 
          dv = score_overall, 
          wid = id, 
          within = .(itype_generic, ilevel, scenario), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_trust$ANOVA


# Visualisation: Item profiles --------------------------------------------

# Re-invert scores in the desired columns for item profile
cols2reverse <- set4proc$varnames4items[c(2, 3, 4, 5, 7)]
#cols2reverse <- set4proc$varnames4items[c(5)]
dat_temp <- get(set4proc$name4df)
dat_temp[, cols2reverse] = lapply(cols2reverse,  function(x) 8 - dat_temp[, x])
assign(set4proc$name4df, dat_temp)


## Experimental focus
plotdat <- plotItemProfile(set4proc$name4df, "mean", "expfocus", set4proc$q) 
plotdat <- makePrintablePlot_ItemProfile(plotdat, set4proc$q)
grid.draw(plotdat)

## Touch vs. gestures ----
plotdat <- plotItemProfile(set4proc$name4df, "mean", "itype_generic", set4proc$q) 
plotdat <- makePrintablePlot_ItemProfile(plotdat, set4proc$q)
grid.draw(plotdat)



# Visualisation: Boxplots -------------------------------------------------

## Complete variation ----
plotdat <- plotBoxplot(set4proc$name4df, "score_overall", "expfocus")
#plotdat <- makePrintablePlot_boxplot(plotdat, set4proc$q)
#plotdat <- makePrintablePlot_theme(plotdat)
#plotdat <- adjustLeftMargin(plotdat, 5)
plot(plotdat)

## Touch vs. gestures ----
plotdat <- plotBoxplot(set4proc$name4df, "score_overall", "itype_generic")
plotdat <- makePrintablePlot_boxplot(plotdat, set4proc$q)
plotdat <- makePrintablePlot_theme(plotdat)
plotdat <- adjustLeftMargin(plotdat, 5)
grid.draw(plotdat)
rm(plotdat)