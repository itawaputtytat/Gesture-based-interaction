
# Preparatory settings ----------------------------------------------------

set4proc <- c()

## Settings for local data
set4proc$q <- "acc"
set4proc$name4df <- "t_q_acc"
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

# Compute scores
computeScores(set4proc$name4df, set4proc$q) 

## Create long data formats for item profiles
## Reshape to long data format for summarizing data
reshapeDat2Long(set4proc$name4df, set4proc$varnames4items)

## Compute key values for each item
computeKeyValues(set4proc$name4df, "expfocus")
computeKeyValues(set4proc$name4df, "itype_generic")
computeKeyValues(set4proc$name4df, "itype_ilevel")



# Handling missing values -------------------------------------------------

## Initialise helper data
dat4imp <- get(set4proc$name4df)

## Identify cases with missing value
rowfinder <- which(is.na(dat4imp$score_satisfying))

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
computeKeyValues(set4proc$name4df.imp, "itype_ilevel")



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

acc_means <- 
  get(set4proc$name4df.imp) %>% 
  group_by(table) %>% 
  summarise(score_usefulness.avg = mean(score_usefulness),
            score_satisfying.avg = mean(score_satisfying),
            score_usefulness.sd = sd(score_usefulness),
            score_satisfying.sd = sd(score_satisfying))
  
acc_means.itype <-
  get(set4proc$name4df.imp) %>% 
  mutate(itype = ifelse(grepl("gestures", table), 
                        "gestures",
                        "touch")) %>% 
  group_by(itype) %>% 
  summarise(score_usefulness.avg = mean(score_usefulness),
            score_satisfying.avg = mean(score_satisfying),
            score_usefulness.sd = sd(score_usefulness),
            score_satisfying.sd = sd(score_satisfying))
acc_means.itype

acc_means.ilevel <-
  get(set4proc$name4df.imp) %>% 
  mutate(ilevel = ifelse(grepl("simple", table), 
                         "simple",
                         "complex")) %>% 
  group_by(ilevel) %>% 
  summarise(score_usefulness.avg = mean(score_usefulness),
            score_satisfying.avg = mean(score_satisfying),
            score_usefulness.sd = sd(score_usefulness),
            score_satisfying.sd = sd(score_satisfying))
acc_means.ilevel %>% data.frame()

acc_means.scenario <-
  get(set4proc$name4df.imp) %>% 
  mutate(scenario = ifelse(grepl("city", table), 
                         "city",
                         "motorway")) %>% 
  group_by(scenario) %>% 
  summarise(score_usefulness.avg = mean(score_usefulness),
            score_satisfying.avg = mean(score_satisfying),
            score_usefulness.sd = sd(score_usefulness),
            score_satisfying.sd = sd(score_satisfying))
acc_means.scenario %>% data.frame()

sapply(names(set4items[[set4proc$q]]$itemnrs4scales), function (s) {
  varname4score <- paste("score_", s, sep = "")
  by(get(set4proc$name4df.imp), get(set4proc$name4df.imp)$table, 
     function(x) sd(x[, varname4score], na.rm = T))
})



# Compute reliability (Cronbach's Alpha) ----------------------------------

## Original data
coll4rel <- c()

invisible( lapply(unique(t_q_acc$table), function(t) {
  
  ## Get data
  #cat("\n\n"); outputSectionTitle(t)
  dat2proc <- t_q_acc %>% filter(table == t)
  
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

invisible( lapply(unique(t_q_acc.imp$table), function(t) {
  
  ## Get data
  #cat("\n\n"); outputSectionTitle(t)
  dat2proc <- t_q_acc.imp %>% filter(table == t)
  
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

bartlett.test(t_q_acc$score_usefulness, t_q_acc$table)
bartlett.test(t_q_acc$score_satisfying, t_q_acc$table)

fligner.test(t_q_acc$score_usefulness, factor(t_q_acc$table))
fligner.test(t_q_acc$score_satisfying, factor(t_q_acc$table))



# Normality ---------------------------------------------------------------

by(t_q_acc, t_q_acc$table, function(x) shapiro.test(x$score_usefulness))



# Inference statistics ----------------------------------------------------

#model_usefulness <- ezANOVA(data = t_q_acc, dv = score_usefulness, wid = id, within = .(scenario, itype, ilevel), type = 3, detailed = F)
model_usefulness <- 
  ezANOVA(data = t_q_acc, 
          dv = score_usefulness, 
          wid = id, 
          within = .(itype_generic, ilevel, scenario), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_usefulness$ANOVA

## Insight:
## itype significant with F(1, 36) = 40.47 with p < 0.001 and generalised eta-squared 0.17

# pairwise.t.test(t_q_acc$score_usefulness, 
#                 t_q_acc$itype, 
#                 paired = T, 
#                 p.adjust.method = "bonferroni")


## Compute with complete case removal
# Re-compute scores
model_satisfying <- 
  ezANOVA(data = t_q_acc %>% filter(!id %in% nafinder_id), 
          dv = score_satisfying, 
          wid = id, 
          within = .(itype_generic, ilevel, scenario), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_satisfying$ANOVA

## Insight
## itype significant with F(1, 35) = 39.10 with p < 0.001 and generalised eta-squared 0.2


## Compute with imputation
model_satisfying <- 
  ezANOVA(data = t_q_acc.imp, 
          dv = score_satisfying, 
          wid = id, 
          within = .(itype_generic, ilevel, scenario), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_satisfying$ANOVA



# Visualisation: Item profiles --------------------------------------------

## Experimental focus
plotdat <- plotItemProfile(set4proc$name4df, "mean", "expfocus", set4proc$q) 
plotdat <- makePrintablePlot_ItemProfile(plotdat, set4proc$q)
grid.draw(plotdat)
# ---
plotdat <- plotItemProfile("t_q_acc.imp", "mean", "expfocus", set4proc$q) 
plotdat <- makePrintablePlot_ItemProfile(plotdat, set4proc$q)
grid.draw(plotdat)

## Touch vs. gestures ----
plotdat <- plotItemProfile(set4proc$name4df, "mean", "itype_generic", set4proc$q) 
plotdat <- makePrintablePlot_ItemProfile(plotdat, set4proc$q)
grid.draw(plotdat)
# ---
plotdat <- plotItemProfile("t_q_acc.imp", "mean", "itype_generic", set4proc$q) 
plotdat <- makePrintablePlot_ItemProfile(plotdat, set4proc$q)
grid.draw(plotdat)



# Visualisation: Boxplots -------------------------------------------------

## Complete variation ----
plotdat <- plotBoxplot(set4proc$name4df, "score_usefulness", "expfocus")
#plotdat <- makePrintablePlot_boxplot(plotdat, set4proc$q)
#plotdat <- makePrintablePlot_theme(plotdat)
#plotdat <- adjustLeftMargin(plotdat, 5)
plot(plotdat)

plotdat <- plotBoxplot(set4proc$name4df, "score_satisfying", "expfocus")
# plotdat <- makePrintablePlot_boxplot(plotdat, set4proc$q)
# plotdat <- makePrintablePlot_theme(plotdat)
# plotdat <- adjustLeftMargin(plotdat, 5)
plot(plotdat)


# ggsave(filename = "q_acc_city_boxplot_expfocus.png",
#        plot = plotdat,
#        path = set4plot$dir4plot, 
#        width = 8, height = 4, units = "cm", dpi = 600)


## Touch vs. gestures ----
plotdat <- plotBoxplot(set4proc$name4df, "score_overall", "itype_generic")
plotdat <- makePrintablePlot_boxplot(plotdat, set4proc$q)
plotdat <- makePrintablePlot_theme(plotdat)
plotdat <- adjustLeftMargin(plotdat, 5)
grid.draw(plotdat)
rm(plotdat)
# ggsave(filename = "q_acc_city_boxplot_typegeneric.png",
#        plot = plotdat,
#        path = set4plot$dir4plot, 
#        width = 8, height = 4, units = "cm", dpi = 600)



# Visualisation: Paper TBD ------------------------------------------------

backup <- t_q_acc.imp
t_q_acc.imp <- backup
t_q_acc.imp$itype_ilevel <- factor(t_q_acc.imp$itype_ilevel)
t_q_acc.imp$itype_generic <- factor(t_q_acc.imp$itype_generic, 
                                    # labels = c("Gesture-based interaction", 
                                    #            "Touch-based interaction"))
                                    labels = c("GBI", 
                                               "TBI"))



print(unique(t_q_acc.imp$scenario_ilevel))

plotdat <- plotBoxplot("t_q_acc.imp", "score_usefulness", "scenario_ilevel", c(2,1,4,3))
#plotdat <- makePrintablePlot_boxplot(plotdat, set4proc$q)
#plotdat <- makePrintablePlot_theme(plotdat)
#plotdat <- adjustLeftMargin(plotdat, 5)

plotdat <- 
  plotdat +
  facet_grid(.~itype_generic) + 
  theme_bw() + 
  guides(fill = F, colour = F, shape = F) + 
  ggtitle(label = "Acceptance score for scale 'usefulness'") + 
  labs(y = "Score (average)") + 
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 5, color = "black", angle = 45, vjust = 0.9, hjust = 0.7),
        axis.text.y = element_text(size = 5, color = "black")) + 
  theme(strip.text.x = element_text(size = 7, face = "bold")) +
  coord_cartesian(ylim = c(-2, 2))

plot(plotdat)

ggsave(filename = "acc_usefulness.png", 
       plot = plotdat,
       path = "plots/PaperTBD_2017",
       width = 15 / 2,
       height = 5,
       units = "cm",
       dpi = 600)




plotdat <- plotBoxplot("t_q_acc.imp", "score_satisfying", "scenario_ilevel", c(2,1,4,3))
#plotdat <- makePrintablePlot_boxplot(plotdat, set4proc$q)
#plotdat <- makePrintablePlot_theme(plotdat)
#plotdat <- adjustLeftMargin(plotdat, 5)

plotdat <- 
  plotdat +
  facet_grid(.~itype_generic) + 
  #coord_flip() + 
  theme_bw() + 
  guides(fill = F, colour = F, shape = F) + 
  ggtitle(label = "Acceptance score for scale 'satisfying'") + 
  labs(y = "Score (average)") + 
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 5, color = "black", angle = 45, vjust = 0.9, hjust = 0.7),
        axis.text.y = element_text(size = 5, color = "black")) + 
  theme(strip.text.x = element_text(size = 7, face = "bold")) +
  coord_cartesian(ylim = c(-2, 2))

plot(plotdat)

ggsave(filename = "acc_satisfying.png", 
       plot = plotdat,
       path = "plots/PaperTBD_2017",
       width = 15 / 2,
       height = 5,
       units = "cm",
       dpi = 600)

