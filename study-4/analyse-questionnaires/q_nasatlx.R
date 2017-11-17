
# Preparatory settings ----------------------------------------------------

set4proc <- c()

## Settings for local data
set4proc$name4df <- "t_q_nasatlx"
set4proc$q <- "nasatlx"

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
               set4proc$prefix, 
               set4proc$suffix, 
               set4proc$name4df)

## Remove subject #01 and #13
assign(set4proc$name4df, 
       get(set4proc$name4df) %>% 
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



# Compute Mean and SD -----------------------------------------------------

## Mean
means <- 
  sapply(names(set4items[[set4proc$q]]$itemnrs4scales), function (s) {
  varname4score <- paste("score_", s, sep = "")
  by(get(set4proc$name4df), get(set4proc$name4df)$table, 
     function(x) mean(x[, varname4score], na.rm = T))
})

by(get(set4proc$name4df), get(set4proc$name4df)$itype_generic, 
   function(x) mean(x[, "score_overall"], na.rm = T))

by(get(set4proc$name4df), get(set4proc$name4df)$ilevel, 
   function(x) mean(x[, "score_overall"], na.rm = T))

by(get(set4proc$name4df), get(set4proc$name4df)$scenario, 
   function(x) mean(x[, "score_overall"], na.rm = T))

## SD
sds <- 
  sapply(names(set4items[[set4proc$q]]$itemnrs4scales), function (s) {
  varname4score <- paste("score_", s, sep = "")
  by(get(set4proc$name4df), get(set4proc$name4df)$table, 
     function(x) sd(x[, varname4score], na.rm = T))
})

by(get(set4proc$name4df), get(set4proc$name4df)$itype_generic, 
   function(x) sd(x[, "score_overall"], na.rm = T))

by(get(set4proc$name4df), get(set4proc$name4df)$ilevel, 
   function(x) sd(x[, "score_overall"], na.rm = T))

by(get(set4proc$name4df), get(set4proc$name4df)$scenario, 
   function(x) sd(x[, "score_overall"], na.rm = T))

# Reliability -------------------------------------------------------------

## Original data
coll4rel <- c()

uniqetables <- unique(get(set4proc$name4df)$table)

invisible( lapply(uniqetables, function(t) {
  print(t)
  ## Get data
  #cat("\n\n"); outputSectionTitle(t)
  dat2proc <- get(set4proc$name4df) %>% filter(table == t)
  
  ## Identify scale names
  name4scales <- names(set4items[[set4proc$q]]$itemnrs4scales)
  
  ## Compute reliability
  rel <- psych::alpha(dat2proc[, set4proc$varnames4items])
  
  coll4rel_temp <- c()
  coll4rel_temp$table = t
  coll4rel_temp$raw_alpha = round(rel$total$raw_alpha, 3)
  coll4rel <<- rbind(coll4rel, coll4rel_temp)
  
}) )
row.names(coll4rel) <- NULL
print(coll4rel)



# Homoskedasticity (Homogeneity of variances) -----------------------------

bartlett.test(t_q_nasatlx[, "nasatlx_01"], get(set4proc$name4df)$table)
bartlett.test(t_q_nasatlx[, "nasatlx_02"], get(set4proc$name4df)$table)
bartlett.test(t_q_nasatlx[, "nasatlx_03"], get(set4proc$name4df)$table)
bartlett.test(t_q_nasatlx[, "nasatlx_04i"], get(set4proc$name4df)$table)
bartlett.test(t_q_nasatlx[, "nasatlx_05"], get(set4proc$name4df)$table)
bartlett.test(t_q_nasatlx[, "nasatlx_06"], get(set4proc$name4df)$table)

bartlett.test(log(t_q_nasatlx[, "nasatlx_01"]), get(set4proc$name4df)$table)
bartlett.test(log(t_q_nasatlx[, "nasatlx_02"]), get(set4proc$name4df)$table)
bartlett.test(log(t_q_nasatlx[, "nasatlx_03"]), get(set4proc$name4df)$table)
bartlett.test(log(t_q_nasatlx[, "nasatlx_04i"]), get(set4proc$name4df)$table)
bartlett.test(log(t_q_nasatlx[, "nasatlx_05"]), get(set4proc$name4df)$table)
bartlett.test(log(t_q_nasatlx[, "nasatlx_06"]), get(set4proc$name4df)$table)

fligner.test(get(set4proc$name4df)[, "nasatlx_01"], factor(get(set4proc$name4df)$table))
fligner.test(get(set4proc$name4df)[, "nasatlx_02"], factor(get(set4proc$name4df)$table))
fligner.test(get(set4proc$name4df)[, "nasatlx_03"], factor(get(set4proc$name4df)$table))
fligner.test(get(set4proc$name4df)[, "nasatlx_04i"], factor(get(set4proc$name4df)$table))
fligner.test(get(set4proc$name4df)[, "nasatlx_05"], factor(get(set4proc$name4df)$table))
fligner.test(get(set4proc$name4df)[, "nasatlx_06"], factor(get(set4proc$name4df)$table))



# Normality ---------------------------------------------------------------

by(t_q_nasatlx, t_q_nasatlx$table, function(x) shapiro.test(x$nasatlx_01))
by(t_q_nasatlx, t_q_nasatlx$table, function(x) shapiro.test(x$nasatlx_02))
by(t_q_nasatlx, t_q_nasatlx$table, function(x) shapiro.test(x$nasatlx_03))
by(t_q_nasatlx, t_q_nasatlx$table, function(x) shapiro.test(x$nasatlx_04i))
by(t_q_nasatlx, t_q_nasatlx$table, function(x) shapiro.test(x$nasatlx_05))
by(t_q_nasatlx, t_q_nasatlx$table, function(x) shapiro.test(x$nasatlx_06))



# Inference statistics ----------------------------------------------------

model_mental_demand <- 
  ezANOVA(data = t_q_nasatlx, 
          dv = nasatlx_01, 
          wid = id, 
          within = .(itype_generic, ilevel, scenario), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_mental_demand$ANOVA

model_physical_demand <- 
  ezANOVA(data = t_q_nasatlx, 
          dv = nasatlx_02, 
          wid = id, 
          within = .(itype_generic, ilevel, scenario), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_physical_demand$ANOVA

model_temporal_demand <- 
  ezANOVA(data = t_q_nasatlx, 
          dv = nasatlx_03, 
          wid = id, 
          within = .(itype_generic, ilevel, scenario), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_temporal_demand$ANOVA

model_performance <- 
  ezANOVA(data = t_q_nasatlx, 
          dv = nasatlx_04i, 
          wid = id, 
          within = .(itype_generic, ilevel, scenario), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_performance$ANOVA

model_effort <- 
  ezANOVA(data = t_q_nasatlx, 
          dv = nasatlx_05, 
          wid = id, 
          within = .(itype_generic, ilevel, scenario), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_effort$ANOVA

model_frustration <- 
  ezANOVA(data = t_q_nasatlx, 
          dv = nasatlx_06, 
          wid = id, 
          within = .(itype_generic, ilevel, scenario), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_frustration$ANOVA

model_overall <- 
  ezANOVA(data = t_q_nasatlx, 
          dv = score_overall, 
          wid = id, 
          within = .(itype_generic, ilevel, scenario), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_overall$ANOVA



# Visualisation: Item profiles --------------------------------------------

## Experimental focus
plotdat <- plotItemProfile(set4proc$name4df, "mean", "expfocus", set4proc$q) 
plotdat <- makePrintablePlot_ItemProfile(plotdat, set4proc$q)
grid.draw(plotdat)

## Touch vs. gestures ----
plotdat <- plotItemProfile(set4proc$name4df, "mean", "itype_generic", set4proc$q) 
plotdat <- makePrintablePlot_ItemProfile(plotdat, set4proc$q)
grid.draw(plotdat)





# Visualisation: Paper TBD ------------------------------------------------

t_q_nasatlx$itype_ilevel <- factor(t_q_nasatlx$itype_ilevel)
t_q_nasatlx$itype_generic <- factor(t_q_nasatlx$itype_generic, 
                                    labels = c("GBI", 
                                               "TBI"))
t_q_nasatlx$expfocus <- factor(t_q_nasatlx$expfocus)



print(unique(t_q_nasatlx$scenario_ilevel))

plotdat <- plotBoxplot(set4proc$name4df, "score_overall", "scenario_ilevel", c(2,1,4,3))
#plotdat <- makePrintablePlot_boxplot(plotdat, set4proc$q)
#plotdat <- makePrintablePlot_theme(plotdat)
#plotdat <- adjustLeftMargin(plotdat, 5)

plotdat <- 
  plotdat +
  facet_grid(.~itype_generic) + 
  theme_bw() + 
  guides(fill = F, colour = F, shape = F) + 
  ggtitle(label = "NASA-TLX overall score") + 
  labs(y = "Score (average)") + 
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 5, color = "black", angle = 45, vjust = 0.9, hjust = 0.7),
        axis.text.y = element_text(size = 5, color = "black")) + 
  theme(strip.text.x = element_text(size = 7, face = "bold")) + 
  coord_cartesian(ylim = c(0, 25))

plot(plotdat)

ggsave(filename = "nasatlx_overall.png", 
       plot = plotdat,
       path = "plots/PaperTBD_2017",
       width = 15 / 2,
       height = 5,
       units = "cm",
       dpi = 600)