
# Preparatory settings ----------------------------------------------------

set4proc <- c()

## Settings for local data
set4proc$q <- "eval"
set4proc$name4df <- "t_q_eval"
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

## Also: Completely remove #08, as there are no answers in the gestures block
assign(set4proc$name4df, 
       get(set4proc$name4df) %>% 
         filter(id != 4008))

## Code missing cases (if not already NA)
codePatternAsNA(set4proc$name4df, 99, set4proc$varnames4items)

# Code new variable for interaction type
codeExpFactors(set4proc$name4df, "table", 2)

# Reverse scores in the desired columns 
cols2reverse <- set4proc$varnames4items[c(2,3,4)]
dat_temp <- get(set4proc$name4df)
dat_temp[, cols2reverse] = lapply(cols2reverse,  function(x) 6 - dat_temp[, x])
assign(set4proc$name4df, dat_temp)



# Compute scores and key values -------------------------------------------

# Compute scores
computeScores(set4proc$name4df, set4proc$q) 

## Create long data formats for item profiles
## Reshape to long data format for summarizing data
reshapeDat2Long(set4proc$name4df, set4proc$varnames4items)

## Compute key values for each item
computeKeyValues(set4proc$name4df, "expfocus")
computeKeyValues(set4proc$name4df, "itype_generic")



# Compute Mean and SD -----------------------------------------------------

by(get(set4proc$name4df), get(set4proc$name4df)$table, 
   function(x) mean(x[, "score_overall"], na.rm = T))

by(get(set4proc$name4df), get(set4proc$name4df)$table, 
   function(x) sd(x[, "score_overall"], na.rm = T))



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

model_eval <- 
  ezANOVA(data = get(set4proc$name4df), 
          dv = score_overall, 
          wid = id, 
          within = .(itype_generic), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_eval$ANOVA




# Visualisation: Item profiles --------------------------------------------

## REQUIRES RE-INVERTES SCORES!

# Reverse scores in the desired columns 
cols2reverse <- set4proc$varnames4items[c(2,3,4)]
dat_temp <- get(set4proc$name4df)
dat_temp[, cols2reverse] = lapply(cols2reverse,  function(x) 6 - dat_temp[, x])
assign(set4proc$name4df, dat_temp)

# Compute scores
computeScores(set4proc$name4df, set4proc$q) 

## Create long data formats for item profiles
## Reshape to long data format for summarizing data
reshapeDat2Long(set4proc$name4df, set4proc$varnames4items)

## Compute key values for each item
computeKeyValues(set4proc$name4df, "expfocus")
computeKeyValues(set4proc$name4df, "itype_generic")


## Touch vs. gestures ----
set4items$eval$labels <- as.character(lapply(set4items$eval$labels, function(x) gsub("_", "\n", x) ))
plotdat <- plotItemProfile(set4proc$name4df, "mean", "itype_generic", set4proc$q, yscale_pos = "top") 
plotdat <- makePrintablePlot_ItemProfile(plotdat, set4proc$q)
grid.draw(plotdat)




# Visualisation: Paper TBD ------------------------------------------------



plotdat <- 
  plotdat +
  #coord_flip() + 
  theme_bw() + 
  #guides(fill = T, colour = T, shape = T) + 
  ggtitle(label = "System evaluation") + 
  #theme(title = element_text(size = 7, face = "bold")) + 
  #theme(title = element_blank()) + 
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black")) + 
  theme(legend.justification=c(0,1), legend.position=c(0.0,1.075)) + 
  theme(legend.text = element_text(size = 5),
        legend.key = element_blank(),
        legend.key.size = unit(0.5, 'lines')) + 
  theme(legend.background = element_blank())

plot(plotdat)

ggsave(filename = "evaluation.png", 
       plot = plotdat,
       path = "plots/PaperTBD_2017",
       width = 15 / 2,
       height = 5,
       units = "cm",
       dpi = 600)

