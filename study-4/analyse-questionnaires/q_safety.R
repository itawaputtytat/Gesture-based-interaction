
# Preparatory settings ----------------------------------------------------

set4proc <- c()

## Settings for local data
set4proc$q <- "safety"
set4proc$name4df <- "t_q_safety"
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

## Also: Completely remove #08 (did not answer questionnaire)
assign(set4proc$name4df, 
       get(set4proc$name4df) %>% 
         filter(id != 4008))

## Code missing cases (if not already NA)
codePatternAsNA(set4proc$name4df, 99, set4proc$varnames4items)



# Compute Mean and SD -----------------------------------------------------

# by(get(set4proc$name4df), get(set4proc$name4df)$table, 
#    function(x) mean(x[, "score_overall"], na.rm = T))
# 
# by(get(set4proc$name4df), get(set4proc$name4df)$table, 
#    function(x) sd(x[, "score_overall"], na.rm = T))



# Compute reliability (Cronbach's Alpha) ----------------------------------

## Get data
#cat("\n\n"); outputSectionTitle(t)
dat2proc <- get(set4proc$name4df) 

## Compute reliability  
colfinder <-set4items[[set4proc$q]]$varnames
rel <- psych::alpha(dat2proc[, colfinder])

print( round(rel$total$raw_alpha, 3) )



# Visualisation: Item profiles --------------------------------------------

# Compute scores
computeScores(set4proc$name4df, set4proc$q) 

## Create long data formats for item profiles
## Reshape to long data format for summarizing data
reshapeDat2Long(set4proc$name4df, set4proc$varnames4items)

## Compute key values for each item
computeKeyValues(set4proc$name4df)

set4items$safety$labels <- as.character(lapply(set4items$safety$labels, function(x) gsub("_", "\n", x) ))

plotdat <- plotItemProfile(set4proc$name4df, "mean", groupvar = NULL, set4proc$q, yscale_pos = "top") 
plotdat <- makePrintablePlot_ItemProfile(plotdat, set4proc$q)
grid.draw(plotdat)




# Visualisation: TBD ------------------------------------------------------

plotdat <- 
  plotdat +
  #coord_flip() + 
  theme_bw() + 
  guides(fill = F, colour = F, shape = F) + 
  ggtitle(label = "Impression of driving safely",
          subtitle = "I have the impression of driving safely when ...") + 
  theme(title = element_text(size = 7, face = "bold")) + 
  #theme(title = element_blank()) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black"))

plot(plotdat)

ggsave(filename = "distraction.png", 
       plot = plotdat,
       path = "plots/PaperTBD_2017",
       width = 15 / 2,
       height = 5,
       units = "cm",
       dpi = 600)



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