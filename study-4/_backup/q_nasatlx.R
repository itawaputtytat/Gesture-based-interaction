
# Preparatory settings ----------------------------------------------------

set4proc <- c()
set4proc$plot_boxplots4items <- F ## Show box plots for single items?

## Settings for table selection
set4proc$name4df <- "test_nasatlx"
set4proc$q <- "nasatlx"
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

#source("preprocessing/dataPreparation.R")
outputString(paste("* Loading item labels:", set4proc$q))
set4proc$varnames4items <- set4items$names[[set4proc$q]]



# Data processing ---------------------------------------------------------

qPreprocess()



# Visualisation: Item profiles --------------------------------------------

plotdat <- plotItemProfile(set4proc$name4df, "mean", "expfocus") 
grid.draw(plotdat)



# Reliability -------------------------------------------------------------

computeReliability()



# Visualisation: Boxplots -------------------------------------------------

plotdat <- plotBoxplot(set4proc$name4df, "score_overall", "expfocus")
grid.draw(plotdat)



# Inference statistics ----------------------------------------------------

model_nasatlx <- 
  ezANOVA(data = get(set4proc$name4df), 
          dv = score_overall, 
          wid = id, 
          within = .(scenario, itype, ilevel), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_nasatlx$ANOVA

## Insight:
## itype significant with F(1, 35) = 19.47 with p < 0.001 and generalised eta-squared 0.08
## ilevel: p = 0.081 with F(1, 35) and generalises eta-squared 0.00