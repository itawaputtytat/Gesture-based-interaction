
# Preparatory settings ----------------------------------------------------

## Show box plots for single items?
showboxplot4items <- F

## Settings for table selection
name4dbsrc <- c()
name4dbsrc$q <- "acc"
name4dbsrc$prefix <- paste("t_q", name4dbsrc$q, sep = "_")
name4dbsrc$suffix <- 
  c("city_gestures_complex",
    "city_gestures_simple",
    "city_touch_complex",
    "city_touch_simple")


# Preprocessing -----------------------------------------------------------

source("preprocessing/dataPreparation.R")



# Boxplots ----------------------------------------------------------------

## Complete variation ----
plotdata <- plotBoxplot(name4data$df, "score", "score", ylim4score, "expfocus")
plotdata <- makePrintablePlot_boxPlot(plotdata)
plotdata <- makePrintablePlot_theme(plotdata)


#source("postprocessing/adjustMarginGtable.R")

#ggsave <- ggplot2::ggsave; body(ggsave) <- body(ggplot2::ggsave)[-2]
ggsave(filename = "q_acc_city_boxplot_expfocus.png",
       plot = plotdata,
       path = file.path(set4plot$dir4plot, "boxplots"), 
       width = 8, height = 4, units = "cm", dpi = 600)


## Touch vs. gestures ----
plotdata <- plotBoxplot(name4data$df, "score", "score", ylim4score, "typegeneric")
plotdata <- makePrintablePlot_boxPlot(plotdata)
plotdata <- makePrintablePlot_theme(plotdata)

source("postprocessing/adjustMarginGtable.R")

ggsave(filename = "q_acc_city_boxplot_typegeneric.png",
       plot = plotdata,
       path = file.path(set4plot$dir4plot, "boxplots"), 
       width = 8, height = 4, units = "cm", dpi = 600)



# Item profiles -----------------------------------------------------------

## Complete variation ----
plotdata <- plotItemProfile(name4data$df, "mean", "expfocus") 
plotdata <- makePrintablePlot_ItemProfile(plotdata)

ggsave(filename = "q_acc_city_itemprofile_expfocus.png",
       plot = plotdata,
       path = file.path(set4plot$dir4plot, "itemprofiles"),
       width = 15, height = 9 * 0.85, units = "cm", dpi = 600)


## Touch vs. gestures ----
plotdata <- plotItemProfile(name4data$df, "mean", "typegeneric") 
plotdata <- makePrintablePlot_ItemProfile(plotdata)

ggsave(filename = "q_acc_city_itemprofile_typegeneric.png",
       plot = plotdata,
       path = file.path(set4plot$dir4plot, "itemprofiles"),
       width = 15, height = 9 * 0.85, units = "cm", dpi = 600)
