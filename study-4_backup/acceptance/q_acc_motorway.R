
# Preparatory settings ----------------------------------------------------

## Show box plots for single items?
showboxplot4items <- F

## Settings for table selection
name4dbsrc$q <- "acc"
name4dbsrc$prefix <- paste("t_q", name4dbsrc$q, sep = "_")
name4dbsrc$suffix <- 
  c("motorway_gestures_complex",
    "motorway_gestures_simple",
    "motorway_touch_complex",
    "motorway_touch_simple")


# Preprocessing -----------------------------------------------------------

source("preprocessing/dataPreparation.R")




# Boxplots ----------------------------------------------------------------

## Complete variation ----
plotdata <- plotBoxplot(name4data$df, "score", "score", ylim4score, "expfocus")
plotdata <- makePrintablePlot_boxPlot(plotdata)
plotdata <- makePrintablePlot_theme(plotdata)

source("postprocessing/adjustMarginGtable.R")

ggsave(filename = "q_acc_motorway_boxplot_expfocus.png",
       plot = plotdata,
       path = file.path(set4plot$dir4plot, "boxplots"), 
       width = 12.5, height = 6, units = "cm", dpi = 600)


## Touch vs. gestures ----
plotdata <- plotBoxplot(name4data$df, "score", "score", ylim4score, "typegeneric")
plotdata <- makePrintablePlot_boxPlot(plotdata)
plotdata <- makePrintablePlot_theme(plotdata)

source("postprocessing/adjustMarginGtable.R")

ggsave(filename = "q_acc_motorway_boxplot_typegeneric.png",
       plot = plotdata,
       path = file.path(set4plot$dir4plot, "boxplots"), 
       width = 12.5, height = 6, units = "cm", dpi = 600)



# Item profiles -----------------------------------------------------------

## Complete variation ----
plotdata <- plotItemProfile(name4data$df, "mean", "expfocus") 
plotdata <- makePrintablePlot_ItemProfile(plotdata)

ggsave(filename = "q_acc_motorway_itemprofile_expfocus.png",
       plot = plotdata,
       path = file.path(set4plot$dir4plot, "itemprofiles"),
       width = 15, height = 9 * 0.85, units = "cm", dpi = 600)


## Touch vs. gestures ----
plotdata <- plotItemProfile(name4data$df, "mean", "typegeneric") 
plotdata <- makePrintablePlot_ItemProfile(plotdata)

ggsave(filename = "q_acc_motorway_itemprofile_typegeneric.png",
       plot = plotdata,
       path = file.path(set4plot$dir4plot, "itemprofiles"),
       width = 15, height = 9 * 0.85, units = "cm", dpi = 600)
