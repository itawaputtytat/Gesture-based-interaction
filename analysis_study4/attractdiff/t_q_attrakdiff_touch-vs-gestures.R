
# Preparatory settings ----------------------------------------------------

## Show box plots for single items?
showboxplot4items <- F

## Settings for table selection
name4dbsrc$q <- "attrakdiff"
name4dbsrc$prefix <- paste("t_q", name4dbsrc$q, sep = "_")
name4dbsrc$suffix <- 
  c("gestures",
    "touch")


# Preprocessing -----------------------------------------------------------

source("preprocessing/dataPreparation.R")




# Boxplots ----------------------------------------------------------------

## Complete variation ----
plotdata <- plotBoxplot(name4data$df, "score", "score", ylim4score, "expfocus")
plotdata <- makePrintablePlot_boxPlot(plotdata)
plotdata <- makePrintablePlot_theme(plotdata)

source("postprocessing/adjustMarginGtable.R")

ggsave(filename = "q_attrakdiff_touch-vs-gestures_boxplot_expfocus.png",
       plot = plot(plotdata),
       path = file.path(set4plot$dir4plot, "boxplots"), 
       width = 12.5, height = 6, units = "cm", dpi = 600)



# Item-profile ------------------------------------------------------------

plotdata <- plotItemProfile4attrakdiff(name4data$df, "mean", "expfocus")
plotdata <- plotdata <- makePrintablePlot_ItemProfile(plotdata)

ggsave(filename = "q_attrakdiff_touch-vs-gestures_itemprofile_expfocus.png",
       plot = plotdata,
       path = file.path(set4plot$dir4plot, "itemprofiles"),
       width = 15, height = 28 * 0.7, units = "cm", dpi = 600)



# AttrakDiff-Matrix -------------------------------------------------------

plotdata <- plotMatrix4attrakdiff(name4data$df, "expfocus")

ggsave(filename = "q_attrakdiff_touch-vs-gestures_matrix_expfocus.png",
       plot = plotdata,
       path = file.path(set4plot$dir4plot, "itemprofiles"),
       width = 12, height = 6, units = "cm", dpi = 600)