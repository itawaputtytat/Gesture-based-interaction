## Boxplot for each item
# boxplot4Items()

# Boxplot for overall score
plotdata <- plotBoxplot(name4data$df, "score", "score", ylim4score, "expfocus")
plotdata

plotdata <- makePrintablePlot_boxPlot(plotdata)
plotdata
plotdata <- makePrintablePlot_theme(plotdata)
plotdata

source("postprocessing/adjustMarginGtable.R")
#plotdata <- adjustMarginGtable(plotdata)
plot(plotdata)

