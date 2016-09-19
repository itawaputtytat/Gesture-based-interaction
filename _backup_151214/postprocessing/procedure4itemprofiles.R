
if(name4dbsrc$q == "attrakdiff")
  plotdata <- plotItemProfile4attrakdiff(name4data$df, "mean", "expfocus") else
    plotdata <- plotItemProfile(name4data$df, "mean", "expfocus")

if(name4dbsrc$q == "nat") 
  plotdata <- plotItemProfile_post4nat()

plotdata <- makePrintablePlot_ItemProfile(plotdata)



