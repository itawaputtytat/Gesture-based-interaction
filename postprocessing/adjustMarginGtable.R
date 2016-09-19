
# Objective ---------------------------------------------------------------

## same outer margin in plots
require(gridExtra)


# Function ----------------------------------------------------------------

#adjustMarginGtable <- 
  
  #function (plotdata) {

  #plotdataname <- deparse(substitute(plotdata))

  plotdata <- ggplot_gtable(ggplot_build(plotdata))
  plotdata$widths[2:3] <- set4plot$gtable_width

  ggsave       <- ggplot2::ggsave
  body(ggsave) <- body(ggplot2::ggsave)[-2]

  plotdata <- arrangeGrob(plotdata)

#  return (plotdata)

#}
