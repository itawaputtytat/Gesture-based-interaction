# Objective ---------------------------------------------------------------
# Draw template matrix plot for taeg 2


# Dummy -------------------------------------------------------------------

plotItemProfile_taeg_template <- 
  #ggplot() + geom_point(data = NULL, aes(x = 0, y = 0), size = 0)
  ggplot() + geom_blank() 



# Split lines -------------------------------------------------------------

plotItemProfile_taeg_template <- 
  plotItemProfile_taeg_template + 

  geom_vline(aes(xintercept = 14.5),
             colour = "grey60",
             size = 0.5) +

  geom_vline(aes(xintercept = 10.5),
             colour = "grey60",
             size = 0.5) +
  
  geom_vline(aes(xintercept = 5.5),
             colour = "grey60",
             size = 0.5) 


# Rectangles for Subscales PQ, HQI, HQS and ATT ---------------------------

plotItemProfile_taeg_template <- 
  plotItemProfile_taeg_template +
  
  annotate("rect",
           xmin = 19+0.45,
           xmax = 19-4.45,
           ymin = -0.5,
           ymax = -0.4,
           fill = "skyblue2",
           alpha = 1) +
  annotate("rect",
           xmin = 14+0.45,
           xmax = 14-3.45,
           ymin = -0.5,
           ymax = -0.4,
           fill = "green2",
           alpha = 1) +
  annotate("rect",
           xmin = 10+0.45,
           xmax = 10-4.45,
           ymin = -0.5,
           ymax = -0.4,
           fill = "red2",
           alpha = 1) +
  annotate("rect",
           xmin = 5+0.45,
           xmax = 5-4.45,
           ymin = -0.5,
           ymax = -0.4,
           fill = "yellow1",
           alpha = 1) + 
  
  geom_text(aes(x = 19, y = -0.3), 
            label = "Excitement", fontface = "bold", hjust = 0, angle = 0) +
  geom_text(aes(x = 14, y = -0.3), 
            label = "Competence", fontface = "bold", hjust = 0, angle = 0) +
  geom_text(aes(x = 10, y = -0.3), 
            label = "- Attitude", fontface = "bold", hjust = 0, angle = 0) +
  geom_text(aes(x = 5, y = -0.3), 
            label = "+ Attitude", fontface = "bold", hjust = 0, angle = 0)
