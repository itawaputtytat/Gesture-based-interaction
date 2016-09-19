# Objective ---------------------------------------------------------------
# Draw template matrix plot for AttrakDiff 2


# Dummy -------------------------------------------------------------------
plotMatrix4attrakdiff_template <- 
  ggplot() +   
  #dummy
  geom_point(data = NULL,
             aes(x = 0,
                 y = 0))


# Rectangles --------------------------------------------------------------
for (r in c(1:9)) {
  plotMatrix4attrakdiff_template <- 
    plotMatrix4attrakdiff_template + 
    geom_rect(data = attrakdiff_rect,
              aes_string(xmin = attrakdiff_rect$xmin[r],
                         xmax = attrakdiff_rect$xmax[r],
                         ymin = attrakdiff_rect$ymin[r],
                         ymax = attrakdiff_rect$ymax[r]),
              fill = attrakdiff_rect$col[r])
}


# White lines between as borders fields -----------------------------------
plotMatrix4attrakdiff_template <- 
  plotMatrix4attrakdiff_template + 
  geom_vline(xintercept = c(-1, 1),
             colour = "white",
             size = 3) +
  geom_hline(yintercept = c(-1, 1),
             colour = "white",
             size = 3)


# Labels ------------------------------------------------------------------
for(l in 1:nrow(labels_attrakdiff_matrix)) {
  
  if (rownames(labels_attrakdiff_matrix)[l] == "bl")
    coltext <- "white" else
      coltext <- "black"
  
  plotMatrix4attrakdiff_template <- 
    plotMatrix4attrakdiff_template + 
    annotate("text", 
             label = labels_attrakdiff_matrix[l, "label_eng"],
             x =     labels_attrakdiff_matrix[l, "x"],
             y =      labels_attrakdiff_matrix[l, "y"],
             size = 7,
             colour = coltext,
             fontface = "bold")
}


# General appearance ------------------------------------------------------
plotMatrix4attrakdiff_template <- 
  plotMatrix4attrakdiff_template + 
  
  labs(x = "Pragmatic Quality (PQ)",
       y = "Hedonic Quality (HQ)") +
  
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) + 
  
  coord_cartesian(xlim = c(-3, 3),
                  ylim = c(-3, 3))