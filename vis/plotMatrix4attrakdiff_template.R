# Objective ---------------------------------------------------------------
# Draw template matrix plot for AttrakDiff 2


# Dummy -------------------------------------------------------------------
plotMatrix4attrakdiff_template <- 
  ggplot() +   
  #dummy
  geom_point(data = NULL,
             aes(x = 0,
                 y = 0)) + 
  theme_bw() + 
  theme(
    panel.border = element_rect(colour = "black"),
    plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), units="line"),
    legend.position = "right",
    legend.margin = unit(-1, "lines"),
    legend.key = element_blank(),
    legend.key.height = unit(0.85, "line"),
    #legend.title = element_blank(),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7))


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
             size = 1) +
  geom_hline(yintercept = c(-1, 1),
             colour = "white",
             size = 1)


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
             size = 2,
             colour = coltext,
             fontface = "bold")
}


# General appearance ------------------------------------------------------
plotMatrix4attrakdiff_template <- 
  plotMatrix4attrakdiff_template + 
  
  labs(x = "Pragmatic Quality (PQ)",
       y = "Hedonic Quality (HQ)",
       shape = set4plot$legend.title[[name4dbsrc$q]],
       colour = set4plot$legend.title[[name4dbsrc$q]]) +
  
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 8.5, face = "bold"),
        axis.title.y = element_text(size = 8.5, face = "bold"),
        plot.title = element_blank()) + 
  
  coord_cartesian(xlim = c(-3, 3),
                  ylim = c(-3, 3)) 