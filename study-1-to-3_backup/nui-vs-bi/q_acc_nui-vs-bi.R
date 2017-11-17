
# Preparatory settings ----------------------------------------------------
 
## Show box plots for single items?
showboxplot4items <- F

## Settings for table selection
name4dbsrc$q <- "acc"
name4dbsrc$prefix <- paste("t_q", name4dbsrc$q, sep = "_")
name4dbsrc$suffix <- 
  c("exp1_nui_n6",
    "exp1_bi_n6")



# Preprocessing -----------------------------------------------------------

source("preprocessing/dataPreparation.R")



# Visualisation -----------------------------------------------------------

# Boxplots ----

source("postprocessing/procedure4boxplots.R")

ggsave(filename = "acc_nu-vs-bi_boxplot_score.png",
       plot = plot(plotdata),
       path = "plots/boxplots", 
       width = 7.5, height = 6, units = "cm", dpi = 600)


## Item profiles ----

source("postprocessing/procedure4itemprofiles.R")

ggsave(filename = "q_acc_nui-vs-bi_itemprofile.png",
       plot = plotdata,
       path = "plots/itemprofiles",
       width = 15, height = 9 * 0.85, units = "cm", dpi = 600)