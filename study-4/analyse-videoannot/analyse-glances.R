
# Load data ---------------------------------------------------------------

dat_glances <- dbGetSrc("dbconn_study4", "t_videoannot_glances_final")
 

# Aggregate data ----------------------------------------------------------

dat_glances.aggr <- 
  dat_glances %>% 
  group_by(id) %>% 
  #filter(interface_nr != 1) %>% 
  # group_by(id, itype, ilevel, scenario) %>% 
  group_by(id, condition) %>% 
  summarise(glances_n.avg = mean(glances_n),
            dur_s.glance.avg = mean(dur_s.glance)) %>% 
  mutate(itype = ifelse(grepl("g", condition), "gestures", "touch"),
         ilevel = ifelse(grepl("e", condition), "simple", "complex"),
         scenario = ifelse(grepl("s", condition), "city", "motorway")) %>% 
  arrange(id, itype, ilevel, scenario) %>% 
  data.frame()



# Create complete data design ---------------------------------------------

dat_glances.complete_template <- 
  expand.grid(
    id = unique(dat_glances.aggr$id),
    itype = unique(dat_glances.aggr$itype),
    ilevel = unique(dat_glances.aggr$ilevel),
    scenario = unique(dat_glances.aggr$scenario))

dat_glances.aggr.complete <- 
  left_join(dat_glances.complete_template,
            dat_glances.aggr)

## Replace missing values with zero
rowfinder <- is.na(dat_glances.aggr.complete$glances_n.avg)
dat_glances.aggr.complete$glances_n.avg[rowfinder] <- 0

rowfinder <- is.na(dat_glances.aggr.complete$dur_s.glance.avg)
dat_glances.aggr.complete$dur_s.glance.avg[rowfinder] <- 0

dat_glances.aggr.complete <- 
  dat_glances.aggr.complete %>% 
  arrange(id, itype, ilevel, scenario)


dat_glances.aggr.complete <- 
  dat_glances.aggr.complete %>% 
  mutate(condition = 
           ifelse(grepl("gestures", itype) & grepl("simple", ilevel) & grepl("city", scenario),
                  "gse", NA)) %>% 
  mutate(condition = 
           ifelse(grepl("gestures", itype) & grepl("complex", ilevel) & grepl("city", scenario),
                  "gsk", condition)) %>% 
  mutate(condition = 
           ifelse(grepl("gestures", itype) & grepl("simple", ilevel) & grepl("motorway", scenario),
                  "gae", condition)) %>% 
  mutate(condition = 
           ifelse(grepl("gestures", itype) & grepl("complex", ilevel) & grepl("motorway", scenario),
                  "gak", condition)) %>% 
  mutate(condition = 
           ifelse(grepl("touch", itype) & grepl("simple", ilevel) & grepl("city", scenario),
                  "tse", condition)) %>% 
  mutate(condition = 
           ifelse(grepl("touch", itype) & grepl("complex", ilevel) & grepl("city", scenario),
                  "tsk", condition)) %>% 
  mutate(condition = 
           ifelse(grepl("touch", itype) & grepl("simple", ilevel) & grepl("motorway", scenario),
                  "tae", condition)) %>% 
  mutate(condition = 
           ifelse(grepl("touch", itype) & grepl("complex", ilevel) & grepl("motorway", scenario),
                  "tak", condition))



# Factorial repeated measures for number of glances as ANOVA --------------

## ez
## Corresponds to results in SPSS
model_glances_n <- 
  ezANOVA(data = dat_glances.aggr.complete, 
          dv = .(glances_n.avg), 
          wid = .(id), 
          within = .(itype, ilevel, scenario), 
          type = 3, 
          detailed = F,
          return_aov = T)
model_glances_n$ANOVA

## aov
summary(aov(glances_n.avg ~ itype*ilevel*scenario + Error(id/itype*ilevel*scenario), 
            data = dat_glances.aggr.complete))




# Factorial repeated measures as GLM for glance duration ------------------

library(nlme)

dat_glances.aggr2 <- dat_glances.aggr
dat_glances.aggr2$itype <- as.factor(dat_glances.aggr2$itype)
dat_glances.aggr2$ilevel <- as.factor(dat_glances.aggr2$ilevel)
dat_glances.aggr2$scenario <- as.factor(dat_glances.aggr2$scenario)

## No predictor
model_baseline <-
  lme(dur_s.glance.avg ~ 1, 
      random = ~1|id/itype/ilevel/scenario, 
      data = dat_glances.aggr2, 
      method = "ML")

## Adding main effects
model_baseline.itype <-    update(model_baseline, .~. + itype)
model_baseline.ilevel <-   update(model_baseline.itype, .~. + ilevel)
model_baseline.scenario <- update(model_baseline.ilevel, .~. + scenario)

## Adding each interaction combination
model_baseline.itype.ilevel <- 
  update(model_baseline.scenario, .~. + itype:ilevel)

model_baseline.itype.scenario <- 
  update(model_baseline.itype.ilevel, .~. + itype:scenario)

model_baseline.ilevel.scenario <- 
  update(model_baseline.itype.scenario, .~. + ilevel:scenario)

model_baseline.itype.ilevel.scenario <- 
  update(model_baseline.ilevel.scenario, .~. + itype:ilevel:scenario)

## Compare models
model_comp <- 
  anova(model_baseline,
        model_baseline.itype,
        model_baseline.ilevel,
        model_baseline.scenario,
        model_baseline.itype.ilevel,
        model_baseline.itype.scenario,
        model_baseline.ilevel.scenario,
        model_baseline.itype.ilevel.scenario)

anova(model_baseline.itype.ilevel.scenario)
summary(model_baseline.itype.ilevel.scenario)
intervals(model_baseline.itype.ilevel.scenario, 0.95)
intervals(model_baseline.itype.ilevel.scenario, 0.95, which = "fixed")

summary(aov(dur_s.glance.avg ~ itype*ilevel*scenario + Error(id/(itype*ilevel*scenario)), data=dat_glances.aggr))

library(lme4)

model_lme4 <- lmer(dur_s.glance.avg ~ itype*ilevel*scenario + (1|id), data=dat_glances.aggr) 
anova((model_lme4))
summary(model_lme4)

model_lme4 <- 
  lmer(dur_s.glance.avg ~ 
         1 + itype*ilevel*scenario + (itype*ilevel*scenario|id), dat_glances.aggr)
anova((model_lme4))
summary(model_lme4)


model_lme4 <- lmer(dur_s.glance.avg ~ itype*ilevel*scenario + (1|id), data = dat_glances.aggr)
anova((model_lme4))
summary(model_lme4)


model_lme4 <- 
  lmer(dur_s.glance.avg ~ itype*ilevel + ilevel*scenario + itype*ilevel*scenario + 
       (1|id) + (1|itype:id) + (1|ilevel:id) +  + (1|ilevel:scenario), data = dat_glances.aggr)

anova((model_lme4))
summary(model_lme4)





pairwise.t.test(dat_glances.aggr.complete$dur_s.glance.avg, 
                dat_glances.aggr.complete$condition, 
                paired = TRUE, 
                p.adjust.method = "bonferroni")


# ezANOVA(data = dat_glances.aggr.complete, 
#         dv = .(dur_s.glance.avg), 
#         wid = .(id), 
#         between = .(itype, ilevel, scenario), 
#         type = 3, 
#         detailed = T,
#         return_aov = T)






# Visualisation: Boxplot for average glance duration ----------------------

dat2plot$dur_s.glance.outlier <- codeOutliersZ(dat_joined.task_nr.aggr.proc.filtered$dur_s.glance)
dat2plot$glances_n.outlier <- codeOutliersZ(dat_joined.task_nr.aggr.proc.filtered$glances_n)

ggplot() + 
  geom_boxplot(data = dat2plot, 
               aes_string(x = "condition", 
                          y = "dur_s.glance",
                          fill = "condition"))

ggplot() + 
  geom_boxplot(data = dat2plot, 
               aes_string(x = "condition", 
                          y = "dur_s.glance.outlier",
                          fill = "condition"))

ggplot() + 
  geom_boxplot(data = dat2plot, 
               aes_string(x = "condition", 
                          y = "glances_n",
                          fill = "condition"))

ggplot() + 
  geom_boxplot(data = dat2plot, 
               aes_string(x = "condition", 
                          y = "glances_n.outlier",
                          fill = "condition"))




# Visualisation: Function glanceExplorer ----------------------------------

glanceExplorer <- function(dat2proc, group, keyval, varinterest, glancefilter = NULL) {
#glanceExplorer <- function(dat2proc, group, keyval, varinterest) {
  
  keyval <- paste("x.", keyval, sep = "")
  
  dat2proc.aggr <- 
    dat2proc %>% 
    group_by_(.dots = group) 
  
  if (!is.null(glancefilter))
    dat2proc.aggr <- 
    dat2proc.aggr %>% 
    filter_(paste(glancefilter, "==", 1))
  
  dat2proc.aggr <- 
    dat2proc.aggr %>% 
    summarise_(x.n = "n()",
               x.sum = paste("sum(", varinterest, ", na.rm = T)"),
               x.avg = paste("mean(", varinterest, ", na.rm = T)"),
               #x.med = paste("median(", varinterest, ", na.rm = T)"),
               x.sd = paste("sd(", varinterest, ", na.rm = T)"),
               x.min = paste("min(", varinterest, ", na.rm = T)"),
               x.max = paste("max(", varinterest, ", na.rm = T)")) %>% 
    mutate(x.se = x.sd / sqrt(x.n)) %>% 
    mutate(upper = x.avg + x.se,
           lower = x.avg - x.se) %>% 
    data.frame()
  
  print(dat2proc.aggr)
  
  if (grepl("dur", varinterest)) 
    scalelim_y <- c(0, 2)
  if (grepl("glances_n", varinterest)) 
    scalelim_y <- c(0, 5)
  
  dodge <- position_dodge(width = 0.9)
  
  dat2proc.aggr$groupvar <- group[1]
  
  if (length(group) >= 2)
    dat2proc.aggr$facet1 <- dat2proc.aggr[, group[2]]
  
  if (length(group) == 3)
    dat2proc.aggr$facet2 <- dat2proc.aggr[, group[3]]
    
  plotdat <- 
    ggplot() + 
    geom_bar(data = dat2proc.aggr, 
             aes_string(x = group, 
                        y = keyval,
                        fill = group), 
             stat = "identity") +
    #coord_cartesian(ylim = scalelim_y) + 
    labs(x = group,
         y = paste(varinterest, keyval, sep = ".")) + 
    ggtitle(paste("Group:", group, "; Keyval:", keyval, "; Filtered for:", glancefilter))
  
  if (length(group) == 2)
    plotdat <- 
      plotdat +
      facet_grid(.~facet1)
  
  if (length(group) == 3)
    plotdat <- 
      plotdat +
      facet_grid(facet2~facet1)
  
  plot(plotdat)
  return(list(dat2proc.aggr, plotdat))
}



## glancefilter
# task_nr.glance
# task_nr.glance.v2
# task_nr.glance.all



# Visualisation: Average glance duration ----------------------------------

dat2plot <- dat_glances

glanceExplorer(dat2plot, c("task", "itype"), "avg", "dur_s.glance", "task_nr.glance")
glanceExplorer(dat2plot, c("task_nr", "itype"), "avg", "dur_s.glance", "task_nr.glance")
glanceExplorer(dat2plot, c("interface_nr", "itype"), "avg", "dur_s.glance", "task_nr.glance")
glanceExplorer(dat2plot, c("interface_nr", "task_nr"), "avg", "dur_s.glance", "task_nr.glance")
glanceExplorer(dat2plot, "interface_nr", "avg", "dur_s.glance", "task_nr.glance")
glanceExplorer(dat2plot, c("condition", "itype"), "avg", "dur_s.glance", "task_nr.glance")
glanceExplorer(dat2plot, "condition", "avg", "dur_s.glance.outlier", "task_nr.glance")
glanceExplorer(dat2plot, "itype", "avg", "dur_s.glance", "task_nr.glance")
glanceExplorer(dat2plot, "itype", "avg", "dur_s.glance.outlier", "task_nr.glance")
glanceExplorer(dat2plot, "ilevel", "avg", "dur_s.glance", "task_nr.glance")
glanceExplorer(dat2plot, "scenario", "avg", "dur_s.glance", "task_nr.glance")

glanceExplorer(dat2plot, "task", "avg", "dur_s.glance", "task_nr.glance_v2")
glanceExplorer(dat2plot, "interface_nr", "avg", "dur_s.glance", "task_nr.glance.v2")
glanceExplorer(dat2plot, "condition", "avg", "dur_s.glance", "task_nr.glance.v2")
glanceExplorer(dat2plot, "itype", "avg", "dur_s.glance", "task_nr.glance.v2")
glanceExplorer(dat2plot, "ilevel", "avg", "dur_s.glance", "task_nr.glance.v2")
glanceExplorer(dat2plot, "scenario", "avg", "dur_s.glance", "task_nr.glance.v2")

glanceExplorer(dat2plot, "task", "avg", "dur_s.glance", "task_nr.glance.all")
glanceExplorer(dat2plot, "interface_nr", "avg", "dur_s.glance", "task_nr.glance.all")
glanceExplorer(dat2plot, "condition", "avg", "dur_s.glance", "task_nr.glance.all")
glanceExplorer(dat2plot, "itype", "avg", "dur_s.glance", "task_nr.glance.all")
glanceExplorer(dat2plot, "ilevel", "avg", "dur_s.glance", "task_nr.glance.all")
glanceExplorer(dat2plot, "scenario", "avg", "dur_s.glance", "task_nr.glance.all")



# Visualisation: Number of glances ----------------------------------------

glanceExplorer(dat2plot, c("task", "itype"), "avg", "glances_n", "task_nr.glance")
glanceExplorer(dat2plot, c("task_nr", "itype"), "avg", "glances_n", "task_nr.glance")
glanceExplorer(dat2plot, c("interface_nr", "itype"), "avg", "glances_n", "task_nr.glance")
glanceExplorer(dat2plot, c("interface_nr", "task_nr"), "avg", "glances_n", "task_nr.glance")
glanceExplorer(dat2plot, c("interface_nr", "task_nr", "itype"), "avg", "glances_n", "task_nr.glance")
glanceExplorer(dat2plot, "interface_nr", "avg", "glances_n", "task_nr.glance")
glanceExplorer(dat2plot, "condition", "avg", "glances_n", "task_nr.glance")
glanceExplorer(dat2plot, "condition", "avg", "glances_n.outlier", "task_nr.glance")
glanceExplorer(dat2plot, "itype", "avg", "glances_n", "task_nr.glance")
glanceExplorer(dat2plot, "itype", "avg", "glances_n.outlier", "task_nr.glance")
glanceExplorer(dat2plot, "ilevel", "avg", "glances_n", "task_nr.glance")
glanceExplorer(dat2plot, "scenario", "avg", "glances_n", "task_nr.glance")

glanceExplorer(dat2plot, "task", "avg", "glances_n", "task_nr.glance.v2")
glanceExplorer(dat2plot, "interface_nr", "avg", "glances_n", "task_nr.glance.v2")
glanceExplorer(dat2plot, "condition", "avg", "glances_n", "task_nr.glance.v2")
glanceExplorer(dat2plot, "itype", "avg", "glances_n", "task_nr.glance.v2")
glanceExplorer(dat2plot, "ilevel", "avg", "glances_n", "task_nr.glance.v2")
glanceExplorer(dat2plot, "scenario", "avg", "glances_n", "task_nr.glance.v2")

glanceExplorer(dat2plot, "task", "avg", "glances_n", "task_nr.glance.all")
glanceExplorer(dat2plot, "interface_nr", "avg", "glances_n", "task_nr.glance.all")
glanceExplorer(dat2plot, "condition", "avg", "glances_n", "task_nr.glance.all")
glanceExplorer(dat2plot, "itype", "avg", "glances_n", "task_nr.glance.all")
glanceExplorer(dat2plot, "ilevel", "avg", "glances_n", "task_nr.glance.all")
glanceExplorer(dat2plot, "scenario", "avg", "glances_n", "task_nr.glance.all")





# Visualisation: TBD ------------------------------------------------------

## Try: Boxplots
ggplot() + 
  geom_boxplot(data = dat_glances.aggr.complete,
               aes(x = condition,
                   y = glances_n.avg,
                   fill = condition))

ggplot() + 
  geom_boxplot(data = dat_glances,
               aes(x = condition,
                   y = dur_s.glance,
                   fill = condition))

## Result: Too mache wasted space in visualisation
## Try: Barplot



# Visualization: TBD #2 ---------------------------------------------------

## Show example
plotdat_temp <- 
  glanceExplorer(dat_glances.aggr.complete, 
                 c("condition", "itype"), 
                 "avg", 
                 "glances_n.avg")

## Aggregate data (complete)
dat2plot <-
  dat_glances.aggr.complete %>% 
  group_by(condition, itype) %>% 
  summarise_(x.n = "n()",
             x.sum = paste("sum(", "glances_n.avg", ", na.rm = T)"),
             x.avg = paste("mean(", "glances_n.avg", ", na.rm = T)"),
             #x.med = paste("median(", varinterest, ", na.rm = T)"),
             x.sd = paste("sd(", "glances_n.avg", ", na.rm = T)"),
             x.min = paste("min(", "glances_n.avg", ", na.rm = T)"),
             x.max = paste("max(", "glances_n.avg", ", na.rm = T)")) %>% 
  mutate(x.se = x.sd / sqrt(x.n)) %>% 
  mutate(upper = x.avg + x.se,
         lower = x.avg - x.se) %>% 
  data.frame()
  
## Aggregate data without zero glances
dat2plot_wo0glances <-
  dat_glances.aggr.complete %>% 
  group_by(condition, itype) %>% 
  filter(glances_n.avg != 0) %>% 
  summarise_(x.n = "n()",
             x.sum = paste("sum(", "glances_n.avg", ", na.rm = T)"),
             x.avg = paste("mean(", "glances_n.avg", ", na.rm = T)"),
             #x.med = paste("median(", varinterest, ", na.rm = T)"),
             x.sd = paste("sd(", "glances_n.avg", ", na.rm = T)"),
             x.min = paste("min(", "glances_n.avg", ", na.rm = T)"),
             x.max = paste("max(", "glances_n.avg", ", na.rm = T)")) %>% 
  mutate(x.se = x.sd / sqrt(x.n)) %>% 
  mutate(upper = x.avg + x.se,
         lower = x.avg - x.se) %>% 
  data.frame()

## Create strip character
dat2plot$itype <- factor(dat2plot$itype, labels = c("GBI", "TBI"))
dat2plot_wo0glances$itype <- factor(dat2plot_wo0glances$itype, labels = c("GBI", "TBI"))

## Create variable used for labelling
dat2plot <- 
  dat2plot %>% 
  mutate(scenario_ilevel = 
           ifelse(grepl("ae", condition), "motorway_simple", "")) %>% 
  mutate(scenario_ilevel = 
           ifelse(grepl("ak", condition), "motorway_complex", scenario_ilevel)) %>% 
  mutate(scenario_ilevel = 
           ifelse(grepl("se", condition), "city_simple", scenario_ilevel)) %>% 
  mutate(scenario_ilevel = 
           ifelse(grepl("sk", condition), "city_complex", scenario_ilevel))

dat2plot_wo0glances <- 
  dat2plot_wo0glances %>% 
  mutate(scenario_ilevel = 
           ifelse(grepl("ae", condition), "motorway_simple", "")) %>% 
  mutate(scenario_ilevel = 
           ifelse(grepl("ak", condition), "motorway_complex", scenario_ilevel)) %>% 
  mutate(scenario_ilevel = 
           ifelse(grepl("se", condition), "city_simple", scenario_ilevel)) %>% 
  mutate(scenario_ilevel = 
           ifelse(grepl("sk", condition), "city_complex", scenario_ilevel)) %>% 
  arrange()

# Set right colours
colset_order <- c("city_simple", "city_complex", "motorway_simple", "motorway_complex")
colset4groupvar <- unlist(set4plot$colour[["scenario_ilevel"]])
colset4groupvar <- as.character(colset4groupvar[colset_order])

## Create plot
plotdat <- 
  ggplot() + 
  geom_bar(data = dat2plot_wo0glances,
           aes(x = scenario_ilevel,
               y = x.avg,
               fill = scenario_ilevel),
           stat = "identity",
           alpha = 0.5) + 
  geom_errorbar(data = dat2plot_wo0glances,
                aes(x = scenario_ilevel, ymin = upper, ymax = lower,
                    col = scenario_ilevel), 
                width = 0.2,
                alpha = 0.35) + 
  geom_bar(data = dat2plot,
           aes(x = scenario_ilevel,
               y = x.avg,
               fill = scenario_ilevel),
           stat = "identity") + 
  geom_errorbar(data = dat2plot,
                aes(x = scenario_ilevel, ymin = upper, ymax = lower), 
                width = 0.2) + 
  facet_grid(.~itype, scales = "free") + 
  coord_cartesian(ylim = c(-0, 4.25)) +
  scale_x_discrete(limits = colset_order,
                   labels = createLabels4PlotLegend("dat2plot_wo0glances", "scenario_ilevel", c(3,4,1,2))) + 
  scale_y_continuous(expand = c(0, 0)) + 
  scale_fill_manual(values = rep(colset4groupvar, 2),
                    labels = createLabels4PlotLegend("dat2plot_wo0glances", "scenario_ilevel", c(3,4,1,2))) + 
  scale_colour_manual(values = rep(colset4groupvar, 2),
                    labels = createLabels4PlotLegend("dat2plot_wo0glances", "scenario_ilevel", c(3,4,1,2))) + 
  theme_bw() + 
  guides(fill = F, colour = F, shape = F) + 
  ggtitle(label = "Average number of gazes towards display") + 
          #subtitle = "(Graduation without zero-number gazes)") + 
  labs(y = "Average number of glances") + 
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 5, color = "black", angle = 45, vjust = 0.9, hjust = 0.7),
        axis.text.y = element_text(size = 5, color = "black")) + 
  theme(strip.text.x = element_text(size = 7, face = "bold")) 
  

plot(plotdat)

ggsave(filename = "glances_n.png", 
       plot = plotdat,
       path = "plots/PaperTBD_2017",
       width = 15 / 2,
       height = 5,
       units = "cm",
       dpi = 600)




# Visualization: TBD glance duration --------------------------------------

## Example
glanceExplorer(dat_glances, "condition", "avg", "dur_s.glance", "task_nr.glance")

## Aggregate data (complete)
dat2plot <-
  dat_glances %>% 
  group_by(condition, itype) %>% 
  filter(task_nr.glance == 1) %>% 
  summarise_(x.n = "n()",
             x.sum = paste("sum(", "dur_s.glance  ", ", na.rm = T)"),
             x.avg = paste("mean(", "dur_s.glance  ", ", na.rm = T)"),
             #x.med = paste("median(", varinterest, ", na.rm = T)"),
             x.sd = paste("sd(", "dur_s.glance  ", ", na.rm = T)"),
             x.min = paste("min(", "dur_s.glance  ", ", na.rm = T)"),
             x.max = paste("max(", "dur_s.glance  ", ", na.rm = T)")) %>% 
  mutate(x.se = x.sd / sqrt(x.n)) %>% 
  mutate(upper = x.avg + x.se,
         lower = x.avg - x.se) %>% 
  data.frame()

## Create strip character
dat2plot$itype <- factor(dat2plot$itype, labels = c("GBI", "TBI"))
dat2plot_wo0glances$itype <- factor(dat2plot_wo0glances$itype, labels = c("GBI", "TBI"))

## Create variable used for labelling
dat2plot <- 
  dat2plot %>% 
  mutate(scenario_ilevel = 
           ifelse(grepl("ae", condition), "motorway_simple", "")) %>% 
  mutate(scenario_ilevel = 
           ifelse(grepl("ak", condition), "motorway_complex", scenario_ilevel)) %>% 
  mutate(scenario_ilevel = 
           ifelse(grepl("se", condition), "city_simple", scenario_ilevel)) %>% 
  mutate(scenario_ilevel = 
           ifelse(grepl("sk", condition), "city_complex", scenario_ilevel))

dat2plot_wo0glances <- 
  dat2plot_wo0glances %>% 
  mutate(scenario_ilevel = 
           ifelse(grepl("ae", condition), "motorway_simple", "")) %>% 
  mutate(scenario_ilevel = 
           ifelse(grepl("ak", condition), "motorway_complex", scenario_ilevel)) %>% 
  mutate(scenario_ilevel = 
           ifelse(grepl("se", condition), "city_simple", scenario_ilevel)) %>% 
  mutate(scenario_ilevel = 
           ifelse(grepl("sk", condition), "city_complex", scenario_ilevel)) %>% 
  arrange()

# Set right colours
colset_order <- c("city_simple", "city_complex", "motorway_simple", "motorway_complex")
colset4groupvar <- unlist(set4plot$colour[["scenario_ilevel"]])
colset4groupvar <- as.character(colset4groupvar[colset_order])

## Create plot
plotdat <- 
  ggplot() + 
  geom_bar(data = dat2plot,
           aes(x = scenario_ilevel,
               y = x.avg,
               fill = scenario_ilevel),
           stat = "identity") + 
  geom_errorbar(data = dat2plot,
                aes(x = scenario_ilevel, ymin = upper, ymax = lower), 
                width = 0.2) + 
  facet_grid(.~itype, scales = "free") + 
  coord_cartesian(ylim = c(0, 1.75)) +
  scale_x_discrete(limits = colset_order,
                   labels = createLabels4PlotLegend("dat2plot_wo0glances", "scenario_ilevel", c(3,4,1,2))) + 
  scale_y_continuous(expand = c(0, 0)) + 
  scale_fill_manual(values = rep(colset4groupvar, 2),
                    labels = createLabels4PlotLegend("dat2plot_wo0glances", "scenario_ilevel", c(3,4,1,2))) + 
  scale_colour_manual(values = rep(colset4groupvar, 2),
                      labels = createLabels4PlotLegend("dat2plot_wo0glances", "scenario_ilevel", c(3,4,1,2))) + 
  theme_bw() + 
  guides(fill = F, colour = F, shape = F) + 
  ggtitle(label = "Average gaze duration towards display") + 
  #subtitle = "(Graduation without zero-number gazes)") + 
  labs(y = "Gaze duraction (s)") + 
  theme(title = element_text(size = 7, face = "bold")) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 5, color = "black", angle = 45, vjust = 0.9, hjust = 0.7),
        axis.text.y = element_text(size = 5, color = "black")) + 
  theme(strip.text.x = element_text(size = 7, face = "bold")) 


plot(plotdat)

ggsave(filename = "glances_dur.png", 
       plot = plotdat,
       path = "plots/PaperTBD_2017",
       width = 15 / 2,
       height = 5,
       units = "cm",
       dpi = 600)




# BACKUP ------------------------------------------------------------------

# dat2plot2 <- dat2plot
# dat2plot2$itype <- factor(dat2plot2$itype)
# plotdat_temp <- glanceExplorer(dat2plot2, c("condition", "itype"), "avg", "glances_n", "task_nr.glance")
# plotdat <- plotdat_temp[[2]]
# dat <- plotdat_temp[[1]]
# 
# plotdat <- 
#   plotdat + 
#   geom_errorbar(data = dat,
#                 aes(x = condition, ymin = upper, ymax = lower), width = 0.2) + 
#   theme_bw()+ 
#   labs(y = "Average number of glances") +
#   theme(title = element_blank()) + 
#   guides(fill = F)
# 
# plot(plotdat)
# 
# ggsave(filename = "glances_n.png", 
#        plot = plotdat,
#        path = "plots/PaperTBD_2017",
#        width = 15 / 2,
#        height = 5,
#        units = "cm",
#        dpi = 600)


# dat2plot2 <- dat2plot
# dat2plot2$itype <- factor(dat2plot2$itype)
# plotdat_temp <- glanceExplorer(dat2plot2, c("condition", "itype"), "avg", "dur_s.glance", "task_nr.glance")
# plotdat <- plotdat_temp[[2]]
# dat <- plotdat_temp[[1]]
# 
# plotdat <- 
#   plotdat + 
#   geom_errorbar(data = dat,
#                 aes(x = condition, ymin = upper, ymax = lower), width = 0.2) + 
#   theme_bw()+ 
#   labs(y = "Average number of glances") +
#   theme(title = element_blank()) + 
#   guides(fill = F)
# 
# plot(plotdat)
# 
# ggsave(filename = "glances_dur.png", 
#        plot = plotdat,
#        path = "plots/PaperTBD_2017",
#        width = 15 / 2,
#        height = 5,
#        units = "cm",
#        dpi = 600)
