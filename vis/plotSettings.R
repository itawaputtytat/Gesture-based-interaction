set4plot <- c()

## Width to left side
set4plot$gtable_width <- readRDS("postprocessing/gtable_width.RDS")



# X-axis-title ------------------------------------------------------------

set4plot$axis.title.x.score$acc <- "Acceptance Score"
set4plot$axis.title.x.score$attrakdiff <- "AttrakDiff Score"
set4plot$axis.title.x.score$nat <- "Naturalness Score"
set4plot$axis.title.x.score$nasatlx <- "NASA-TLX Score"



# Scales ------------------------------------------------------------------

# Breaks ----
set4plot$ybreaks4score$acc <- seq(-20, 20, 10)
set4plot$ybreaks4score$attrakdiff <- seq(-100, 100, 20)
set4plot$ybreaks4score$nasatlx <- seq(0, 120, 20)
set4plot$ybreaks4score$nat <- seq(0, 180, 20)

set4plot$ybreaks4profile$acc <- seq(-2, 2, 1)
set4plot$ybreaks4profile$attrakdiff <- seq(-100, 100, 20)
set4plot$ybreaks4profile$nasatlx <- seq(0, 20, 5)
set4plot$ybreaks4profile$nat <- seq(0, 5, 1)

# Limits for scores ----
set4plot$ylim4score$acc <- c(-25, 25)
set4plot$ylim4score$attrakdiff <- c(-90, 90)
set4plot$ylim4score$nasatlx <- c(-5, 125)
set4plot$ylim4score$nat <- c(-5, 185)

# Limits for items ----
set4plot$ylim4items$acc <- c(-2.5, 2.5)
set4plot$ylim4items$attrakdiff <- c(-3.5, 3.5)
set4plot$ylim4items$nasatlx <- c(-2.5, 22.5)
set4plot$ylim4items$nat <- c(-0.5, 5.5)



# Export ------------------------------------------------------------------

set4plot$dir4plot <- file.path("plots", dbset$name)



# Title -------------------------------------------------------------------

set4plot$legend.title$acc        <- "Acceptance"
set4plot$legend.title$nasatlx    <- "NASA-TLX"
set4plot$legend.title$attrakdiff <- "AttrakDiff"
set4plot$legend.title$nat        <- "Naturalness"
set4plot$legend.title$fb         <- "Feedback"


# Labels ------------------------------------------------------------------

set4plot$labels4legend <- dbGetSrc("t_q_condlabels")
set4plot$labels4legend$condlabel <- gsub(" ", "\n", set4plot$labels4legend$condlabel)




colset <- list(
  expfocus = 
    list(exp1_mouse = "darkorange2",
         exp2_mouse = "darkorange2",
         exp3_mouse = "darkorange2",
         exp1_nui_n6 = "red2",
         exp2_nui_n4 = "blue2",
         exp3_nui_n2 = "green4",
         exp1_bi_n6 = "magenta",
         exp3group_mouse = "darkorange4",
         exp3group_nui_n6 = "deepskyblue4",
         exp3_fb1_nui_n6 = "turquoise2",
         exp3_fb2_nui_n6 = "slateblue2",
         exp3_fb3_nui_n6 = "royalblue4",
         motorway_gestures_simple = "red2",
         motorway_gestures_complex = "red3",
         city_gestures_simple = "red2",
         city_gestures_complex = "red3",
         motorway_touch_simple = "magenta3",
         motorway_touch_complex = "magenta4",
         city_touch_simple = "magenta3",
         city_touch_complex = "magenta4",
         gestures = "red2",
         touch = "magenta3"),
  typedetail = 
    list(mouse = "darkorange2",
         nui = "red2",
         bi = "magenta",
         gestures_simple = "red2",
         gestures_complex = "red3",
         touch_simple = "magenta3",
         touch_complex = "magenta4"),
  typegeneric = 
    list(mouse = "darkorange2",
         gesture = "red2",
         gestures = "red2",
         touch = "magenta3"))

shapeset <- list(
  expfocus = 
    list(exp1_mouse = 15,
         exp2_mouse = 16,
         exp3_mouse = 17,
         exp1_nui_n6 = 15,
         exp2_nui_n4 = 15,
         exp3_nui_n2 = 15,
         exp1_bi_n6 = 15,
         exp3group_mouse = 15,
         exp3group_nui_n6 = 15,
         exp3_fb1_nui_n6 = 15,
         exp3_fb2_nui_n6 = 16,
         exp3_fb3_nui_n6 = 17,
         motorway_gestures_simple = 16,
         motorway_gestures_complex = 16,
         city_gestures_simple = 16,
         city_gestures_complex = 16,
         motorway_touch_simple = 17,
         motorway_touch_complex = 17,
         city_touch_simple = 17,
         city_touch_complex = 17,
         gestures = 16,
         touch = 17),
  typedetail = 
    list(mouse = 15,
         nui = 15,
         bi = 15,
         gestures_simple = 16,
         gestures_complex = 16,
         touch_simple = 17,
         touch_complex = 17
         ),
  typegeneric = 
    list(mouse = 15,
         gesture = 15,
         gestures = 16,
         touch = 17))

