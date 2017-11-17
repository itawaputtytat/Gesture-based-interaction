
# Objective ---------------------------------------------------------------

## Analysis of annotated glances



# Data preparation --------------------------------------------------------

## Load data
glances <- dbGetSrc("t_glances_lisa")
procedure <- dbGetSrc("t_procedure")

## Join data
glances_ext <- 
  left_join(glances,
            procedure %>% select(id, block, block_task, cond = condition),
            by = c("id", "cond"))
  