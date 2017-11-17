
# Objective ---------------------------------------------------------------

## Extract exact conditions from pdf-protocol


# Packages ----------------------------------------------------------------

library(pdftools)
library(stringr)
library(dplyr)
library(zoo)



# Extract pdf text --------------------------------------------------------

path <- "analysis-study-4/scrape-and-export-exp-procedure-to-db/"
file <- "Instruktionen und Protokoll Experiment 4_V2.pdf"
protocol <- pdf_text(pdf = file.path(path, file))



# String operations -------------------------------------------------------

## Find positions of "Vp"
posfinder <- str_locate(protocol, "Vp")[, "start"]

## Create subject ids from these positions
ids <- str_sub(protocol, posfinder + 2, posfinder + 4)

## Remove date, time and line for "Vp"
protocol_clean <- str_sub(protocol, posfinder + 5, nchar(protocol))

## Split rows
protocol_rows <- str_split(protocol_clean, "\r\n")

## Remove unneeded rows 1, 2, and 40
protocol_rows2 <- lapply(protocol_rows, "[", c(3:39))

## Find multiple spaces in each list element and vector element
spacefinder <- lapply(protocol_rows2, function(x) str_locate_all(x, "  ") )

## Remove connected spaces
protocol_clean2 <- lapply(protocol_rows2,
       function(x)
         lapply(x, 
                function(x) {
                  temp <- str_split(x, "  ")
                  temp <- temp[[1]][which(temp[[1]] != "")] 
                } ) )

## Four elements in each row
protocol_elements <- 
  lapply(protocol_clean2,
         function(x)
           lapply(x, 
                  function(x) {
                    if (length(x) == 4) x <- x else
                      if(length(x) == 3) x <- append(x, "") else
                        if(length(x) == 2) x <- c("", "", x)
                  } ) )

## Convert to data.frame and name columns
header_new <- c("nr", "scenario", "task", "scenario_pos_m")

protocol_list <- 
  lapply(protocol_elements, function(x) {
           
           ## Create data frame
           x <- data.frame(matrix(unlist(x), ncol = 4, byrow = T)) 
           
           ## Rename columns
           colnames(x) <- header_new
           
           ## Data adjustments
           x <-
             x %>%
             ## De-factorise values
             mutate(nr = as.numeric(as.character(nr))) %>%
             mutate(scenario = as.character(scenario)) %>%
             mutate(task = as.character(task)) %>%
             mutate(scenario_pos_m = 
                      as.numeric(as.character(scenario_pos_m))) %>%
             ## Fill missing values
             mutate(nr = ifelse(is.na(nr), na.locf(nr), nr)) %>%
             mutate(scenario = ifelse(scenario == "", 
                                      NA, scenario)) %>%
             mutate(scenario = ifelse(is.na(scenario), 
                                      na.locf(scenario), scenario)) %>% 
             mutate(scenario_pos_m = ifelse(is.na(scenario_pos_m), 
                                            0, scenario_pos_m)) %>%
             ## Create task numbers
             mutate(block = ifelse(nr %in% c(1, 2, 3), 0, nr - 3),
                    block = ifelse(nr %in% c(12, 13), 9, block)) %>%
             group_by(block) %>%
             mutate(block_task = row_number()) %>%
             ## Create dataframe
             data.frame() 
           x
         } )

## Name list elements
names(protocol_list) <- paste("Vp", ids, sep = "")



# Final adjustments -------------------------------------------------------

## Convert list into data frame
protocol_df <- do.call(rbind.data.frame, protocol_list)

## Remove first spaces
protocol_df$scenario <- 
  ifelse(str_sub(protocol_df$scenario, 1, 1) == " ", 
         str_replace(protocol_df$scenario, " ", ""),
         protocol_df$scenario)

protocol_df$task <- 
  ifelse(str_sub(protocol_df$task, 1, 1) == " ", 
         str_replace(protocol_df$task, " ", ""),
         protocol_df$task)

## Create ids
protocol_df$id <- substr(rownames(protocol_df), 3, 5)
protocol_df$id <- as.numeric(protocol_df$id)
rownames(protocol_df) <- NULL

## Create conditions abbrebiations (corresponding to glance data from LG)
conditions <-
  protocol_df %>%
  group_by(id) %>%
  filter(block_task == 1) %>%
  mutate(cond = NA) %>%
  mutate(cond = ifelse(scenario == "Autobahn" & 
                         grepl("Geste", task) & 
                         grepl("einfach", task), "gae", cond),
         cond = ifelse(scenario == "Autobahn" & 
                         grepl("Geste", task) & 
                         grepl("komplex", task), "gak", cond),
         cond = ifelse(scenario == "Autobahn" & 
                         grepl("Touch", task) & 
                         grepl("einfach", task), "tae", cond),
         cond = ifelse(scenario == "Autobahn" & 
                         grepl("Touch", task) & 
                         grepl("komplex", task), "tak", cond),
         cond = ifelse(grepl("Stadt", scenario) & 
                         grepl("Geste", task) & 
                         grepl("einfach", task), "gse", cond),
         cond = ifelse(grepl("Stadt", scenario) & 
                         grepl("Geste", task) & 
                         grepl("komplex", task), "gsk", cond),
         cond = ifelse(grepl("Stadt", scenario) & 
                         grepl("Touch", task) & 
                         grepl("einfach", task), "tse", cond),
         cond = ifelse(grepl("Stadt", scenario) & 
                         grepl("Touch", task) & 
                         grepl("komplex", task), "tsk", cond)) %>%
  data.frame()
conditions

## Join protocol data and conditions
protocol_df <-
  left_join(protocol_df,
            conditions %>% select(id, block, condition = cond),
            by = c("id", "block"))

## Re-order columns
protocol_df <- 
  protocol_df %>%
  select(id, block, block_task, scenario, scenario_pos_m, task, condition)



# Upload to database ------------------------------------------------------

dbWriteTable(dbconn, "t_procedure", protocol_df, row.names = F)
