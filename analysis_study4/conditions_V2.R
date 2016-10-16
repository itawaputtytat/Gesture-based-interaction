
# Objective ---------------------------------------------------------------
## Extract exact conditions from pdf-protocol


# Packages ----------------------------------------------------------------
library(pdftools)
library(stringr)
library(dplyr)
library(zoo)


# Extract pdf text --------------------------------------------------------
protocol <- pdf_text(pdf = "Instruktionen und Protokoll Experiment 4_V2.pdf")


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

protocol_df <- 
  lapply(protocol_elements, function(x) {
           
           ## Create data frame
           x <- data.frame(matrix(unlist(x), ncol = 4, byrow = T)) 
           
           ## Rename columns
           colnames(x) <- header_new
           
           ## Final adjustments to data frame
           x <-
             x %>%
             ## De-factorise values
             mutate(nr = as.numeric(as.character(nr))) %>%
             mutate(scenario = as.character(scenario)) %>%
             mutate(task = as.character(task)) %>%
             mutate(scenario_pos_m = as.numeric(as.character(scenario_pos_m))) %>%
             # ## Remove first spaces
             # mutate(scenario =
             #          ifelse(str_sub(scenario, 1, 1) == " "),
             #                 str_sub(scenario, 2, nchar(scenario)),
             #                 scenario) %>%
             # mutate(task =
             #          ifelse(str_sub(task, 1, 1) == " "),
             #                 str_sub(task, 2, nchar(task)),
             #                 task) 
             ## Fill missing values
             mutate(nr = ifelse(is.na(nr), na.locf(nr), nr)) %>% #
             mutate(scenario = ifelse(scenario == "", NA, scenario)) %>%
             mutate(scenario = ifelse(is.na(scenario), na.locf(scenario), scenario)) %>% #
             mutate(scenario_pos_m = ifelse(is.na(scenario_pos_m), 0, scenario_pos_m)) %>%
             ## Create task numbers
             mutate(block = ifelse(nr %in% c(1, 2, 3), 0, nr - 3),
                    block = ifelse(nr %in% c(12, 13), 9, block)) %>%
             group_by(block) %>%
             mutate(block_tasknr = row_number()) %>%
             ## Create dataframe
             data.frame() 
           x
         } )

## Name list elements
names(protocol_df) <- paste("Vp", ids, sep = "")
