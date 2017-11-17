
# Objective ---------------------------------------------------------------

## Extract exact conditions from pdf-protocol


# Packages ----------------------------------------------------------------

library(pdftools)
library(stringr)



# Extract pdf text --------------------------------------------------------

protocol <- pdf_text(pdf = "Instruktionen und Protokoll Experiment 4_V2.pdf")



# String operations -------------------------------------------------------

## Find positions of "Vp"
posfinder <- str_locate(protocol, "Vp")[, "start"]

## Create subject ids
ids <- str_sub(protocol, posfinder + 2, posfinder + 4)

## Remove date, time and vp-line
protocol_clean <- str_sub(protocol, posfinder + 5, nchar(protocol))

## Split rows
protocol_rows <- str_split(protocol_clean, "\r\n")

## Remove unneeded rows
protocol_rows <- lapply(protocol_rows, "[", c(3:39))

# # ## Remove first spaces in each line
# protocol_rows <- lapply(protocol_rows, function(x) str_sub(x, 2, nchar(x)) )

## Find multiple spaces in each list element and vector element
spacefinder <- lapply(protocol_rows, function(x) str_locate_all(x, "  ") )
# 
# ## Identify connected spaces as sequences
# spacefinder_seq <- 
#   lapply(spacefinder, 
#          function(x) { 
#            x <- 
#              data.frame(x) %>%  
#              ## If new start position directly follows previous end position
#              ## ... code as 0, otherwise 1
#              mutate(newseq = 
#                       ifelse(start == lag(end, default = 1) + 1, 0, 1)) %>% 
#              ## Create sequence numbers for actual sequence (coded as 1)
#              ## ... otherwise sequence number will be 0
#              group_by(newseq) %>% 
#              mutate(seqnr = row_number() * newseq) %>% 
#              ## Extract information of actual sequences
#              filter(seqnr != 0)
#          } )


#test <- protocol_rows[[1]]

## Remove connected spaces
# testi <- 
#   lapply(test, 
#          function(x) {
#            temp <- str_split(x, "  ")
#            temp <- temp[[1]][which(temp[[1]] != "")] 
#          } )

test4 <- 
  lapply(protocol_rows,
         function(x)
           lapply(x, 
                  function(x) {
                    temp <- str_split(x, "  ")
                    temp <- temp[[1]][which(temp[[1]] != "")] 
                  } )
  )


## Four elements in each row
# testi2 <- 
#   lapply(testi, 
#          function(x) {
#            if (length(x) == 4) x <- x else
#              if(length(x) == 3) x <- append(x, "") else
#                if(length(x) == 2) x <- c("", "", x)
#          } )

test5 <- 
  lapply(test4,
         function(x)
           lapply(x, 
                  function(x) {
                    if (length(x) == 4) x <- x else
                      if(length(x) == 3) x <- append(x, "") else
                        if(length(x) == 2) x <- c("", "", x)
                  } )
  )



## Convert to data.frame
# testi3
# for(i in 1:length(testi2))
#   testi3 <- rbind(testi3, testi2[[i]])

testi <- 
  lapply(test5,
         function(x) {
           test6 <- c()
           for (i in 1:length(x))
             test6 <- rbind(test6, x[[i]])
           x <- test6
         } )

names(testi) <- paste("Vp", ids, sep = "")

## Rename dataframes in list
header <- c("nr", "scenario", "scenario_pos_m", "task")

testi <- 
  lapply(seq_along(testi),
         function(x) {
           ## Rename dataframe
           colnames(testi[[x]]) <- header
           ## Return object
           testi[[x]]
         } )

# lapply(testi,
#        function(x)
#          apply(x, 2,
#                 function(x)
#                   if (str_sub(x, 1, 1) == " ")
#                     x <- str_sub(x, 2, nchar(x))
#                     ) )

# # ## Remove first spaces in each line
testi2 <- 
  lapply(testi,
         function(x)
           apply(x, 2, 
                 function(x) 
                   sapply(x,
                          function(x) {
                            if (str_sub(x, 1, 1) == " ")
                              x <- str_sub(x, 2, nchar(x))
                            x
                          }
                   ) ) 
  )

## Convert to dataframes

testi2[[1]] %>% 
  data.frame() %>% 
  mutate(nr = ifelse(nr == "", lag(nr), nr))






# 
# 
# 
# 
# ## Create header
# 
# 
# ## Find position of "Streckenposition"
# posfinder <- str_locate(protocol, "Streckenposition")[, "start"]
# 
# ## Remove header
# protocol_clean <- 
#   lapply(protocol, 
#          function(x) 
#            x <- str_sub(x, 1, posfinder + nchar("Streckenposition") )
# 
# gsub("Datum, Uhrzeit:\r\n", "")
