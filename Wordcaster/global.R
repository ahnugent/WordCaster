## Script:      global.R
## Folder:      Wordcaster
## Author:      Allen H. Nugent
## Created:     2016-04-10
## Last edit:   2016-04-13
## Last test:   2016-04-13
## Purpose:     Shared data initialisation file for Shiny app in DS10 Course project
##



library(shiny)
library(stringr)

# infolder <- "data/modeldir"
# files <- dir(infolder)

maxnum.choices <- 8  #: must = 'max' arg in numericInput("numChoices",...) !



# # custom functions used ...
# 
# # round and format a number with a non-ridiculous number of decimals:
# cround <- function(x, ndec)   
# {
#     if (ndec == 0)
#     {
#         fmt1 <- "%d"
#     } else 
#     {
#         fmt1 <- paste0("%.", ndec, "f")
#     }
#     result <- sprintf(fmt = fmt1, round(x, ndec))
#     return(result)
# }
# 
# 
# trim <- function (x) {
#     # returns string w/o leading or trailing whitespace:
#     return (gsub("^\\s+|\\s+$", "", x))
# }
# 
# 
# get.predicts <- function(textin, num.choices = maxnum.choices) {
#     
#     predicts.init <- matrix(rep("____", maxnum.choices), nrow = 1, ncol = maxnum.choices)
#     predicts.init <- as.data.frame(predicts.init, stringsAsFactors = FALSE)
#     names(predicts.init) <- as.character(unlist(seq(from = 1, to = maxnum.choices)))
#     rownames(predicts.init) <- "?"
#     
#     predicts <- predicts.init
#     
#     if (trim(textin) != "")
#     {    
#         for (j in 1:num.choices)
#         {
#     #        predicts[1, j] <- paste0("a", as.character(j))  #: dummy assignment for testing
#             predicts[1, j] <- paste0(textin, as.character(j))  #: dummy assignment for testing
#         }
#     }
#     predicts <- predicts[, 1:num.choices]
#     
#     return(predicts)
# }
# 
# 

