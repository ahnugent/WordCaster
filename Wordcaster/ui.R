## Script:      ui.R
## Folder:      Wordcaster
## Author:      Allen H. Nugent
## Created:     2016-04-10
## Last edit:   2016-04-17
## Last test:   2016-04-17
## Purpose:     Shiny UI file for DS10 Course project
##
## 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  

# Set up environment ...

#  Disable these before publishing:
#         setwd("E:/R_data/Course10/Wordcaster")
#         library(shiny)   
        
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  


# load global data & functions ...

source("global.R")


# design the web page ...

shinyUI(
    fluidPage(
        
        titlePanel("Wordcaster Text Prediction"),
        
        br(),

        sidebarPanel(
            
            br(),

            numericInput("numChoices", label = "Number of word choices to show:", 
                         value = 6, min = 1, max = 8, step = 1),
            
            br(),
            
            helpText(a("Click for more information", href="http://rpubs.com/AltShift/Wordcaster"))

        ),
        
        mainPanel(
            
            br(),
            
            textInput("textin", "Enter text phrase here:"),
            
            submitButton("Predict next word"),
            
            br(),

            h4("Predicted choices:"),

            tableOutput("wordcasts"),
            
            br()

        )
    )
)
