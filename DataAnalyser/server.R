#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)

readFile <- function(file){
    if (is.null(file))
        return(NULL)
    else 
        data <- read.csv(file$datapath)
    return(data)
}


function(input, output) {
    output$table_content <- renderDataTable(readFile(input$in_file))
}


