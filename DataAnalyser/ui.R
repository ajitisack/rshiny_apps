#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


loadData <- function(){
    fluidPage(
        titlePanel(h4("Load Data")),
        tags$br(),
        fluidRow(
            column( 4, fileInput("in_file", "Choose Data File"))
        )
    )
}


viewData <- function(){
    fluidPage(
        fluidRow(
            dataTableOutput("table_content")
        )
    )
}


navbarPage("Data Player",
      tabPanel("Load Data", loadData())
    , tabPanel("View Data", viewData())
    , tabPanel("Basic Statistics")
    , tabPanel("Charts & Graphs")
    , tabPanel("Advanced Charts")
)


