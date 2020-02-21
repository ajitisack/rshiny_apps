

library(shiny)
library(shinydashboard)

library(data.table)
library(DT)

library(bpa)

library(plotly)
library(ggplot2)

source("D:/Machine Learning/R/DataVisualization.R")
source("D:/Machine Learning/R/DataProcessing.R")

rm(list=ls(all.names = T))
d <<- data.table()

# sidebar
#--------
sidebar <- dashboardSidebar(
    sidebarMenu(
          menuItem("Load & View Data", tabName = "loaddata", icon = icon("cog"))
        , menuItem("Basic Statistics", tabName = "basicstats", icon = icon("th"))
        , menuItem("Pattern Analysis", tabName = "patanal", icon = icon("th"))
        , menuItem("Frequency Plots", tabName = "freqplots", icon = icon("bar-chart"))
    )
)


#-------------------------------------------------------------------------------


# Load and View Data TabItem
# ----------------------------
loaddata_item1 <- box(
      title = "Select Input file"
    , solidHeader = T
    , status = 'primary'
    , width = 3
    , fileInput("in_file", "")
)

loaddata_item2 <- infoBoxOutput("nrowbox")
loaddata_item3 <- infoBoxOutput("ncolbox")

# loaddata_item4 <- box(
#       title = "Data Table"
#     , solidHeader = T
#     , width = 12
#     , status = 'primary'
#     , DT::dataTableOutput("table_content")
# )

loaddata_item4 <- DT::dataTableOutput("table_content")



# Basic Statistics result TabItem
# -------------------------------------

bstat_item1 <- box(
    title = "Basic Statistics Table"
    , solidHeader = T
    , width = 12
    , status = 'primary'
    , DT::dataTableOutput("bstatresult")
)


# Basic Pattern Analysis result TabItem
# -------------------------------------

bpa_item1 <- box(
      title = "Basic Pattern Analysis"
    , solidHeader = T
    , width = 8
    , status = 'primary'
    , DT::dataTableOutput("bparesult")
)

bpa_item2 <- box(
    title = "Basic Pattern Analysis - Summary"
    , solidHeader = T
    , width = 4
    , status = 'primary'
    , DT::dataTableOutput("bpasummary")
)

bpa_item3 <- box(
    title = "Basic Pattern Analysis - Plot"
    , solidHeader = T
    , width = 8
    , status = 'primary'
    , plotlyOutput("bpaplot", width = "96%", height="100%")
)

# dashboard body
#---------------

body <- dashboardBody(
    tabItems(
        tabItem("loaddata"
                , fluidRow(loaddata_item1, loaddata_item2, loaddata_item3)
                , fluidRow(loaddata_item4)
        ),
        tabItem("basicstats"
                , fluidRow(bstat_item1)
        ),
        tabItem("patanal"
                , fluidRow(bpa_item3, bpa_item2)
                , fluidRow(bpa_item1)
        ),
        tabItem("freqplots"
                , uiOutput("freqplots")
                , style = "overflow-y:scroll;"
        )
    )
)


# ui part
#---------

ui <- dashboardPage(
        dashboardHeader(title = "Machine Learning")
      , sidebar
      , body
)


#x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x=x


# server part
#---------
server <- function(input, output){
    
    options(shiny.maxRequestSize=500*1024^2)

    output$table_content <- renderDataTable({
        if (is.null(input$in_file))
            return(NULL)
        else
            fdata <- fread(input$in_file$datapath)
        assign("d", fdata, envir = .GlobalEnv)
        DT::datatable(fdata, options = list(pageLength = 15))
    })

    output$nrowbox <- renderInfoBox({
        infoBox(
              title = tags$h4(tags$b("Number of rows"))
            , ifelse(is.null(input$in_file), 0, nrow(d))
            , color = "blue"
            , fill = TRUE
        )
    })

    output$ncolbox <- renderInfoBox({
        infoBox(
              title = tags$h4(tags$b("Number of columns"))
            , ifelse(is.null(input$in_file), 0, ncol(d))
            , color = "blue"
            , fill = TRUE
        )
    })

    output$bparesult <- renderDataTable({
        if (!is.null(input$in_file)){
            x <- bpa(d, unique_only = TRUE) %>% lapply(data.frame)
            x <- do.call("rbind", x)
            y <- data.table(
                 "Column_Name" = gsub("(^.[^\\.]*)\\.(.*$)", "\\1", rownames(x))
                , "Pattern" = x$Var1
                , "Count"   = x$Freq
            ) %>% arrange(Column_Name, Pattern)
            y <- data.table(y)
            assign("dt_bpa", y, envir = .GlobalEnv)
            DT::datatable(y, options = list(paging = FALSE))
        }
    })

    output$bpasummary <- renderDataTable({
        if (!is.null(input$in_file)){
            z <- dt_bpa[,.("Count" = .N), by = Column_Name]
            DT::datatable(z, options = list(paging = FALSE))
        }
    })
    
    output$bpaplot <- renderPlotly({
        if (!is.null(input$in_file)){
            barPlot(dt_bpa, "Column_Name")
        }
        else 
            return(NULL)
    })
    
    output$bstatresult <- renderDataTable({
        if (!is.null(input$in_file)){
            x <- mysum(d)
            names <- rownames(x)# %>% data.frame
            rownames(x) <- NULL
            x <- cbind("Column_Name" = names, x)
            DT::datatable(data.table(x), options = list(paging = FALSE))
        }
    })
    
    output$freqplots <- renderUI({
        plot_output_list <- lapply(1:ncol(d), function(i) {
            plotname <- names(d)[i]
            box(
                title = plotname
                , solidHeader = T
                , width = 8
                , status = 'primary'
                , plotlyOutput(plotname, width = "100%", height="100%")
            )
        })
        if (!is.null(input$in_file))
            do.call(tagList, plot_output_list)
    })
    
    n <- ncol(d)
    if ( !is.null(n) ) n <- 1
    
    for (i in n) {
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
            my_i <- i
            if (ncol(d) != 0) {
                plotname <- names(d)[i]
                output[[plotname]] <- renderPlotly({ freqPlot(d, plotname) })
            }
        })
    }
}


# Run the application
#--------------------
shinyApp(ui, server)

