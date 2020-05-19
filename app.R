#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


### library imports
library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)
library(shinyWidgets)


## read in data 
data <- read.csv("CIP_Type1_2020-04-07.csv", header=T, na.strings=c("-"))
head(data)
str(data)


##UI Set Up
shinyApp(
    ui = fluidPage(
        titlePanel("Forest Carbon Index Project"),
        sidebarLayout(
            sidebarPanel(
                h3("Let's Get Started!"),
                br(),
                selectInput("user_role", "I am a(n):",
                            c("Concerned Citizen", ## include advocacy materials
                              "Elected Official", ## include sample policies
                              "Scientist")), ## include data and model outputs
                pickerInput("state", "I live in:",
                            levels(data$State), options=list(`actions-box` = TRUE), multiple = FALSE, selected="Alabama"),
                
                sliderInput("slider", "Slider", 1, 100, 50), 
            ), #end sidebar panel
            
            
            
            mainPanel(
                tabsetPanel(type="tabs", 
                            
                    tabPanel("Introduction", 
                             p(),
                             p(strong("Dealing with climate change can be a challenging prospect for our nation’s local leaders."), " There is ", em("a lot"), " of information out there, but not a lot of easy ways to apply it. This project is meant to bridge the gap between data availability and data usability."), 
                             p("Climate change is happening because there is simply too much carbon dioxide, and other greenhouse gases, entering the atmosphere. This is creating a warming effect that is changing our weather patterns and possibilities of extreme events. The United States is one of the worst carbon emitters, and so, we must take steps at the national, state, and local level to combat our carbon emissions.", strong("Emissions are cut in one of a few ways: increasing efficiency, reducing use, and increasing natural carbon sequestration (carbon being absorbed OUT of the atmosphere).")), 
                             
                             p(strong("Natural climate solutions are activities that increase natural carbon sequestration or prevent carbon emissions from natural areas. "), "Experts believe that up to a third of our carbon emissions can be mitigated through natural climate solutions. In order to implement natural climate solutions, we need to know how much carbon our natural areas are currently storing, how much they could potentially store, and ways in which they might lose that carbon over time."),
                             
                             p(strong("Select your info (left) and then click through the tabs (top) to explore your area’s Forest Carbon Index (FCI), identify areas for improvement, and print out a report to help you take action."), style="color:blue;")),
                    
                    tabPanel("State Report",
                             h3(textOutput("stateChosenName")),
                             p("The great state of ", textOutput("justStateName", inline=T), " has ", textOutput("stTotalAc", inline=T), " acres of forestland."),
                             p(textOutput("userRole")),
                             
                             dataTableOutput("stateTable")
                             
                             
                             
                             ),#end tabPanel
                    tabPanel("Original", 
                             
                             downloadButton("report", "Generate report")
                             )        
                            
                            )
            )
        ),

    ),
    
    
    
    
    
    
    ### back end and data stuff
    
    
    server = function(input, output) {
        
        
        output$userRole <- renderText({
            paste("I am a(n) ", input$user_role)
        })
        
        
        output$stateChosenName <- renderText({ 
            paste("State Chosen: ", input$state)
        })
        
        output$justStateName <- renderText({input$state})
        
        stateData <- reactive({
            ##Filter By state
            
            data <- subset(
                data,
                State %in% input$state
            )
            
            data
        })
        
        output$stateTable <- renderDataTable({
            datatable(stateData())
        })
        
        
        
        output$stTotalAc <- reactive({
            format(
                round(
                    subset(data, 
                      State %in% input$state &
                      Stand_Origin %in% "Total" &
                      Stand_Age %in% "Total" &
                      Forest.type.group %in% "Total")[,"ac"], #endsubset
                    0), #endround 
                big.mark=",")#endformat
           
        })
        
        output$stTotalC <- reactive({
          format(
            round(
              subset(data, 
                    State %in% input$state &
                    Stand_Origin %in% "Total" &
                    Stand_Age %in% "Total" &
                    Forest.type.group %in% "Total")[,"tC"], #endsubset
                  0), #endround 
              big.mark=",")#endformat
        })
        
        
        output$report <- downloadHandler(
            # For PDF output, change this to "report.pdf"
            filename = "report.pdf",
            content = function(file) {
                # Copy the report file to a temporary directory before processing it, in
                # case we don't have write permissions to the current working dir (which
                # can happen when deployed).
                tempReport <- file.path(tempdir(), "report.Rmd")
                file.copy("report.Rmd", tempReport, overwrite = TRUE)
                
                # Set up parameters to pass to Rmd document
<<<<<<< HEAD
                params <- list(data= data, n = input$slider, stateText = input$state, user= input$user_role)
=======
                params <- list(n = input$slider, stateText = input$state)
>>>>>>> 4acb8685a6dc797c0ec619b2d4998780af1b8a63
                
                # Knit the document, passing in the `params` list, and eval it in a
                # child of the global environment (this isolates the code in the document
                # from the code in this app).
                rmarkdown::render(tempReport, output_file = file,
                                  params = params,
                                  envir = new.env(parent = globalenv())
                )
            }
        )
    }
)