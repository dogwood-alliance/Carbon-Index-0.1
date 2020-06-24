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
library(dplyr)
library(ggplot2)
library(shinyWidgets)


## read in data 
data <- read.csv("CIP_Type1_2020-04-07.csv", header=T, na.strings=c("-"))
data$ac<- as.numeric(as.character(data$ac))
data$tC<- as.numeric(as.character(data$tC))
#data$tC_ac<- as.numeric(as.character(data$tC_ac))
head(data)
str(data)

##UI Set Up
shinyApp(
    ui = fluidPage(
        titlePanel("Forest Carbon Index"),
        sidebarLayout(
            sidebarPanel(
                style = "position:fixed;width:30%;",
                h3("Let's Get Started!"),
                br(),
                selectInput("user_role", "I am a(n):",
                            c("Concerned Citizen", ## include advocacy materials
                              "Elected Official", ## include sample policies
                              "Scientist")), ## include data and model outputs
                pickerInput("state", "I live in:",
                            levels(data$State), options=list(`actions-box` = TRUE), multiple = FALSE, selected="Alabama"),
                
                sliderInput("slider", "Percentage Slider", 0, 100, 10, 5), 
            ), #end sidebar panel
            
            
            
            mainPanel(
                tabsetPanel(type="tabs", 
                            
                    tabPanel("Introduction", 
                             p(),
                             p(strong("Dealing with climate change can be a challenging prospect for our nation’s local leaders."), " There is ", em("a lot"), " of information out there, but not a lot of easy ways to apply it. This project is meant to bridge the gap between data availability and data usability."), 
                             p("Climate change is happening because there is simply too much carbon dioxide, and other greenhouse gases, entering the atmosphere. This is creating a warming effect that is changing our weather patterns and possibilities of extreme events. The United States is one of the worst carbon emitters, and so, we must take steps at the national, state, and local level to combat our carbon emissions.", strong("Emissions are cut in one of a few ways: increasing efficiency, reducing use, and increasing natural carbon sequestration (carbon being absorbed OUT of the atmosphere).")), 
                             
                             p(strong("Natural climate solutions are activities that increase natural carbon sequestration or prevent carbon emissions from natural areas. "), "Experts believe that up to a third of our carbon emissions can be mitigated through natural climate solutions. In order to implement natural climate solutions, we need to know how much carbon our natural areas are currently storing, how much they could potentially store, and ways in which they might lose that carbon over time."),
                             
                             p(strong("Select your info (left) and then click through the tabs (top) to explore your area’s Forest Carbon Index (FCI), identify areas for improvement, and print out a report to help you take action."), style="color:blue;")),
                    
                    tabPanel("Facts and Figures",
                             h3(textOutput("stateChosenName")),
                             p(strong("The great state of ", textOutput("justStateName", inline=T), " has ", textOutput("stTotalAc", inline=T), " acres of forestland, which store ", textOutput("stTotalC", inline=T), " tons of carbon.")),
                             p("This gives ", textOutput("justStateName2", inline=T), " a Forest Carbon Index (FCI) of ", textOutput("stTotalFCI", inline=T), " tons of carbon per acre."),
                             p("The state has ", textOutput("stNumFt", inline= T), "different forest types across the state. The acreage and total carbon stored per forest type are represented in the graphs below."),
                             p(),
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("acPie"),  plotOutput("tCPie")),
                             
                             p(strong("Fake Forests")),
                             p("On average, natural forests in ", textOutput("justStateName3", inline=T), " store ", textOutput("additionalC", inline= T), " additional tons of carbon per acre than artificially regenerated forests. Across the state there are ", textOutput("stPlantedAc", inline=T), " acres of plantations, representing ", textOutput("percentage", inline=T), "% of total forestland."),
                             p("If those acres of planted forests had been naturally regenerated, they would currently be storing approximately ", textOutput("stMissedC", inline= T), " more tons of carbon, equivalent to an additional ", textOutput("cars", inline=T), " passenger vehicles on the road for one year."),
                             p("You can see how much more tons of carbon per acre that natural forests store than planted forests"),
                             plotOutput("fakeForestsBar", width= "100%"),
                             #dataTableOutput("stateTable"),
                             p(),
                             p(strong("Natural Climate Solutions")),
                             p(textOutput("justStateName4", inline= T), " could store more carbon, decreasing the effects of climate change, if more of the state's forests were allowed to mature instead of being prematurely cut down and if more forests were naturally regenerated rather than artificially planted. You can see how much more carbon the state's forests would store by picking a percentage from the slider on the left."),
                             
                             ),#end tabPanel
                    tabPanel("Download State Report", 
                             
                             downloadButton("report", "Generate report")
                             )        
                            )
            )
        ),

    ),

    ### back end and data stuff
    
    
    server = function(input, output){
        
        output$userRole <- renderText({
            paste("I am a(n) ", input$user_role)
        })
        
        output$stateChosenName <- renderText({ 
            paste("State Chosen: ", input$state)
        })
        
        output$justStateName <- renderText({input$state})
        output$justStateName2 <- renderText({input$state})
        output$justStateName3<- renderText({input$state})
        output$justStateName4<- renderText({input$state})
        
        stateData <- reactive({
            ##Filter By state
            data <- subset(data, State %in% input$state)
            data
        })
        
        forestTypeData<- reactive({
           data<- subset(data, State== input$state & Stand_Origin== "Total" & Stand_Age== "Total" & 
                                     Forest.type.group!= "Total") 
           data
           
        })
        
        output$stateTable <- renderDataTable({
            datatable(stateData())
        })
        
        fakeForestsData<- reactive({
            d<- subset(data, State %in% input$state & Stand_Age %in% "Total" & Stand_Origin!= "Total") %>% 
                mutate_if(is.numeric, ~replace(., is.na(.), 0))
            d
        })
        
        #stTotal Ac finds the total acreage value in the dataframe for selected state
        output$stTotalAc <- renderText({
            format(
                round(
                    subset(stateData(),
                      Stand_Origin %in% "Total" &
                      Stand_Age %in% "Total" &
                      Forest.type.group %in% "Total")[,"ac"], #endsubset
                    0), #endround 
                big.mark=",")#endformat
           
        })
        #stTotalC finds the total carbon stored value in the dataframe for selected state
        output$stTotalC <- renderText({
          format(
            round(
              subset(stateData(), 
                    Stand_Origin %in% "Total" &
                    Stand_Age %in% "Total" &
                    Forest.type.group %in% "Total")[,"tC"], #endsubset
                  0), #endround 
              big.mark=",")#endformat
        })
        
        #stTotalFCI finds the total forest carbon index value in the dataframe for selected state
        output$stTotalFCI <- renderText({
                round(
                    subset(stateData(),
                               Stand_Origin %in% "Total" &
                               Stand_Age %in% "Total" &
                               Forest.type.group %in% "Total")[,"tC_ac"], #endsubset
                    0) #endround 
            
        })
        #stNumFt finds the number of different forest types in a selected state
        output$stNumFt<- renderText({ 
            n_distinct(select(subset(stateData(), Forest.type.group!= "Total"), Forest.type.group))
        })  
        
        output$acPie<- renderPlot({
           ggplot(forestTypeData(), aes(x="", y= forestTypeData()$ac, fill= forestTypeData()$Forest.type.group)) +
                geom_bar(width=1, stat= "identity") + coord_polar("y", start=0) +
                labs(title= "Acreage per forest type", fill= "Forest Type") +
                theme_void()
        })
        
        output$tCPie<- renderPlot({
            ggplot(forestTypeData(), aes(x="", y= forestTypeData()$tC, fill= forestTypeData()$Forest.type.group)) +
                geom_bar(width=1, stat= "identity") + coord_polar("y", start=0) +
                labs(title= "Tons of carbon stored per forest type", fill= "Forest Type") +
                #scale_fill_viridis(discrete = TRUE) +
                theme_void()
        })
        output$stPlantedAc<- reactive({
            format(subset(data, State %in% input$state & Stand_Origin %in% "Planted" & Stand_Age %in% "Total" &
                       Forest.type.group %in% "Total")[,"ac"], big.mark=",")
        })
        
        output$additionalC<- reactive({
            (subset(data, State %in% input$state & Stand_Origin %in% "Natural" & Stand_Age %in% "Total" &
                       Forest.type.group %in% "Total")[,"tC_ac"]) - 
            (subset(data, State %in% input$state & Stand_Origin %in% "Planted" & Stand_Age %in% "Total" &
                           Forest.type.group %in% "Total")[,"tC_ac"])
        })
        
        output$percentage<- reactive({
            
            round(((subset(data, State %in% input$state & Stand_Origin %in% "Planted" & Stand_Age %in% "Total" &
                               Forest.type.group %in% "Total")[,"ac"])/(subset(data, State %in% input$state & Stand_Origin %in% "Total" &
                Stand_Age %in% "Total" & Forest.type.group %in% "Total")[,"ac"])*100),2)
        })
        
        output$stMissedC<- reactive({
            plantedAc<- subset(data, State %in% input$state & Stand_Origin %in% "Planted" & Stand_Age %in% "Total" &
                                   Forest.type.group %in% "Total")[,"ac"]
            stFCINatural<- subset(state.data, Stand_Origin== "Natural" & Stand_Age== "Total" & Forest.type.group== 
                                      "Total")[, "tC_ac"]
            stPlantedC<- subset(state.data, Stand_Origin== "Planted" & Stand_Age== "Total" & Forest.type.group== 
                                    "Total")[, "tC"]
            format(((plantedAc* stFCINatural)- stPlantedC), big.mark = ",")
        })
        
        output$cars<- reactive({
            plantedAc<- subset(data, State %in% input$state & Stand_Origin %in% "Planted" & Stand_Age %in% "Total" &
                                   Forest.type.group %in% "Total")[,"ac"]
            stFCINatural<- subset(state.data, Stand_Origin== "Natural" & Stand_Age== "Total" & Forest.type.group== 
                                      "Total")[, "tC_ac"]
            stPlantedC<- subset(state.data, Stand_Origin== "Planted" & Stand_Age== "Total" & Forest.type.group== 
                                    "Total")[, "tC"]
            format((((plantedAc * stFCINatural)- stPlantedC)*(44/12) * 0.192608384), big.mark = ",")
        })
        
        output$fakeForestsBar<- renderPlot(
            ggplot(data= fakeForestsData(), aes(x= fakeForestsData()$Forest.type.group, y= fakeForestsData()$tC_ac, fill= fakeForestsData()$Stand_Origin)) +
                geom_col(position= position_dodge()) + 
                ggtitle("FCI per Forest Type Group") +
                labs(x= "Forest Type Group", y= "Forest Carbon Index (FCI)\n", fill= "Stand Origin") +
                geom_text(aes(label=fakeForestsData()$tC_ac), vjust=2.2, color="white",position = position_dodge(0.9), size=2) +
                scale_fill_manual(values= c("#348045", "#B3DCBC")) + theme_minimal() +
                theme(
                    plot.title= element_text(size= 20, face= "bold"),
                    axis.text.x= element_text(angle= 45, size= 8, vjust= .99, hjust= .95, color= "black"),
                    axis.title= element_text(size= 14),
                    legend.title= element_text(size= 12),
                    legend.text = element_text(size= 10),
                    axis.text.y= element_text(size=10, color= "black"))
        )

        output$report <- downloadHandler(
            # For PDF output, change this to "report.pdf"
            filename = "report.html",
            content = function(file) {
                # Copy the report file to a temporary directory before processing it, in
                # case we don't have write permissions to the current working dir (which
                # can happen when deployed).
                tempReport <- file.path(tempdir(), "report.Rmd")
                file.copy("report.Rmd", tempReport, overwrite = TRUE)
                
                # Set up parameters to pass to Rmd document
                params <- list(data= data, n = input$slider, stateText = input$state, user= input$user_role)
                
                # Knit the document, passing in the `params` list, and eval it in a
                # child of the global environment (this isolates the code in the document
                # from the code in this app).
                rmarkdown::render(tempReport, output_file = file,
                                  params = params,
                                  envir = new.env(parent = globalenv()) )
            }
        )
    }
)