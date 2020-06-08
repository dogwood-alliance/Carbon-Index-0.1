#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


### library imports
install.packages("shinyWidgets")
install.packages("tidyverse")

library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)
library(shinyWidgets)
library(reshape2)
library(dplyr)
library(plyr)


## read in data 
data <- read.csv("CIP_Type1_2020-04-07.csv", header=T, na.strings=c("-"))
View(data)
head(data)
str(data)

# Plantation in the US South
data_clean=subset(data,data$Stand_Age!="Total"&data$Stand_Origin!="Total"&data$Forest.type.group!="Total")
data$Stateshort=mapvalues(data$State,from = c("Alabama","Arkansas","Florida","Georgia","Kentucky","Louisiana","Mississippi","Missouri","North Carolina","Oklahoma","South Carolina","Tennessee","Texas","Virginia"),to=c("AL","AR","FL","GA","KY","LA","MO","MS","NC","OK","SC","TN","TX","VA"))

# Forest areas by forest age
AgeForest<-read.csv("ageforest.csv")

AgeForestTotal<-subset(AgeForest,AgeForest$Measurement=="acres"&AgeForest$Type=="ALL"&AgeForest$Forest.type=="Total")                     
AgeForestTotal<-AgeForestTotal[,-c(1,2,4,5)]

names(AgeForestTotal)<-c("State","0.to.20.years","21.to.40.years","41.to.60.years","61.to.80.years","81.to.100.years","101.to.120.years","121.to.140.years","141.to.160.years","161.to.180.years","181.to.200.years","500+years")
AgeForestTotal<-melt(AgeForestTotal,id.vars = "State")
AgeForestTotal$value<-as.numeric(AgeForestTotal$value)

AgeForestTotal<-group_by(AgeForestTotal,State) %>%
    mutate(percent = value/sum(value))
AgeForestTotal$value<-as.numeric(AgeForestTotal$value)


# Change in Forest areas by type
his_pine<-read.csv("Historical_pine.csv")

his_pine_long<-melt(his_pine, id.vars = c("State","Forest.Type"))
names(his_pine_long)[c(3,4)]<-c("Year","Acre.Land")
his_pine_long$Year<-as.numeric(gsub("X","",his_pine_long$Year))
his_pine_long$Acre.Land<-as.numeric(gsub(",","",his_pine_long$Acre.Land))
his_pine_long<-his_pine_long[-which(his_pine_long$State=="ZTotal"|his_pine_long$Forest.Type %in% c("All","PercentPine")),]
his_pine_long$Forest.Type=factor(his_pine_long$Forest.Type,levels = c("Oak-pine","Lowland hardwood","Upland hardwood","Planted pine","Natural Pine"))




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
                            levels(data$State), options=list(`actions-box` = TRUE), multiple = F, selected="Alabama"),
                
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
                             ),
                    tabPanel("Plantation in the US South",
                             p(plotOutput("graphtotal"),plotOutput("ForestAge"),plotOutput("changetotal"))
                            ),
                    tabPanel("Plantation by State ",
                             p(plotOutput("changestate")))
            )
        ),

    )),
    
    
    
    
    
    
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

                params <- list(data= data, n = input$slider, stateText = input$state, user= input$user_role)

                params <- list(n = input$slider, stateText = input$state)

                # Knit the document, passing in the `params` list, and eval it in a
                # child of the global environment (this isolates the code in the document
                # from the code in this app).
                rmarkdown::render(tempReport, output_file = file,
                                  params = params,
                                  envir = new.env(parent = globalenv())
                )
            }
        )
        
        output$graphtotal <- renderPlot({ggplot()+
                geom_bar(data=subset(data,data$Stand_Age!="Total"&data$Stand_Origin!="Total"&data$Forest.type.group!="Total"), aes(x=Stateshort,y=ac, fill=Stand_Origin),stat="identity")+
                geom_text(data=subset(data,data$Stand_Age=="Total"&data$Stand_Origin=="Total"&data$Forest.type.group=="Total"), aes(x=Stateshort, y=ac, label=round(ac/1000000,digits=2)), position = position_stack(vjust = 1.1), size=3)+
                scale_y_continuous()+
                theme(axis.text.y=element_blank(),axis.ticks = element_blank())+
                labs(title="Forest area in the US South, by stand origin", x="State",y="Area (million acres)")
        })
        
        output$changetotal<- renderPlot({ ggplot(his_pine_long, aes(y=Acre.Land/1000,x=Year, group=Forest.Type, color=Forest.Type))+
                geom_area(aes(fill=Forest.Type),color="gray")+
                scale_x_continuous(breaks=c(1952,1962,1970,1982,1989,1999))+
                scale_y_continuous(breaks=seq(0,200, by = 10))+
                labs(title="Changes in Forest Areas by Forest Type", y="Area (million acres)", x="Year", legend="Forest Type")+
                facet_wrap(~State)
        })
        
        output$ForestAge<-renderPlot({
                ggplot(AgeForestTotal, aes(x=State,y=percent, group=State, fill= variable))+
                    geom_bar(stat="identity", width = 1, color="gray")+
                    geom_text(aes(label=paste0(round(percent,digits=2)*100,"%")), position = position_stack(vjust = 0.5), size=3)+
                    scale_y_continuous(name = "Percentage",axis(ticks=F,labels = F))
        })
        
        output$changestate<-renderPlot({
            ggplot(subset(his_pine_long,State %in% input$state), aes(y=Acre.Land,x=Year, group=Forest.Type))+
                geom_area(aes(fill=Forest.Type),color="gray")+
                scale_x_continuous(breaks=c(1952,1962,1970,1982,1989,1999))+
                scale_y_discrete(breaks=seq(0,200, by = 20))+
                geom_text(aes(label=paste0(round(Acre.Land/1000,digits=2))),position = position_stack(vjust = 0.5),size=3)+
                labs(title="Changes in Forest Areas by Forest Type", subtitle = input$state, y="Area (million acres)", x="Year", legend="Forest Type")
        })
    }
)



   
ggplot(AgeForestTotal, aes(x=State,y=value, group=State, fill= variable), vjust=1)+
    geom_bar(stat="identity", width = 1)+
    geom_text(aes(label=paste0(round(value/1000000, digits = 1))), position = position_stack(vjust = 0.5))
    

ggplot(his_pine_long, aes(y=Acre.Land,x=Year, group=Forest.Type,shape=Forest.Type))+
    geom_area(aes(fill=Forest.Type),color="gray")+
    scale_x_continuous(breaks=c(1952,1962,1970,1982,1989,1999))+
    scale_y_discrete(breaks=seq(0,200000, by = 20000))+
    facet_wrap(~State,ncol = 3)

ggplot(subset(his_pine_long,his_pine_long$State==input$State), aes(y=Acre.Land,x=Year, group=Forest.Type))+
    geom_area(aes(fill=Forest.Type),color="gray")+
    scale_x_continuous(breaks=c(1952,1962,1970,1982,1989,1999))+
    scale_y_discrete(breaks=seq(0,200000, by = 20000))+
    geom_text(aes(label=paste0(round(Acre.Land/1000,digits=2))),position = position_stack(vjust = 0.5),size=3)+
    labs(title="Changes in Forest Areas by Forest Type", subtitle = textOutput(input$State), y="Area (million acres)", x="Year", legend="Forest Type")



