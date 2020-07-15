
### library imports
library(shiny)
library(DT)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shinyWidgets)

## read in data 
d <- read.csv("CIP_Type1_2020-04-07.csv", header=T, na.strings=c("-")) 
d$State<- as.character(d$State)
data<-subset(d, State!= "Texas" & State!= "Oklahoma" & State!= "Missouri")
data$State<- factor(data$State)

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
                img(src="dogwood-logo-color-01.png", height= "20%", width= "80%"), #add Dogwood logo at the top
                tabsetPanel(type="tabs", 
                            
                    tabPanel("Introduction", 
                             br(),
                             p(strong("Dealing with climate change can be a challenging prospect for our nation’s local leaders."), " There is ", em("a lot"), " of information out there, but not a lot of easy ways to apply it. This project is meant to bridge the gap between data availability and data usability."), 
                             p("Climate change is happening because there is simply too much carbon dioxide, and other greenhouse gases, entering the atmosphere. This is creating a warming effect that is changing our weather patterns and possibilities of extreme events. The United States is one of the worst carbon emitters, and so, we must take steps at the national, state, and local level to combat our carbon emissions.", strong("Emissions are cut in one of a few ways: increasing efficiency, reducing use, and increasing natural carbon sequestration (carbon being absorbed OUT of the atmosphere).")), 
                             
                             p(strong("Natural climate solutions are activities that increase natural carbon sequestration or prevent carbon emissions from natural areas. "), "Experts believe that up to a third of our carbon emissions can be mitigated through natural climate solutions. In order to implement natural climate solutions, we need to know how much carbon our natural areas are currently storing, how much they could potentially store, and ways in which they might lose that carbon over time."),
                             
                             p(strong("Select your info (left) and then click through the tabs (top) to explore your area’s Forest Carbon Index (FCI), identify areas for improvement, and print out a report to help you take action."), style="color:steelblue;")),
                    
                    tabPanel("Facts and Figures",
                             h3(textOutput("stateChosenName")),
                             p(strong("The great state of ", textOutput("justStateName", inline=T), " has ", textOutput("stTotalAc", inline=T), " acres of forestland, which store ", textOutput("stTotalC", inline=T), " tons of carbon."), style="color:steelblue;"),
                             p("This gives ", textOutput("justStateName2", inline=T), " a Forest Carbon Index (FCI) of ", textOutput("stTotalFCI", inline=T), " tons of carbon per acre."),
                             p("The state has ", textOutput("stNumFt", inline= T), "different forest types across the state. The acreage and total carbon stored per forest type are represented in the graphs below."),
                             br(),
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("acPie"),  plotOutput("tCPie")),
                             h4("Natural Climate Solutions"),
                             h5("Letting Forests Regenerate Naturally"),
                             p("On average, natural forests in ", textOutput("justStateName3", inline=T), " store ", textOutput("additionalC", inline= T), " additional tons of carbon per acre than artificially regenerated forests. Across the state there are ", textOutput("stPlantedAc", inline=T), " acres of plantations, representing ", textOutput("percentage", inline=T), "% of total forestland."),
                             p("If those acres of planted forests had been naturally regenerated, they would currently be storing approximately ", textOutput("stMissedC", inline= T), " more tons of carbon, equivalent to an additional ", textOutput("cars", inline=T), " passenger vehicles on the road for one year."),
                             p("You can see how many more tons of carbon the state's artificially planted forests would store if they were naturally generated by picking a percentage from the slider on the left."),
                             p(strong("If the state moved ", textOutput("percentText2", inline= T), "% of its artificially planted forests to naturally regenerated, it would result in an additional ", textOutput("letForestsRegenerate", inline= T), " tons of carbon sequestered.")),
                             plotOutput("fakeForestsBar", width= "100%"),
                             p("Although planting trees does regenerate the area more quickly, there are lasting effects from those plantings that are seen in carbon sequestration. Naturally regenerated forest stands usually store more carbon than artificially regenerated (planted) stands. Beyond carbon benefits, natural regeneration also reduces the amount of pesticides, soil impaction, and water runoff that can occur after a forest harvest."), 
                             br(),
                             h5("Letting Forests Mature"),
                             p(textOutput("justStateName4", inline= T), " could store more carbon, decreasing the effects of climate change, if more of the state's forests were allowed to mature instead of being prematurely cut down. Additionally, old growth forests support more biodiversity and provide more ecosystem services than younger forests."), 
                             p("You can see how much more carbon the state's forests would store if allowed to mature by picking a percentage from the slider on the left."),
                             p(strong("If the state moved ", textOutput("percentText", inline= T), "% of all forests to the next age class, it would result in an additional ", textOutput("letForestsMature", inline= T), " tons of carbon sequestered.")),
                             p("Every time an acre of forest is harvested, it releases carbon. If that acre is used for bioenergy, the carbon can be emitted in as little as 1-2 years. If that acre is used for more traditional forest products, like paper, pulp, or lumber, 86% of that carbon is emitted within 100 years, with just 14% remaining behind in finished products and landfills. If allowed to fully mature,  older forests continue to store carbon well beyond 100 years of age."),
                             p("As shown in the graph below, the total FCI, or tons of carbon stored per acre, in ", textOutput("justStateName5", inline=T), " becomes greater as forests age."),
                             plotOutput("forestAgeBar", width= "90%"),
                             br(),
                             em("All data gathered from: USDA Forest Service, Forest Inventory and Analysis Program, Tue Mar 31 13:53:53 GMT 2020. Forest Inventory EVALIDator web-application Version 1.8.0.01. St. Paul, MN: U.S. Department of Agriculture, Forest Service, Northern Research Station. [Available only on internet: http://apps.fs.usda.gov/Evalidator/evalidator.jsp]"),
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
        
        output$stateChosenName <- renderText({ 
            paste("State Chosen: ", input$state)
        })
 #rendering inputted state as text       
        output$justStateName <- renderText({input$state})
        output$justStateName2 <- renderText({input$state})
        output$justStateName3<- renderText({input$state})
        output$justStateName4<- renderText({input$state})
        output$justStateName5<- renderText({input$state})
 #rendering inputted percentage as text       
        output$percentText<- renderText({input$slider})
        output$percentText2<- renderText({input$slider})
        
        stateData <- reactive({
            ##Filter By state
            data <- subset(data, State %in% input$state)
            data
        })
        
        forestTypeData<- reactive({
            #Filter data to include only totals values for each forest type for pie charts
           data<- subset(stateData(), Stand_Origin== "Total" & Stand_Age== "Total" & 
                                     Forest.type.group!= "Total") 
           data
           
        })
        
        fakeForestsData<- reactive({
            #Filter data to include only total Stand Age rows for bar graph showing FCI of natural and planted stands per forest type group 
            d<- subset(stateData(), Stand_Age == "Total" & Stand_Origin!= "Total") %>% 
                mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%  #replace NA values with )
                mutate_at(vars(tC_ac), funs(round(., 0))) #round FCI column to whole number
            
            #reorder the forest type groups so that the Total value comes first followed by specific groups in alphabetical order
            level_order<- c('Total', 'Aspen / birch group', 'Elm / ash / cottonwood group', 'Exotic hardwoods group', 'Exotic softwoods group',
                            'Loblolly / shortleaf pine group', 'Longleaf / slash pine group', 'Maple / beech / birch group', 'Nonstocked', 'Oak / gum / cypress group', 'Oak / hickory group', 'Oak / pine group', 'Other eastern softwoods group', 'Other hardwoods group', 'Pinyon juniper group', 'Spruce / fir group', 'Tropical hardwoods group', 'White / red / jack pine group', 'Woodland hardwoods group')
            d$Forest.type.group<- factor(d$Forest.type.group, levels= level_order)
            
            d
        })
        
        ageData<- reactive({
            #subset for totals values for bar graph showing FCI per forest age class
            age.data<- subset(stateData(), Stand_Origin== "Total" & Forest.type.group== "Total" & Stand_Age!= "Total") %>% 
                mutate_at(vars(tC_ac), funs(round(., 0))) #round FCI values to whole number
            age.data$Stand_Age<- factor(age.data$Stand_Age, levels= c("0-20 years", "21-40 years", "41-60 years", "61-80 years", "81-100 years", "100+ years"))
            age.data
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
        
        #Pie chart of acreage  by forest type via geom_bar + coord_polar
        output$acPie<- renderPlot({
           ggplot(forestTypeData(), aes(x="", y= forestTypeData()$ac, fill= forestTypeData()$Forest.type.group)) +
                geom_bar(width=1, stat= "identity") + coord_polar("y", start=0) +
                labs(title= "Acreage per forest type\n", fill= "Forest Type") +
                theme_void()
        })
        
        #pie chart of tons of carbon stored by forest type via geom_bar + coord_polar
        output$tCPie<- renderPlot({
            ggplot(forestTypeData(), aes(x="", y= forestTypeData()$tC, fill= forestTypeData()$Forest.type.group)) +
                geom_bar(width=1, stat= "identity") + coord_polar("y", start=0) +
                labs(title= "Tons of carbon stored per forest type\n", fill= "Forest Type") +
                #scale_fill_viridis(discrete = TRUE) +
                theme_void()
        })
        
        #selecting the value equal to the total acreage of all planted forests in the state
        output$stPlantedAc<- reactive({
            format(subset(stateData(), Stand_Origin %in% "Planted" & Stand_Age %in% "Total" &
                       Forest.type.group %in% "Total")[,"ac"], big.mark=",")
        })
        
        #calculates the additional tons of carbon per acreage stored by natural forests
        output$additionalC<- reactive({
            (subset(stateData(), Stand_Origin %in% "Natural" & Stand_Age %in% "Total" &
                       Forest.type.group %in% "Total")[,"tC_ac"]) - 
            (subset(stateData(), Stand_Origin %in% "Planted" & Stand_Age %in% "Total" &
                           Forest.type.group %in% "Total")[,"tC_ac"])
        })
        #calculates the percentage of land holding plantations in the state
        output$percentage<- reactive({
            round(((subset(stateData(), Stand_Origin %in% "Planted" & Stand_Age %in% "Total" &
                               Forest.type.group %in% "Total")[,"ac"])/(subset(stateData(), Stand_Origin %in% "Total" &
                Stand_Age %in% "Total" & Forest.type.group %in% "Total")[,"ac"])*100),2)
        })
        
        #calculates the extra carbon that would be stored if the land used for plantations held natural forests instead
        output$stMissedC<- reactive({
            plantedAc<- subset(stateData(), Stand_Origin %in% "Planted" & Stand_Age %in% "Total" &
                                   Forest.type.group %in% "Total")[,"ac"]
            stFCINatural<- subset(stateData(), Stand_Origin== "Natural" & Stand_Age== "Total" & Forest.type.group== 
                                      "Total")[, "tC_ac"]
            stPlantedC<- subset(stateData(), Stand_Origin== "Planted" & Stand_Age== "Total" & Forest.type.group== 
                                    "Total")[, "tC"]
            format(((plantedAc* stFCINatural)- stPlantedC), big.mark = ",")
        })
        
        #calculates how many cars are equal to the carbon emissions that would be stored if the land used for plantations held natural forests instead
        output$cars<- reactive({
            plantedAc<- subset(stateData(), Stand_Origin %in% "Planted" & Stand_Age %in% "Total" &
                                   Forest.type.group %in% "Total")[,"ac"]
            stFCINatural<- subset(stateData(), Stand_Origin== "Natural" & Stand_Age== "Total" & Forest.type.group== 
                                      "Total")[, "tC_ac"]
            stPlantedC<- subset(stateData(), Stand_Origin== "Planted" & Stand_Age== "Total" & Forest.type.group== 
                                    "Total")[, "tC"]
            format((((plantedAc * stFCINatural)- stPlantedC)*(44/12) * 0.192608384), big.mark = ",")
        })
        
        #plot a bar graph to show the FCI per forest type group for natural and planted stands
        output$fakeForestsBar<- renderPlot(
            ggplot(data= fakeForestsData(), aes(x= fakeForestsData()$Forest.type.group, y= fakeForestsData()$tC_ac, fill= fakeForestsData()$Stand_Origin)) +
                geom_col(position= position_dodge()) + 
                ggtitle("FCI per Forest Type Group") +
                labs(x= "Forest Type Group", y= "Forest Carbon Index (FCI)\n", fill= "Stand Origin") +
                geom_text(aes(label=fakeForestsData()$tC_ac), vjust=2.2, color="white",position = position_dodge(0.9), size=4) +
                scale_fill_manual(values= c("#348045", "#B3DCBC")) + theme_minimal() +
                theme( #adjusting text sizes
                    plot.title= element_text(size= 20, face= "bold"),
                    axis.text.x= element_text(angle= 45, size= 8, vjust= .99, hjust= .95, color= "black"),
                    axis.title= element_text(size= 14),
                    legend.title= element_text(size= 12),
                    legend.text = element_text(size= 10),
                    axis.text.y= element_text(size=10, color= "black"))
        )
        
        #plot a bar graph to show increasing FCI by stand age
        output$forestAgeBar<- renderPlot(
            ggplot(ageData(), aes(x= Stand_Age, y= tC_ac)) +
                geom_col(fill= "darkolivegreen4") + 
                ggtitle("FCI per Forest Age Class") +
                labs(x= "Forest Age Class", y= "Forest Carbon Index (FCI)\n", fill= "Stand Origin") +
                geom_text(aes(label=tC_ac), vjust=2.2, color="white",position = position_dodge(0.9), size=5) + theme_minimal() +
                theme( #adjusting text sizes
                    plot.title= element_text(size= 20, face= "bold"),
                    axis.text.x= element_text(size= 10, color= "black", vjust= .99),
                    axis.title= element_text(size= 14),
                    axis.text.y= element_text(size=10, color= "black"))
        )
        
        #function to calculate how much more carbon would be stored if a user inputted percentage of all forests were moved to the next age class
        output$letForestsMature<- reactive({
            #subset data to not include total stand age rows and omit the missing values
            model.data<- subset(stateData(), stateData()$Stand_Age!= "Total") %>% na.omit()
            #reorder the categories of stand age
            model.data$Stand_Age<- factor(model.data$Stand_Age, levels= c("0-20 years", "21-40 years", "41-60 years", "61-80 years", "81-100 years", "100+ years"))
            linear.mod<- lm(tC ~ Stand_Age + ac, data= model.data) #linear regression of effect on stand age and acreage on carbon storage
            predict.data<- subset(model.data, Stand_Origin== "Total" & Forest.type.group== "Total") #subset data for total acreage in each age class
            decimal<- as.numeric(input$slider)/100
            more<- 0 #accumulator variable
            for (x in predict.data$Stand_Age){ 
                idx<- which(predict.data$Stand_Age== x) #find index of current age class in for loop
                ac.val<- (predict.data$ac[idx]) #find corresponding acreage value for the age class
                if(x!= "100+ years"){ #condition to terminate loop at the final age class
                    newdata<- data.frame(ac= ac.val*decimal,Stand_Age= predict.data$Stand_Age[idx+1]) #create dataframe for prediction
                    more<- more + as.numeric(predict(linear.mod, newdata, type="response")) #predict total carbon value of the next age class and add to accumulator 
                    }
                }
            format(round(more, 0),  big.mark=",") #round and format value
        })
        
        #function to calculate how much more carbon on average would be stored if a user inputted percentage of all forests were naturally generated instead of artificially planted
        output$letForestsRegenerate<- reactive({
                model.data2<- subset(stateData(), stateData()$Stand_Origin!= "Total") %>% na.omit() #exclude Stand_Origin total values and omit NAs
                decimal<- as.numeric(input$slider)/100
                avg<- mean(model.data2$tC_ac[model.data2$Stand_Origin== "Natural"]) #find average FCI of Natural Stands
                acreage<- subset(stateData(), Stand_Origin=="Planted" & Stand_Age=="Total" & Forest.type.group=="Total")[,"ac"] #find total acreage of planted forests
                format(round(avg*acreage*decimal, 0), big.mark=",")
        })

    #generate downloadable report
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