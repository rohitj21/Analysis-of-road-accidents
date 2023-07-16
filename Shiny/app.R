library(shiny)
library(markdown)
library(ggplot2)
library(shinythemes)
library(reshape2)
library(htmlwidgets)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
library(sf)
library(dplyr)
library(viridis)
library(ggrepel)
library(ggthemes)
library(plotly)

shp0 = read_sf("IND_adm0.shp")
shp1 = read_sf("IND_adm1.shp")
shp2 = read_sf("IND_adm2.shp")
shp3 = read_sf("IND_adm3.shp")

# load required data sets which have already been cleaned
load("Guj_data.Rdata")
load("Registered_vehicle.Rdata")
load("Time_wise_data.Rdata")
load("all_state_Data.Rdata")
load("Weather 2018.Rdata")
load("population_data.Rdata")


# reading source files for the maps




# ui consists of 5 tabs
ui <- navbarPage("Road Accidents in India", theme = shinytheme("sandstone"),
                 tabPanel("Causewise Distribution",
                          sidebarLayout(
                              sidebarPanel(
                                  sliderInput("selectedYear2", label = "Select Year", min= 2015, max=2020, step = 1, animate = TRUE,value = 2018),
                                  radioButtons("Type", "Incident", choices = c("Deaths per 1,000,000 of the population" = "1", "Injuries per 1,000,000 of the population" = "2", "Accidents per 1,000,000 of the population" = "3"), selected = "3"),
                                  selectInput("selectedStates", "Select States", choices = allData[[1]][[1]][,1],multiple = TRUE, selected = c("Rajasthan","Uttar Pradesh", "Tamil Nadu", "Goa", "Bihar", "Gujrat", "Maharashtra", "Kerala", "Punjab")),
                                  selectInput("selectedCauses", "Select Causes", choices = colnames(allData[[1]][[1]])[2:12],multiple = TRUE, selected = c("Over Speeding","Driving under Influence of Drug or Alcohol", "Careless Driving or Over-taking"))
                              ),
                              mainPanel(
                                  plotlyOutput("plot4")
                              )
                          )
                 ),
                 tabPanel("Type of weather",
                          sidebarLayout(
                              sidebarPanel(
                                  radioButtons("typeofWeather", "Choose the weather", choices = c("Sunny", "Rainy", "Foggy", "Haily"), selected =  "Sunny"),
                                  radioButtons("typeofIncident2", "Choose the Type of Incident", choices = c("Accidents",  "Injured", "Deaths"), selected =  "Accidents")
                              ),
                              mainPanel(
                                  plotlyOutput("plot5")
                              )
                          )
                 ),

                 tabPanel("Registered Motor Vehicles",
                          sidebarLayout(
                              sidebarPanel(
                                  radioButtons("typeofVehicle", "Choose what to see !!", selected = "Cars", choices = c(
                                      "All Transport Vehicles" = "Transport", "Two Wheelers"= "Two_wheeler", "Cars"= "Cars", "All Non-Transport Vehicles" = "Non_transport")),
                                  selectInput("toRemove", "Select Outliers to remove", choices = c("Chennai", "Delhi", "Mumbai" ),multiple = TRUE )
                              ),
                              mainPanel(
                                  plotlyOutput("plot2")
                              )
                          )
                 ),

           tabPanel("Drunk and Drive",
                    sidebarLayout(
                        sidebarPanel(
                            radioButtons("typeofIncident", "Choose what to see !!", choices = c("Accidents", "Deaths", "Injured","All"), selected =  "All")
                        ),
                        mainPanel(
                            plotlyOutput("plot1")
                        )
                    )
           ),
           tabPanel("Time of the Day",
                   sidebarLayout(
                       sidebarPanel(
                           sliderInput("selectedYear1", label = "Select Year", min= 2008, max=2018, step = 1, animate = TRUE,value = 2009 )
                       ),
                       mainPanel(
                           plotlyOutput("plot3")
                       )
                   )
           )


)

server <- function(input, output, session) {
    # 1 plot for drunk and driving
    output$plot1 <- renderPlotly({
        if(input$typeofIncident == "All"){
            Guj = X_Gj
        }
        else {
            Guj = X_Gj[X_Gj$Type == input$typeofIncident,]
        }
        p <- ggplot(Guj,aes(x = Year, y = Incidents, color = Type)  )+
        geom_point() +
        geom_smooth(method = "gam",  formula = y ~ s(x, bs = "cs"), alpha = .2) +
        geom_vline(xintercept = 2009, col = "black", lwd = 1, show.legend = TRUE,linetype = "dashed") + theme_classic()+
        geom_text(aes(x = 2009 ,y = (max(Incidents)+min(Incidents))/2,  label = "\n ALCOHOL BANNED") , col = "black", angle = 180, lwd = 4)+
        labs(title ="Road Accidents in Gujrat" ) + theme(plot.title = element_text(face="bold"))+ theme(plot.title = element_text(hjust = 0.5))+
        xlab("Year") + ylab("Number of Incidents")+
        labs(caption = "(based on data from ...)")

        ggplotly(p)
    })

    # 2 plot for Registered motor vehicles

    output$plot2 <- renderPlotly({
        newInfo <- city_info[!(city_info$City %in% input$toRemove),]
        NumberOfVehicles <-  newInfo[,(colnames(newInfo) ==  input$typeofVehicle)]
        CityPopulation <-  factor(round(log10(newInfo$Population)))

        p <-ggplot(newInfo, aes(x = log10(NumberOfVehicles) , y = log10(Cases), color = CityPopulation, label = City, size = Deaths)) +
            geom_point(alpha= 0.7) +
            geom_text(aes(label=ifelse(Population>2.5e6,City,''), size = 12),hjust=0,vjust=0, size = 3) +
            geom_smooth(method = "lm", alpha = .0, formula = y~x)+  theme_classic()+
            xlab(paste("Number of", (input$typeofVehicle))) + ylab("Number of Accidents")  +
            labs(title = "Number of Road Accidents in Major Cities of India") +
            theme(plot.title=element_text(hjust=0.5))+ theme(plot.title = element_text(face="bold"))+
            guides(color =guide_legend(title="Order of magnitude of the Population"))
            ggplotly(p)
    })

    # 3 code for time of occurance plot
    output$plot3 <- renderPlotly({
        X_timewise <- TimeWiseData[[input$selectedYear1 - 2007]]
        p <- ggplot(X_timewise, aes(x = `Time of Occurence`, y = `Number of Accidents`, fill = `Day or Night`))+
            geom_bar(stat = 'identity') + ylim(min = 0, max = 90000)  + coord_flip() + theme_classic()+
            labs(title = "Road Accidents in India V/S Time of Occurrence") + theme(plot.title = element_text(face="bold"))+
            theme(plot.title=element_text(hjust=0.5))
        ggplotly(p)
    })

    # 4 Causewise Distribution

    output$plot4 <- renderPlotly({
        X <- melt(allData[[input$selectedYear2-2014]][[as.numeric(input$Type)]], value.name = "incidents", variable.name = "Cause")
        X <- X[X$State %in% input$selectedStates,]
        X <- X[X$Cause %in% input$selectedCauses,]
        p <- ggplot(X, aes(State, incidents, fill = Cause)) +
            geom_bar(stat = 'identity')+
            ylab(ifelse(input$Type == '1', "Deaths per 1,000,000 of the population", ifelse(input$Type == '2',"Injuries per 1,000,000 of the population" ,"Accidents per 1,000,000 of the population"))) +
            coord_flip() + theme_classic()+
            labs(title = "Causes of Accidents in States of India") +
            theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(face="bold"))

        ggplotly(p)
    })


    # 5 Weather analysis

    output$plot5 <- renderPlotly({
          ggplotly(plotMap( input$typeofWeather, input$typeofIncident2))


    })


}

shinyApp(ui, server)
