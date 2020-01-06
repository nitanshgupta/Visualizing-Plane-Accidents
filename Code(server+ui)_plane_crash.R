#Shiny Assignment 2: Nitansh Gupta : NXG180004


library(tidyverse) 
library(shiny)
library(leaflet)
library(DT)
library(stringr)
library(shinyWidgets)
library(descr)

user_data <- read.csv("yelp_user.csv")

##---------------Data Cleaning-----------------------------------------
plane_accident_data <- read.csv("Plane_Accidents1.csv", sep = ';',stringsAsFactors = F)
plane_accident_data1 <- distinct(plane_accident_data, ... = plane_accident_data$Event.Id, .keep_all = TRUE)
plane_accident_data1$Injury.Severity <-  replace(plane_accident_data1$Injury.Severity, startsWith(plane_accident_data1$Injury.Severity, "F"), 'Fatal')
plane_accident_data1$Country[!(plane_accident_data1$Country == "United States")]= "Others"
plane_accident_data1$Event.Date<- as.Date(plane_accident_data1$Event.Date, format = "%Y-%m-%d")
plane_accident_data2 <- plane_accident_data1 #, cols="plane_accident1$Longitude"
plane_accident_data2$year <- as.numeric(substring(plane_accident_data2$Event.Date,1,4))
##--------------------UI-------------------------------------------------
renderinputa <- function(){
  wellPanel(
    fluidRow(
      column(6,
             pickerInput(inputId = "a_country",label = "Country",choices = unique(plane_accident_data2$Country), options = list(`actions-box` = TRUE),multiple = T),
             #selectInput(inputId = paste0(prefix, "_", "country"),label = "Country",choices = c("All",unique(plane_accident_data2$Country)),selected = "All", multiple = TRUE),
             pickerInput(inputId = "a_location",label = "Location",choices = unique(plane_accident_data2$Location), options = list(`actions-box` = TRUE), multiple = T),
             pickerInput(inputId = "a_flight_purpose",label = "Flight Purpose",choices = unique(plane_accident_data2$Purpose.of.Flight), options = list(`actions-box` = TRUE), multiple = T)
      ),
      column(6,
             pickerInput(inputId = "a_air_category",label = "Aircraft Category",choices = unique(plane_accident_data2$Aircraft.Category), options = list(`actions-box` = TRUE), multiple = T),
             pickerInput(inputId = "a_air_engine_n",label = "Aircraft Engines#",choices = unique(plane_accident_data2$Number.of.Engines), options = list(`actions-box` = TRUE), multiple = T),
             pickerInput(inputId = "a_air_engine_type",label = "Aircraft Engine Type",choices = unique(plane_accident_data2$Engine.Type), options = list(`actions-box` = TRUE), multiple = T)
      )
    ),
    sliderInput(inputId = "a_year_range",label = "Time Period (Years):", min = 1948, max = 2019, value = c(1948,2018))
  )}
renderinputb <- function(){
  wellPanel(
    fluidRow(
      column(6,
             pickerInput(inputId = "b_country",label = "Country",choices = unique(plane_accident_data2$Country), options = list(`actions-box` = TRUE),multiple = T),
             #selectInput(inputId = paste0(prefix, "_", "country"),label = "Country",choices = c("All",unique(plane_accident_data2$Country)),selected = "All", multiple = TRUE),
             pickerInput(inputId = "b_location",label = "Location",choices = unique(plane_accident_data2$Location), options = list(`actions-box` = TRUE), multiple = T),
             pickerInput(inputId = "b_flight_purpose",label = "Flight Purpose",choices = unique(plane_accident_data2$Purpose.of.Flight), options = list(`actions-box` = TRUE), multiple = T)
      ),
      column(6,
             pickerInput(inputId = "b_air_category",label = "Aircraft Category",choices = unique(plane_accident_data2$Aircraft.Category), options = list(`actions-box` = TRUE), multiple = T),
             pickerInput(inputId = "b_air_engine_n",label = "Aircraft Engines#",choices = unique(plane_accident_data2$Number.of.Engines), options = list(`actions-box` = TRUE), multiple = T),
             pickerInput(inputId = "b_air_engine_type",label = "Aircraft Engine Type",choices = unique(plane_accident_data2$Engine.Type), options = list(`actions-box` = TRUE), multiple = T)
      )
    ),
    sliderInput(inputId = "b_year_range",label = "Time Period (Years):", min = 1948, max = 2019, value = c(1948,2018))
  )}

###-------------------------
ui<- fluidPage(theme="simplex.min.css",
               tags$style(type="text/css",
                          "label {font-size: 12px;}",
                          ".recalculating {opacity: 1.0;}"
               ),
               setBackgroundColor(
                 color = '#f4f4f4'),
               
               # Application title
               tags$h1("Aircraft Accidents"),
               tags$h4("The following graphical dashboard answers the three major questions, with added liberty to user to compare different scenerios side-by-side"),
               
               
               fluidRow(
                 column(6, tags$h3("Scenario A")),
                 column(6, tags$h3("Scenario B"))
               ),
               fluidRow(
                 column(6, renderinputa()),
                 column(6, renderinputb())
               ),tags$h3("To Check: If most crashes occure during takeoff"),
               
               fluidRow(
                 column(6,
                        plotOutput("a_distPlot1", height = "400px")
                 ),
                 column(6,
                        plotOutput("b_distPlot1", height = "400px")
                 )
               ),br(),br(),tags$h3("To Check: If most crashes are fatal or not"),
               fluidRow(
                 column(6,
                        plotOutput("a_distPlot2", height = "400px")
                 ),
                 column(6,
                        plotOutput("b_distPlot2", height = "400px")
                 )
               ),br(),br(),tags$h3("To Check: If Air travel has become safer"),
               fluidRow(
                 column(6,
                        plotOutput("a_distPlot3", height = "400px")
                 ),
                 column(6,
                        plotOutput("b_distPlot3", height = "400px")
                 )
               )
)
##--------------------Server-------------------------------------------------
server <- function(input,output,session) {
  a_filteredData <-reactive({
    a_filteredData = plane_accident_data2[plane_accident_data2$Country %in%  input$a_country &
                                            plane_accident_data2$Location %in%  input$a_location &
                                            plane_accident_data2$Purpose.of.Flight %in%  input$a_flight_purpose &
                                            plane_accident_data2$Aircraft.Category %in%  input$a_air_category &
                                            plane_accident_data2$Number.of.Engines %in%  input$a_air_engine_n &
                                            plane_accident_data2$Engine.Type %in%  input$a_air_engine_type &
                                            plane_accident_data2$year >= input$a_year_range[1] &
                                            plane_accident_data2$year <= input$a_year_range[2] ,]
  })
  
  b_filteredData <-reactive({
    b_filteredData = plane_accident_data2[plane_accident_data2$Country %in%  input$b_country &
                                            plane_accident_data2$Location %in%  input$b_location &
                                            plane_accident_data2$Purpose.of.Flight %in%  input$b_flight_purpose &
                                            plane_accident_data2$Aircraft.Category %in%  input$b_air_category &
                                            plane_accident_data2$Number.of.Engines %in%  input$b_air_engine_n &
                                            plane_accident_data2$Engine.Type %in%  input$b_air_engine_type &
                                            plane_accident_data2$year >= input$b_year_range[1] &
                                            plane_accident_data2$year <= input$b_year_range[2],]
  })
  #a_filteredData() %>% 
  a_bpf <- reactive({as.data.frame(a_filteredData() %>% 
                                     group_by(Broad.Phase.of.Flight) %>% tally())})
  
  b_bpf <- reactive({as.data.frame(b_filteredData() %>% 
                                     group_by(Broad.Phase.of.Flight) %>% tally()  )})
  a_fatal <- reactive({as.data.frame(a_filteredData() %>% 
                                       group_by(Injury.Severity) %>% tally() )})
  b_fatal <- reactive({as.data.frame(b_filteredData() %>% 
                                       group_by(Injury.Severity) %>% tally() )})
  a_year <- reactive({as.data.frame(subset(a_filteredData(),Injury.Severity == "Fatal") %>%group_by(year) %>% tally() )})
  b_year <- reactive({as.data.frame(subset(b_filteredData(), Injury.Severity == "Fatal") %>%group_by(year) %>% tally() )})
  
  #mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
  
  output$a_distPlot1 <- renderPlot({ ggplot(data=a_bpf(), aes(x = Broad.Phase.of.Flight, y = n)) +
      geom_bar(stat="identity", fill='#1491af') +
      geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
      theme_minimal()+ ggtitle("Accidents# by Flight Phase") +
      ylab("Accidents Count") + 
      xlab("Phase of Flight")+
      theme(plot.title = element_text(size=16, color="#181818",hjust=0.5)) +
      theme(axis.title = element_text(color="#181818", face="bold", size=12))})
  
  output$b_distPlot1 <-renderPlot({ ggplot(data=b_bpf(), aes(x = Broad.Phase.of.Flight, y = n)) +
      geom_bar(stat="identity", fill='#00a7a2') +
      geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
      theme_minimal()+ ggtitle("Accidents# by Flight Phase")+
      ylab("Accidents Count") + 
      xlab("Phase of Flight")+
      theme(plot.title = element_text( size=16, color="#181818",hjust=0.5)) +
      theme(axis.title = element_text(color="#181818", face="bold", size=12)) })
  
  output$a_distPlot2 <-renderPlot({ ggplot(data=a_fatal(), aes(x = Injury.Severity, y = n)) +
      geom_bar(stat="identity", fill='#1491af') +
      geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
      theme_minimal()+ ggtitle("Accidents# by Severity")+
      ylab("Accidents Count") + 
      xlab("Severity of Accidents")+
      theme(plot.title = element_text(size=16, color="#181818",hjust=0.5)) +
      theme(axis.title = element_text(color="#181818", face="bold", size=12))})
  
  output$b_distPlot2 <-renderPlot({ ggplot(data=b_fatal(), aes(x = Injury.Severity, y = n)) +
      geom_bar(stat="identity", fill='#00a7a2') +
      geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
      theme_minimal()+ ggtitle("Accidents# by Severity")+
      ylab("Accidents Count") + 
      xlab("Severity of Accidents")+
      theme(plot.title = element_text(size=16, color="#181818",hjust=0.5)) +
      theme(axis.title = element_text(color="#181818", face="bold", size=12))})
  
  output$a_distPlot3 <-renderPlot({ ggplot(data=a_year(), aes(x = year, y= n)) + 
      geom_line(stat="identity", color = '#1491af')+
      geom_area(fill = '#1491af', alpha = 1)+
      geom_text(aes(label=n), vjust=1.6, size=3.5)+
      ylab("Accidents Count") + 
      xlab("Year") + ggtitle("Fatal Accidents Over Years")+
      theme(plot.title = element_text(size=16, color="#181818",hjust=0.5)) +
      theme(axis.title = element_text(color="#181818", face="bold", size=12))})
  
  output$b_distPlot3 <-renderPlot({ggplot(data=b_year(), aes(x = year, y= n)) + 
      geom_line(stat="identity", color = '#00a7a2')+
      geom_area(fill = '#00a7a2', alpha = 1)+
      geom_text(aes(label=n), vjust=1.6, size=3.5)+
      ylab("Accidents Count") + 
      xlab("Year") + ggtitle("Fatal Accidents Over Years") +
      theme(plot.title = element_text(size=16, color="#181818",hjust=0.5)) +
      theme(axis.title = element_text(color="#181818", face="bold", size=12))})
  
}

shinyApp(ui=ui, server=server)