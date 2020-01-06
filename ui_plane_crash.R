library(shiny)
library(shinyWidgets)
renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(6,
             pickerInput(inputId = paste0(prefix, "_", "country"),label = "Country",choices = unique(plane_accident_data2$Country), options = list(`actions-box` = TRUE),multiple = T),
             #selectInput(inputId = paste0(prefix, "_", "country"),label = "Country",choices = c("All",unique(plane_accident_data2$Country)),selected = "All", multiple = TRUE),
             pickerInput(inputId = paste0(prefix, "_", "location"),label = "Location",choices = unique(plane_accident_data2$Location), options = list(`actions-box` = TRUE), multiple = T),
             pickerInput(inputId = paste0(prefix, "_", "flight_purpose"),label = "Flight Purpose",choices = unique(plane_accident_data2$Purpose.of.Flight), options = list(`actions-box` = TRUE), multiple = T)
             
      ),
      column(6,
             
             pickerInput(inputId = paste0(prefix, "_", "air_category"),label = "Aircraft Category",choices = unique(plane_accident_data2$Aircraft.Category)), options = list(`actions-box` = TRUE), multiple = T),
      pickerInput(inputId = paste0(prefix, "_", "air_engine_n"),label = "Aircraft Engines#",choices = unique(plane_accident_data2$Number.of.Engines)), options = list(`actions-box` = TRUE), multiple = T),
    pickerInput(inputId = paste0(prefix, "_", "air_engine_type"),label = "Aircraft Engine Type",choices = unique(plane_accident_data2$Engine.Type)), options = list(`actions-box` = TRUE), multiple = T)
      )
    ),
    sliderInput(inputId = paste0(prefix, "_", "year_range"),label = "Time Period (Years):", min = 1948, max = 2019, value = c(1948,2018))
  )
}

# Define UI for application that plots random distributions
ui<- fluidPage(theme="simplex.min.css",
          tags$style(type="text/css",
                     "label {font-size: 12px;}",
                     ".recalculating {opacity: 1.0;}"
          ),
          
          # Application title
          tags$h2("Aircraft Accidents"),
          p("The following graphical dashboard answers the three major questions, with added liberty to user to compare different scenerios side-by-side"),
          hr(),
          
          fluidRow(
            column(6, tags$h3("Scenario A")),
            column(6, tags$h3("Scenario B"))
          ),
          fluidRow(
            column(6, renderInputs("a")),
            column(6, renderInputs("b"))
          ),
          fluidRow(
            column(6,
                   plotOutput("a_distPlot1", height = "300px")
            ),
            column(6,
                   plotOutput("b_distPlot1", height = "300px")
            )
          ),
          fluidRow(
            column(6,
                   plotOutput("a_distPlot2", height = "300px")
            ),
            column(6,
                   plotOutput("b_distPlot2", height = "300px")
            )
          ),
          fluidRow(
            column(6,
                   plotOutput("a_distPlot3", height = "300px")
            ),
            column(6,
                   plotOutput("b_distPlot3", height = "300px")
            )
          )
)


shinyApp(ui=ui, server=server)



sum(as.numeric(JuneData1$Account.Balance), na.rm = TRUE)

#selectInput(inputId = "FA",label = "Fatal Accident*",choices = c(" ",unique(plane_accident_data2$Fatality)),selected = " ")