#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(ggplot2)
library(DT)

# Define UI for application that draws a histogram
#startData <- read_csv("https://media.githubusercontent.com/media/ChristianUlrichRoth/rassignment/master/MajorAssignment/data/cleandata/flights15_complete_sample.csv")
startData <- read_csv("flights15_complete_sample.csv")

GBChoices <- as.list(names(startData))
names(GBChoices) <- paste(names(startData),map(startData,~length(unique(.x))))

ui <- fluidPage(
  
  # Application title
  titlePanel("Flights Observations"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      #----------------------------------Input------------------------------------------------------------
      useShinyjs(),

      
      
      pickerInput(inputId = "airportInput",
                  label = "Airport",
                  choices = unique(startData$ORIGIN_AIRPORT),
                  selected = unique(startData$ORIGIN_AIRPORT),
                  multiple = T,
                  options = list(`actions-box` = TRUE,
                                 `selected-text-format`= "count",
                                 `count-selected-text` = "{0} items selected ({1} total)"
                  )
      ),
      
      pickerInput(inputId = "airlineInput",
                  label = "Airline",
                  choices = unique(startData$AIRLINE.y),
                  selected = unique(startData$AIRLINE.y),
                  multiple = T,
                  options = list(`actions-box` = TRUE,
                                 `selected-text-format`= "count"
                                 #`count-selected-text` = "TOTAL",
                                 #`selected-text-format` = length(unique(startData$ORIGIN_AIRPORT)),
                                 #`count-selected-text` = "TOTAL"
                  )
      ),
      selectInput(inputId = "GB",
                  label = "Group By",
                  choices = GBChoices,
                  selected="AIRLINE"),
      
      selectInput(inputId = "Metric",
                  label = "Metric",
                  choices = c("Total Flights", "Number of delayed Flights", names(select(startData, "DEPARTURE_DELAY", "ARRIVAL_DELAY", "DISTANCE")))),
      
      #Button Show or Hide Data Table
      actionButton(inputId = "button", label = "Data Table (show/hide)"),
      
    
    ),
    
    # Show a plot of the generated distribution
    mainPanel(


      
      # shinyjs::hidden 
      # (
      #   DTOutput('tbl')      ),
     
      plotOutput("plot1"),
      DT::dataTableOutput("tbl"),
      #DTOutput('tbl')
      #DT::dataTableOutput("results", width = 300)
      # hidden(
      #   p(id = "tbl", dataTableOutput)
      # ),
      
    )
  )
)

#-------------------------------------------------------------------------------------------------SERVER PART-----------------------------------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it d epends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  
  ## observe the button being pressed
  observeEvent(input$button, {
    shinyjs::toggle("tbl")
  })
  
  ## observe the button being pressed
  # observeEvent(input$button, {
  #   
  #   if(input$button %% 2 == 1){
  #     shinyjs::hide(id = "tbl")
  #   }else{
  #     shinyjs::show(id = "tbl")
  #   }
  # })
  

   ###TOTAL Flights delayed
  #!! is a one-to-one replacement. !!! (called “unquote-splice”, and pronounced bang-bang-bang) is a one-to-many replacement. It takes a list of expressions and inserts them at the location of the !!!:
   updateData <- reactive(
     if(input$Metric == "Number of delayed Flights"){
       startData %>%
         filter(ARRIVAL_DELAY > 0) %>% 
         group_by(!!! rlang::syms(input$GB)) %>% 
         select(AIRLINE, FLIGHT_NUMBER, TAIL_NUMBER, ORIGIN_AIRPORT, DESTINATION_AIRPORT,  SCHEDULED_DEPARTURE, DEPARTURE_TIME, DEPARTURE_DELAY, SCHEDULED_TIME, ELAPSED_TIME, AIR_TIME, DISTANCE, SCHEDULED_ARRIVAL, ARRIVAL_TIME, ARRIVAL_DELAY) %>%
         summarise("Number of delayed Flights" =n()) #TODO hier fehlt noch was
     }
     else if(input$Metric == "Total Flights"){
        startData %>%
             filter(ORIGIN_AIRPORT %in% input$airportInput, AIRLINE.y %in% input$airlineInput) %>%
             group_by(!!! rlang::syms(input$GB)) %>%
             select(AIRLINE, FLIGHT_NUMBER, TAIL_NUMBER, ORIGIN_AIRPORT, DESTINATION_AIRPORT,  SCHEDULED_DEPARTURE, DEPARTURE_TIME, DEPARTURE_DELAY, SCHEDULED_TIME, ELAPSED_TIME, AIR_TIME, DISTANCE, SCHEDULED_ARRIVAL, ARRIVAL_TIME, ARRIVAL_DELAY) %>%
             summarise("Total Flights" =n())
     }
     else {
         startData %>%
           filter(ORIGIN_AIRPORT %in% input$airportInput, AIRLINE.y %in% input$airlineInput) %>%
           group_by(!!! rlang::syms(input$GB)) %>%
           select(AIRLINE, FLIGHT_NUMBER, TAIL_NUMBER, ORIGIN_AIRPORT, DESTINATION_AIRPORT,  SCHEDULED_DEPARTURE, DEPARTURE_TIME, DEPARTURE_DELAY, SCHEDULED_TIME, ELAPSED_TIME, AIR_TIME, DISTANCE, SCHEDULED_ARRIVAL, ARRIVAL_TIME, ARRIVAL_DELAY) %>%
           summarise_if(is.numeric, sum,na.rm=T)
     }

)

   
   output$plot1 <- renderPlot({
    updateData() %>% 
      ggplot(aes(x=!! rlang::sym(input$GB),y=!! rlang::sym(input$Metric),fill=!! rlang::sym(input$GB))) +
      geom_col()
  }) 
  
  
  output$tbl <- renderDT(
    updateData(), rownames = F, extensions = 'Buttons', filter="top", editable=F, 
    options = list(
      dom = 'Blfrtip',
      buttons = c( I('colvis'), 'copy', 'csv', 'excel', 'pdf', 'print'),
      lengthMenu = list(c(10,50,100,-1),c(10,50,100,"All"))
    )
  )
  
  #Automatically stop a Shiny app when closing the browser tab
  #session$onSessionEnded(stopApp)
  
  
}

shinyApp(ui, server)
