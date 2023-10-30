#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)


accident <- read.csv("accident.csv")
accident$COUNTYNAME <- gsub("\\s*\\(\\d+\\)", "", accident$COUNTYNAME)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Car Accident Location"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("State", "Select State", unique(accident$STATENAME)),
            selectInput("County", "Select County", "All"),
            br(),
            tableOutput("accidentCountTable"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("map",width = "100%", height = "600px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observe({
    if (input$State != "All") {
      counties <- unique(accident$COUNTYNAME[accident$STATENAME == input$State])
      updateSelectInput(session, "County", choices = c("All", counties))
  } else {
      updateSelectInput(session, "County", choices = "All")
    }
  })
  

  filtered_accident <- reactive({
    filtered <- accident
    
    if (input$State != "All") {
      filtered <- filter(filtered, STATENAME == input$State)
    }

    if (input$County != "All") {
      filtered <- filter(filtered, COUNTYNAME == input$County)
    }
    

    filtered
    
  })
  
  
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = -95.7129, lat = 37.0902, zoom = 3.5) %>%
        addMarkers(data = filtered_accident(),
                   lat = ~LATITUDE,
                   lng = ~LONGITUD,
                   popup = ~paste("Accident ID:", ST_CASE, 
                                  "<br>", "Description:", HARM_EVNAME,
                                  "<br>", "Weather:", WEATHERNAME))
    })
    
    output$accidentCountTable <- renderTable({
      data <- filtered_accident()
      if (input$State != "All") {
        state_accidents <- data %>%
          group_by("State" = STATENAME) %>%
          summarise("Number of Accidents" = n())
        state_accidents
      } else {
        NULL
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
