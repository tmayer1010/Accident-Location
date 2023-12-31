################################################################
# Shiny app by Tyson Mayer
# Oct 30, 2023
#
# Shows the location of fatal accidents in 2021 in the US
#
# Deployed at https://tysonmayer.shinyapps.io/Shiny_1/
# Source code at GitHub: https://github.com/tmayer1010/Accident-Location.git
################################################################

library(shiny)
library(leaflet)
library(dplyr)


accident <- read.csv("accident.csv")
accident$COUNTYNAME <- gsub("\\s*\\(\\d+\\)", "", accident$COUNTYNAME)

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Car Accident Location"),

    # selecting state and county 
    sidebarLayout(
        sidebarPanel(
            selectInput("State", "Select State", unique(accident$STATENAME)),
            selectInput("County", "Select County", "All"),
            br(),
            tableOutput("accidentCountTable"),
        ),

        # Show map
        mainPanel(
           leafletOutput("map",width = "100%", height = "600px")
        )
    )
)

# Define server 
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
