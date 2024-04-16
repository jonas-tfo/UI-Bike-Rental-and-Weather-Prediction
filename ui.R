# this file contains the frontend code for the UI which is displayed to the user when the ui and server scripts are run

# loads leaflet
require(leaflet)

# creates shiny ui page
shinyUI(fluidPage(
  padding = 5,
  titlePanel("Bike-sharing demand prediction app"),

# determines where the plots (created in server.R) are displayed on the UI and what attributes they have
  
  sidebarLayout(
    mainPanel(id = 'city_bike_map',
              height = 1500,
              leafletOutput('city_bike_map')),
    sidebarPanel(
      # select drop down list to select city
      selectInput(inputId = "city_dropdown", "Select City:", 
                  choices = c("All", "Seoul", "Suzhou", "London", "New York", "Paris")),
      plotOutput("temp_line", height = 200),
      plotOutput("bike_line",
                 click = "plot_click", height = 200),
      verbatimTextOutput("bike_date_output"),
      plotOutput("humidity_pred_chart", height = 200)
      
    )
  )
))
