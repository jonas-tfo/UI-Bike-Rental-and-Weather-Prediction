# this file contains the components required for the server end of the UI, which contains a world map, with several marked cities, a dropdown menu to select cities, and several graphics related to the weather for a given city, when said city is selected
# NOTE: Make sure "model_prediction.R" is present in directory.
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


install.packages("lubridate")
install.packages("plotly")
library(lubridate)
library(shiny)
library(leaflet)
library(tidyverse)
library(plotly)


source("model_prediction.R")        # make sure this file is present in directory, code will not run without


test_weather_data_generation <- function() {
  city_weather_bike_df <- generate_city_weather_bike_data()
  stopifnot(nrow(city_weather_bike_df) > 0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# sets up the shiny server

shinyServer(function(input, output) {

  city_list <- c("All", "Seoul", "Suzhou", "London", "New York", "Paris")
  

  city_weather_bike_df <- test_weather_data_generation()
  

  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))

  
  observeEvent(input$city_dropdown, {

# determines what to do if the option "All is selected in the dropdown menu"    
    
    if(input$city_dropdown == 'All') {
      output$city_bike_map <- renderLeaflet({
        # Create another data frame called `cities_max_bike` with each row containing city location info and max bike prediction for the city
        cities_max_bike <- city_weather_bike_df %>%
          group_by(CITY_ASCII) %>%
          select(CITY_ASCII, BIKE_PREDICTION_LEVEL, BIKE_PREDICTION, LNG, LAT, LABEL, DETAILED_LABEL, FORECASTDATETIME) %>%
          mutate(MAX_BIKE_PREDICTION = max(BIKE_PREDICTION))
        
        # Render leaflet map
        leaflet() %>%
          addTiles() %>%
          addCircleMarkers(
            data = cities_max_bike,
            lng = ~LNG,
            lat = ~LAT,
            radius = case_when(
              cities_max_bike$BIKE_PREDICTION_LEVEL == "small" ~ 6,
              cities_max_bike$BIKE_PREDICTION_LEVEL == "medium" ~ 10,
              cities_max_bike$BIKE_PREDICTION_LEVEL == "large" ~ 12
            ),
            fillColor = ~color_levels(BIKE_PREDICTION_LEVEL),  # Use colorFactor to assign fill color
            popup = ~DETAILED_LABEL
          )
      })
      
# determines what to do if an individual city is selected
      
    } else {
      output$city_bike_map <- renderLeaflet({
        cities_max_bike <- city_weather_bike_df[city_weather_bike_df$CITY_ASCII == input$city_dropdown,] %>%
          select(CITY_ASCII, BIKE_PREDICTION_LEVEL, BIKE_PREDICTION, LNG, LAT, LABEL, DETAILED_LABEL, FORECASTDATETIME) %>%
          mutate(MAX_BIKE_PREDICTION = max(BIKE_PREDICTION))
        leaflet() %>%
          addTiles() %>%
          addCircleMarkers(
            data = cities_max_bike,
            lng = ~LNG,
            lat = ~LAT,
            radius = case_when(
              cities_max_bike$BIKE_PREDICTION_LEVEL == "small" ~ 6,
              cities_max_bike$BIKE_PREDICTION_LEVEL == "medium" ~ 10,
              cities_max_bike$BIKE_PREDICTION_LEVEL == "large" ~ 12
            ),
            fillColor = ~color_levels(BIKE_PREDICTION_LEVEL),  
            popup = ~DETAILED_LABEL
          )
      })
      
      plot_data_specific <- city_weather_bike_df[city_weather_bike_df$CITY_ASCII == input$city_dropdown, ]
      datetime_var <- ymd_hms(plot_data_specific$FORECASTDATETIME)
      plot_data_specific$TIME_OF_DAY <- format(datetime_var, "%H:%M:%S")
      plot_data_specific <- plot_data_specific[order(plot_data_specific$TIME_OF_DAY),]
      plot_data_specific <- plot_data_specific[plot_data_specific$CITY_ASCII == input$city_dropdown, ]
      plot_data_specific <- plot_data_specific %>%
        group_by(TIME_OF_DAY) %>%
        summarize(AVG_TEMPERATURE = mean(TEMPERATURE),
                  AVG_BIKE_PREDICTION = mean(BIKE_PREDICTION))
      

      current_time <- Sys.time()
      hours_sequence <- seq(current_time, current_time + hours(7), by = "hour")
      hours_labels <- format(hours_sequence, "%H:%M:%S")
      
      # FIRST PLOT
      
      output$temp_line <- renderPlot({
        ggplot(plot_data_specific, aes(x = TIME_OF_DAY, y = AVG_TEMPERATURE, group = 1)) +
          geom_line(color = "yellow") +
          geom_point() +
          geom_text(aes(label = AVG_TEMPERATURE)) +
          scale_x_discrete(labels = hours_labels) +  
          labs(x = "Time (7 hours ahead)",
               y = "Temperature (°C)")
      }) 
      
      city_weather_bike_df <- city_weather_bike_df %>%
        group_by(FORECASTDATETIME) %>%
        mutate(AVG_BIKE_PREDICTION = mean(BIKE_PREDICTION))
      
      current_time <- Sys.time() 
      future_hours <- current_time + hours(3)
      
      # SECOND PLOT (interactive)
      
      output$bike_line <- renderPlot({
        ggplot(data = plot_data_specific, aes(x = TIME_OF_DAY, y = AVG_BIKE_PREDICTION, group = 1)) +
          geom_line(color = "darkgreen", linetype = 3) +
          geom_point() +
          scale_x_discrete(labels = hours_labels) +
          labs(x = "Time of day",
               y = "Bike prediction")
      }) 

      # codes for the interactive aspect of the second plot 
      
      output$bike_date_output <- renderText({
        event <- input$plot_click
        if (!is.null(event)) {
          clicked_x <- input$plot_click$x
          clicked_y <- input$plot_click$y
          paste("Date and time:", clicked_x + current_time, "\nBike prediction:", clicked_y)
        } else {
          "Click on a point to see details."
        }
      })

     # THIRD PLOT 
      
     output$humidity_pred_chart <- renderPlot({
       ggplot(data = city_weather_bike_df, aes(x = HUMIDITY, y = BIKE_PREDICTION)) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ poly(x, 4))
       
     })
    }
  })
})

  


