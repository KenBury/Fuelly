#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
library(xml2)
library(rvest)
library(jsonlite)
library(lubridate)
library(zoo)
library(ggplot2)
library(plotly)



fuelly <- html_session("https://www.fuelly.com/login")
login <- fuelly %>% html_node ("form") %>%
  html_form() %>%
  set_values(
    email = "kenbury1@gmail.com",
    password = "rmeetup"
  )
logged_in <- fuelly %>% submit_form(login)


cars_url <- html_nodes(logged_in,".dashboard-vehicle") %>% html_attr("data-clickable") 
cars_name <- html_nodes(logged_in,"h3.dashboard-vehicle-name") %>% html_text()

car_no <- 1


ui <- dashboardPage(
  dashboardHeader(title = "Fuelly Analysis"),
  dashboardSidebar(
    radioButtons("carradioButton", "Car selection",
                 cars_name),
    sliderInput("slider", "Moving average periods",min = 3, max = 10, value = 3, step = 1)
    ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotlyOutput("plot1"))
      
      #box(
       # title = "Controls",
        #sliderInput("slider", "Number of observations:", 1, 100, 50)
      #)
    )
  )
)

server <- function(input, output) {
  

 
  
  # select from cars_name to pick the car page to go to
  
  car_data_import <- reactive( {
    
    car_no <- which(cars_name == input$carradioButton)
    print(car_no)
   
  
  #car_page <- logged_in %>% jump_to(cars_url[2])
  
  #car_export <- logged_in %>% jump_to(paste0(cars_url[2],"/export/fuelups"))
  # this is nice but I seem to loose control after export button
  
  car_chart <- logged_in %>% jump_to(paste0(cars_url[car_no],"/fuelchart"))
  
  car_chart_data <- html_node(car_chart,"div#highchart-alltime-fuelups") %>% html_attr("data-fuelupdata")
  
  df_car_data <- fromJSON(car_chart_data)
  
  df_car_data <- df_car_data$series
  

  
  
  df_car_data$fuelup_date <- df_car_data$fuelup_date <- ymd_hms(df_car_data$fuelup_date,tz= "EST")
  df_car_data$mpg <- df_car_data$mpg <- as.numeric(df_car_data$mpg)
  df_car_data
  
  })
  
  
  
  
 
  
  

  
  
  
  output$plot1 <-  renderPlotly({
    df_car_data <- car_data_import()
    df_car_data$ma <- rollapply(df_car_data$mpg, input$slider, mean, fill = NA, align = "right")
    df_car_data$sd <- rollapply(df_car_data$mpg, input$slider, sd, fill = NA, align = "right")
    ggplot(df_car_data, aes(x = fuelup_date, y = mpg)) + geom_ribbon(aes(ymin = ma - (2*sd), ymax = ma + (2* sd)), alpha = 0.25) + geom_point() + geom_line(aes(y = ma), color = "blue")
      
  })
}

shinyApp(ui, server)



