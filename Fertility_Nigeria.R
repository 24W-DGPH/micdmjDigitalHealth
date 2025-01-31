library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(rsconnect)

# my_shiny_app online connect
#rsconnect::deployApp('C:\Users\micdm\Documents\GPH\Digital Health\myShinyApp')

# Loading my_dataset in _excel
data <- read_excel("Fertility_Nigeria.xlsx", sheet = "Fertility_Nigeria")

# cleaning my_dataset
data <- data %>%
  filter(!is.na(Location) & !is.na('sample size')) %>% #removing missing and NA values
  distinct() %>% #remove duplicates
  mutate(Location = trimws(Location)) %>% #to remove white spaces in location names
  filter('sample size' > 0) #remove invalid sample size

# setting parameters for my_shiny_app GUI
# UI
iu <- fluidPage(
  titlePanel("Fertility Rates in Nigeria by Location"),
  sidebarLayout(
    sidebarPanel(
      selectInput("location", "Select State: Multiple Selection Possible", choices = unique(data$Location), multiple = TRUE)
    ),
    mainPanel(
      plotOutput("fertilityPlot")
    )
  )
)
# the plot input for my_shiny_app
# Server
server <- function(input, output) {
  output$fertilityPlot <- renderPlot({
    filtered_data <- subset(data, Location %in% input$location) 
    
    ggplot(filtered_data, aes(x = Location, y = `Sample Size`, color = Location, group = Location)) +
      geom_point(size = 4) +
      geom_smooth(method = "loess", se = FALSE, aes(color = Location)) +
      scale_color_brewer(palette = "Dark2") +
      labs(title = "Fertility Sample Size by Location", x = "Location", y = "Sample Size") +
      theme_minimal()
  })
}

# Run App
shinyApp(ui = iu, server = server)
