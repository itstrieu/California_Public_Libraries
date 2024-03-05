
library(shiny)
library(wesanderson)
library(shinyWidgets)
library(tidyverse)

threeColors = wes_palette("Darjeeling1", 3, type = "discrete")
oneColor    = wes_palette("Darjeeling1", 1, type = "discrete")

df_model = read.csv('California_Public_Libraries/Data/CPL_Ready_For_Model.csv')
df = read.csv('California_Public_Libraries/Data/CPL_Cleaned_Data.csv')
variables = colnames(df)[-c(1:3)]
filter = dplyr::filter

ui <- fluidPage(
  
    titlePanel('California Public Libraries Statistics'),

    sidebarLayout(
        sidebarPanel(
          pickerInput("location", 
                      "Select Library/Library System", 
                      choices = unique(df$Location), 
                      options = list(
                        `live-search` = TRUE)
                      ),
          pickerInput("variable",
                      "Variable",
                      choices = variables,
                      options = list(
                        `live-search` = TRUE)
                      ),
          submitButton("submit")
        ),

        mainPanel(
          plotOutput("Plot")
        )
    )
)

server <- function(input, output) {

  data = eventReactive({
    df %>% 
      filter(Location == input$location) %>%
      mutate(y = input$variable)
  })
  
  output$Plot = renderPlot({
    
    data() %>%
      ggplot() +
      geom_col(aes(x=Year, y=y, fill=oneColor)) +   
      labs(title = input$variable,
           x = "Year") +
      theme_minimal() +
      theme(legend.position = "none")
  })
}


shinyApp(ui = ui, server = server)
