library(shiny)
library(wesanderson)
library(shinyWidgets)
library(tidyverse)
library(shinythemes)
library(thematic)
library(bslib)
library(rsconnect)

threeColors = wes_palette("Darjeeling1", 3, type = "discrete")
oneColor    = wes_palette("Darjeeling1", 1, type = "discrete")

df_model = read.csv('Data/CPL_Ready_For_Model.csv')
df = read.csv('Data/CPL_Cleaned_Data.csv')

locationID = df$Location
year       = df$Year

df = df %>%
  select(-X, -Location, -Year) %>%
  sapply(., as.numeric) %>% as.data.frame(.) %>%
  mutate(Location = locationID,
         Year     = year) %>%
  select(Location, Year, everything())

variables = colnames(df)[-c(1:3)]
filter = dplyr::filter
thematic_shiny(font = "auto")


ui <- fluidPage(
  titlePanel('California Public Libraries Statistics'),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabselected == 'Variable Counts'",
        pickerInput("location", 
                    "Select Library/Library System", 
                    choices = unique(df$Location), 
                    options = list(
                      `live-search` = TRUE)
        ),
        actionButton("submit", "Submit"),
        pickerInput("variable",
                    "Variable",
                    choices = variables,
                    options = list(
                      `live-search` = TRUE)
        )
      ),
      conditionalPanel(
        condition = "input.tabselected == 'Variable Relationships'",
        pickerInput("var1",
                    "Variable 1",
                    choices = variables,
                    options = list(
                      `live-search` = TRUE),
        ),
        pickerInput("var2",
                    "Variable 2",
                    choices = variables,
                    options = list(
                      `live-search` = TRUE)
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabselected",
        tabPanel("Variable Counts", value = "Variable Counts",
                 plotly::plotlyOutput("Plot")),
        tabPanel("Variable Relationships", value = "Variable Relationships",
                 plotly::plotlyOutput("Relationships"))
      )
      
    )
  )
)


server <- function(input, output) {
  
  data = eventReactive(input$submit, {
    df %>%
      filter(Location == input$location)
  })
  
  output$Plot = plotly::renderPlotly({
    data = data()
    data %>%
      ggplot(aes_string(x='Year',y=as.name(input$variable))) +
      geom_col(fill=oneColor) +   
      labs(title = input$variable,
           x = "Year") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  output$Relationships = plotly::renderPlotly({
    df %>%
      ggplot() +
      geom_point(aes_string(x=input$var1, y=input$var2),position="jitter") +
      labs(title = "Variable Relationships",
           x = input$var1, y = input$var2) +
      theme_minimal()
  })
}


shinyApp(ui = ui, server = server)
