#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(highcharter)
library(plotly)

series.imdb <- read.csv("series_from_imdb.csv", stringsAsFactors = FALSE)

series.imdb <- series.imdb %>%
  mutate(series_name = factor(series_name)) %>%
  mutate(season = factor(season))

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
     selectInput("series", "Série(s):",
                 levels(series.imdb$series_name),
                 multiple = TRUE
       ),
     sliderInput("seasons", "Temporadas",
                 min = 1, max = 23, value = c(1,2))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput("plot")
    )
  )
))
