#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(plotly)
library(highcharter)
library(purrr)
library(htmltools)

series.imdb <- read.csv("series_from_imdb.csv", stringsAsFactors = FALSE)

series.imdb <- series.imdb %>%
  mutate(series_name = factor(series_name)) %>%
  mutate(season = factor(season))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$plot <- renderUI({
  
    if (!is.null(input$series)){
      minimo <- min(series.imdb %>%
                      filter(series_name %in% input$series) %>%
                      filter(findInterval(season, input$seasons, rightmost.closed = TRUE) == 1L) %>%
                      select(UserRating))
      
      maximo <- max(series.imdb %>%
                      filter(series_name %in% input$series) %>%
                      filter(findInterval(season, input$seasons, rightmost.closed = TRUE) == 1L) %>%
                      select(UserRating))  
      map(input$series, function(x){
        series.imdb %>%
          filter(series_name == x) %>%
          filter(findInterval(season, input$seasons, rightmost.closed = TRUE) == 1L) %>%
          hchart("line", hcaes(x = series_ep, y = UserRating, group = season)) %>%
          hc_add_theme(hc_theme_smpl()) %>% 
          hc_title(text = x) %>%
          hc_xAxis(title = list(text = "Número do episódio"), tickInterval = 2) %>%
          hc_yAxis(title = list(text = "Nota do episódio"), tickInterval = .5, min = minimo, max = maximo) %>%
          hc_tooltip(pointFormat = "Nome: {point.Episode} <br> Nota: {point.y} <br> Número de Votos: {point.UserVotes}") %>%
          hc_legend(title = list(text = "Temporada"), align = "right", verticalAlign = "top",
                    layout = "vertical", x = 0, y = 50)
      }) %>% 
        hw_grid(ncol = 2)  %>% browsable()
    }
      
      #plot_ly(x = ~series_ep, y = ~UserRating, color = ~season,  type = 'scatter', mode = 'lines') %>%
      #subplot(nrows = 2,  shareX = TRUE)
    
  })
  
})
