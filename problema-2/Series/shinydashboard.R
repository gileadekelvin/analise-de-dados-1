library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(highcharter)
library(purrr)
library(htmltools)

series.imdb <- read.csv("series_from_imdb.csv", stringsAsFactors = FALSE)

series.imdb <- series.imdb %>%
  mutate(series_name = factor(series_name)) %>%
  mutate(season = factor(season))

ui <- dashboardPage(
  dashboardHeader(title = "Minha série é a melhor!", titleWidth = 250),
  dashboardSidebar(width = 250, sidebarMenu( 
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    sliderInput("size", "Altura da visualizações", min = 200, max = 700, value = 300),
    menuItem("Info", tabName = "info", icon = icon("info")), 
    menuItem("Comments", tabName = "disqus_here", icon = icon("comments"))
    
  )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard", class = "active",
        fluidPage(
          # Application title
          # titlePanel("Minha série é a melhor!"),
          
          box(width = 4,
            title = "Entradas", status = "warning",
            selectInput("series", "Série(s):",
                        levels(series.imdb$series_name),
                        multiple = TRUE
            ),
            sliderInput("seasons", "Temporadas",
                        min = 1, max = 23, value = c(1,2))
            
          ),
          box(width = 8,
            title = "Comparação", status = "primary",
            htmlOutput("plot")
            
          )
          
          
        )
      ),
      
      tabItem(tabName = "info",
              box(
                title = "Dados", status = "primary",
                "A fonte dos dados é o IMDB, um banco de dados online com informação sobre música, filmes, séries, cinema, jogos, programas e comerciais de TV, atualmente percente a Amazon. ", br()
              ),
              box(
                title = "Sobre", status = "warning",
                "Essa aplicação foi desenvolvida por Gileade Kelvin durante a disciplina de Análise de Dados 1 - UFCG.", br())
      ), 
      
      tabItem(tabName = "disqus_here",
              div(id="disqus_thread",
                  HTML(
                    "<script>
    (function() {  
        var d = document, s = d.createElement('script');
        s.src = 'https://minha-serie-e-a-melhor.disqus.com/embed.js';
        s.setAttribute('data-timestamp', +new Date());
        (d.head || d.body).appendChild(s);
    })();
</script>
<noscript>Please enable JavaScript to view the
<a href='https://disqus.com/?ref_noscript' rel='nofollow'>comments powered by Disqus.</a>
</noscript>"
                  )
              )
      )
    ),
    div(HTML('<script id="dsq-count-scr" src="//minha-serie-e-a-melhor.disqus.com/count.js" async></script>'))
      
      
    )
  )

server <- function(input, output) { 
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
        hw_grid(rowheight = input$size, ncol = 2)  %>% browsable()
    }
    
    
    
  })
  
  }


shinyApp(ui, server)