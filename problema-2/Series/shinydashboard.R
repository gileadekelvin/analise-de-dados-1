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
    menuItem("Dicas", tabName = "tips", icon = icon("question")), 
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
                        multiple = TRUE,
                        selected = c("Breaking Bad", "Game of Thrones")
            ),
            sliderInput("seasons", "Temporadas",
                        min = 1, max = 23, value = c(1,2))
            
          ),
          box(width = 8,
            title = "Comparação", status = "primary",
            htmlOutput("plot")
            
          ),
          
          box(width = 4,
            title = "Minha série é a melhor!", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            "Você já teve alguma discussão com um amigo(a) seu sobre como sua série favorita é melhor que a dele?", br(), 
            "Agora você pode comparar as séries e provar que a sua é realmente melhor.", br(),
            "Você pode comparar várias séries e temporadas para mais informações acesse info."
          ),
          
          box(width = 4,
              title = "Análise", status = "primary", solidHeader = TRUE,
              collapsible = TRUE,
              "Se escolhermos as duas primeiras temporadas de Breaking Bad (BB) e Game of Thrones (GOT) para comparar, podemos perceber que a maioria dos episódios de GOT tem avaliação melhor que os de BB.", br(), 
              "Ao compararmos o formato das linhas podemos notar que GOT tem um crescimento maior nas notas dos episódios conforme as temporadas avançam.", br(),
              "Os penúltimos episódios, com exceção da segunda temporada de BB, são melhores que os últimos espisódios de suas respectivas temporadas.", br(),
              "Quer saber se isso acontece em outras temporadas também? Adicione mais temporadas a visualização e confira!"
          )
          
          #end fluidpage
        )
      ),
      
      tabItem(tabName = "tips",
              column(width = 12,
                     valueBox(width = 4,
                              tagList("mais de 500 séries", tags$sup(style="font-size: 20px")),
                              "É possível pesquisar pelo nome (em inglês).", icon = icon("line-chart"), color = "blue"
                     ),
                     valueBox(width = 4,
                              tagList("Altura", tags$sup(style="font-size: 20px")),
                              "Ajuste a altura dos gráficos pelo controle no menu sidebar.", icon = icon("sliders"), color = "orange"
                     ),
                     valueBox(width = 4,
                              tagList("Temporadas", tags$sup(style="font-size: 20px")),
                              "Escolha qual o intervalo de temporadas no controle em Entradas.", icon = icon("sliders"), color = "blue"
                     )
              )
      ),
      
      tabItem(tabName = "info",
              box(width = 4,
                title = "Dados", status = "primary",
                "A fonte dos dados é o IMDB, um banco de dados online com informação sobre música, filmes, séries, cinema, jogos, programas e comerciais de TV, atualmente percente a Amazon. ", br(),
                "Os dados foram obtidos aqui: github.com/nazareno/imdb-series"
              ),
              box(width = 4,
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