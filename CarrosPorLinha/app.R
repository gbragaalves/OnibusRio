############ CARROS POR LINHA ############


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(lubridate)

BusPorLinha <- read.csv2("./MediasLinhas.csv",stringsAsFactors = FALSE)
ListaLinhas <- read.csv2("./ListaLinhas.csv",stringsAsFactors = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(width = 2,
            selectInput("select", 
                        label = "Selecione uma Linha:", 
                        choices = ListaLinhas$linha, 
                        selected = "397"),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(width = 10, 
                  tabsetPanel( 
                      tabPanel(title = 'Grafico', 
                               plotlyOutput(outputId="distPlot")))
                  
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- 
        renderPlotly({
            
            valor <- paste0("L",input$select)
            
            BusPorLinha[[valor]]<-as.numeric(BusPorLinha[[valor]])
            
            BusPorLinha$DataCompleta <- as.POSIXct(BusPorLinha$DataCompleta, format="%Y-%m-%d %H:%M:%S")
            max_Date <- ymd_hms(tail(BusPorLinha$DataCompleta, n=1),tz="America/Sao_Paulo")
            max_Date_ms <- interval("1970-01-01 00:00:00", max_Date) / dmilliseconds(1)
            min_Date_ms <- max_Date_ms - 604800000
            
            fig <- plot_ly(BusPorLinha, x = ~DataCompleta, y = BusPorLinha[[valor]], type = "scatter", mode = 'lines')
            fig <- fig %>% layout(title = 'Quantidade de Onibus em Circulacao no Rio de Janeiro',
                                  xaxis = list(zeroline = TRUE, title = 'Dia / Hora', range = c(min_Date_ms, max_Date_ms)),
                                  yaxis = list(title = 'Quantidade'))
            fig
            
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
