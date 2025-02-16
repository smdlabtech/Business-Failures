

library(shiny)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)

ui <- fluidPage(
  titlePanel("Défaillances d'entreprises en Normandie (Hébergement-Restauration)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choisir un fichier CSV", accept = ".csv"),
      numericInput("startYear", "Année de début", 1990, min = 1900, max = 2100),
      numericInput("forecastPeriod", "Prévisions (trimestres)", 10, min = 1, max = 50),
      actionButton("analyze", "Analyser")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Aperçu des données", tableOutput("dataPreview")),
        tabPanel("Série Temporelle", plotOutput("timeSeriesPlot")),
        tabPanel("Saisonnalité", plotOutput("seasonalityPlot")),
        tabPanel("Corrélation", plotOutput("acfPlot")),
        tabPanel("Prévisions", plotOutput("forecastPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    df <- read.csv2(input$file$datapath, sep = ";", stringsAsFactors = FALSE)
    colnames(df) <- c("date", "NbreObs", "code")
    df <- df[complete.cases(df), ]
    df$NbreObs <- as.numeric(df$NbreObs)
    df
  })

  output$dataPreview <- renderTable({
    head(data(), 10)
  })

  tsData <- reactive({
    df <- data()
    ts(df$NbreObs, start = c(input$startYear, 1), frequency = 4)
  })

  output$timeSeriesPlot <- renderPlot({
    ts_series <- tsData()
    plot(ts_series, col = "brown", ylab = "Nombre de défaillances", main = "Série Temporelle")
  })

  output$seasonalityPlot <- renderPlot({
    ts_decomp <- decompose(tsData(), type = "multiplicative")
    plot(ts_decomp, main = "Décomposition de la Série Temporelle")
  })

  output$acfPlot <- renderPlot({
    acf(tsData(), main = "Autocorrélation de la Série")
  })

  output$forecastPlot <- renderPlot({
    hw_model <- ets(tsData(), model = "MMM")
    hw_pred <- forecast(hw_model, h = input$forecastPeriod)
    plot(hw_pred, main = "Prévision Holt-Winters")
  })
}

shinyApp(ui = ui, server = server)
