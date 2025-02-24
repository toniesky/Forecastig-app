suppressPackageStartupMessages({
  library(shiny)
  library(readxl)
  library(ggplot2)
  library(TTR)       # Para SMA y WMA
  library(tseries)   # Para adf.test
  library(shinyjs)
  library(plotly)
  library(dplyr)     # Para manipulación de datos
  library(stats)     # Para funciones estadísticas básicas
})

# Obtener el nombre de usuario del sistema
username <- Sys.info()[["user"]]

options(shiny.launch.browser = function(url) {
  # browseURL(url)  # Abre la URL en el navegador predeterminado
})

ui <- fluidPage(
  useShinyjs(),  # Habilitar shinyjs
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f0f8ff; 
        color: #333; 
        font-family: Arial, sans-serif;
      }
      .title-panel {
        background-color: #1e90ff; 
        color: white;
        padding: 20px;
        text-align: center;
        border-radius: 10px;
        margin-bottom: 20px;
      }
      .sidebar {
        background-color: #e6f2ff; 
        padding: 15px;
        border-radius: 10px;
      }
      .main-panel {
        background-color: #ffffff;
        padding: 20px;
        border-radius: 10px;
      }
      .footer {
        text-align: center;
        margin-top: 30px;
        font-size: 14px;
        color: #7f8c8d;
      }
      a {
        color: #1e90ff;
        text-decoration: none;
      }
      a:hover {
        text-decoration: underline;
      }
      /* Separación entre botones */
      .action-buttons .btn {
        margin-bottom: 10px;
        width: 100%;
      }
      /* Estilos para la tabla de métricas */
      .table-container {
        display: flex;
        justify-content: center;
      }
      .table-container table {
        width: 80%;
        border-collapse: collapse;
      }
      th, td {
        padding: 8px 12px;
        border: 1px solid #ddd;
        text-align: center;
      }
      th {
        background-color: #f2f2f2;
      }
    "))
  ),
  
  div(class = "title-panel", 
      titlePanel("Modelos de Promedio Móvil Ponderado")
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      fileInput("file", "Seleccionar archivo Excel", accept = c(".xlsx")),
      uiOutput("sheet_ui"),
      uiOutput("time_col_ui"),
      uiOutput("value_col_ui"),
      textInput("n", "Ingrese el valor de n para el promedio móvil", value = "5"),
      textInput("pesos", "Ingrese los pesos para el promedio móvil ponderado (separados por +)", value = "0.6+0.2+0.1+0.05+0.05"),
      hr(),
      div(class = "action-buttons",
          actionButton("run_model", "Ejecutar Modelo", icon = icon("play"), class = "btn-primary")
      ),
      hr(),
      p("by: ", a("Toñesky en YouTube", href = "https://www.youtube.com/@Tonyesky", target = "_blank")),
      p("Desarrollado en 2024")
    ),
    
    mainPanel(
      class = "main-panel",
      tabsetPanel(
        id = "tabset",
        tabPanel(
          title = tagList(icon("chart-line"), "Resultados"),
          value = "resultados",
          fluidRow(
            column(12, 
                   h3("IP y Usuario"),
                   verbatimTextOutput("ip_user_info"),
                   hr(),
                   h3("Test de Dickey-Fuller"),
                   verbatimTextOutput("adf_test")
            )
          ),
          hr(),
          fluidRow(
            column(12, 
                   h3("Pronóstico"),
                   verbatimTextOutput("forecast")
            )
          ),
          hr(),
          fluidRow(
            column(12, 
                   h3("Gráfico del Modelo"),
                   plotlyOutput("plot", height = "400px")
            )
          ),
          hr(),
          fluidRow(
            column(12, 
                   h3("Errores de Pronóstico"),
                   div(class = "table-container",
                       tableOutput("error_report")
                   )
            )
          )
        )
      ),
      div(class = "footer",
          HTML("&copy; 2024 - by <a href='https://www.youtube.com/@Tonyesky' target='_blank'>Toñesky en YouTube</a>")
      )
    )
  )
)

server <- function(input, output, session) {
  # Obtener la dirección IP del usuario
  ip_address <- reactive({
    session$request$REMOTE_ADDR
  })
  
  # Función para calcular los errores
  errores <- function(Pronosticado, Real) { 
    Error = Real - Pronosticado
    ErrorAbsoluto = abs(Error)
    ErrorCuadrado = ErrorAbsoluto^2 
    ProporcionError = ErrorAbsoluto / Real
    Bias <- mean(Error, na.rm = TRUE)
    
    MAD <- mean(ErrorAbsoluto, na.rm = TRUE)
    MAPE <- mean(ProporcionError, na.rm = TRUE)
    MSE <- mean(ErrorCuadrado, na.rm = TRUE)
    RMSE <- sqrt(MSE)  
    
    TipoErrores <- c("Bias (Sesgo)", "MAPE", "MAD", "MSE", "RMSE")
    Errores <- c(round(Bias, 2), paste(round(MAPE * 100, 2), "%"), round(MAD, 2), round(MSE, 2), round(RMSE, 2))
    
    dfMedidasError <- data.frame(
      "Error" = TipoErrores,
      "Valor" = Errores
    )
    
    return(dfMedidasError)
  }
  
  # Reactive value to store the data
  data_reactive <- reactiveVal()
  
  # Reactive value to store sheet names
  sheet_names <- reactive({
    req(input$file)
    excel_sheets(input$file$datapath)
  })
  
  # Update sheet dropdown based on file input
  output.sheet_ui <- renderUI({
    req(sheet_names())
    selectInput("sheet", "Seleccionar Hoja", choices = sheet_names())
  })
  
  # Update column selections based on sheet choice
  observeEvent(input$sheet, {
    df <- read_excel(input$file$datapath, sheet = input$sheet)
    data_reactive(df)
    output$time_col_ui <- renderUI({
      req(df)
      selectInput("time_col", "Seleccionar Columna de Tiempo", choices = names(df))
    })
    output$value_col_ui <- renderUI({
      req(df)
      selectInput("value_col", "Seleccionar Columna de Observación", choices = names(df))
    })
  })
  
  # Run the model and plot when button is clicked
  model_data <- eventReactive(input$run_model, {
    req(input$time_col, input$value_col, data_reactive())
    
    n <- as.numeric(input$n)
    req(!is.na(n) && n > 0)
    
    pesos <- as.numeric(unlist(strsplit(input$pesos, "\\+")))
    req(length(pesos) > 0 && all(!is.na(pesos)))
    
    df <- data_reactive()
    
    if(length(pesos) != n) {
      showModal(modalDialog(
        title = "Error",
        "El número de pesos debe coincidir con el valor de n.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    serie <- ts(df[[input$value_col]], start = c(2012, 1), frequency = 12)
    
    promedio_movil <- SMA(df[[input$value_col]], n = n)
    promedio_movil_ponderado <- WMA(df[[input$value_col]], n = length(pesos), wts = pesos)
    
    df$promedio_movil <- promedio_movil
    df$promedio_movil_ponderado <- promedio_movil_ponderado
    
    adf_result <- adf.test(serie)
    
    list(promedio_movil = promedio_movil, promedio_movil_ponderado = promedio_movil_ponderado, adf_result = adf_result, df = df, pesos = pesos)
  })
  
  output$adf_test <- renderPrint({
    req(model_data())
    model_data()$adf_result
  })
  
  output$plot <- renderPlotly({
    req(model_data())
    df <- model_data()$df
    promedio_movil <- model_data()$promedio_movil
    promedio_movil_ponderado <- model_data()$promedio_movil_ponderado
    
    plot_ly(df, x = ~seq_along(df[[input$value_col]])) %>%
      add_lines(y = ~df[[input$value_col]], name = 'Serie original', line = list(color = 'blue')) %>%
      add_lines(y = ~promedio_movil, name = 'Promedio móvil', line = list(color = 'red', dash = 'dash'), na.rm = TRUE) %>%
      add_lines(y = ~promedio_movil_ponderado, name = 'Promedio móvil ponderado', line = list(color = 'green', dash = 'dot'), na.rm = TRUE) %>%
      layout(title = "Comparación de la Serie Original, Promedio Móvil y Promedio Móvil Ponderado",
             xaxis = list(title = "Periodo"),
             yaxis = list(title = "Valores"),
             legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2))
  })
  
  output$forecast <- renderPrint({
    req(model_data())
    df <- model_data()$df
    pesos <- model_data()$pesos
    valores_recientes_ponderado <- tail(df[[input$value_col]], length(pesos))
    pronostico_ponderado <- sum(valores_recientes_ponderado * pesos) / sum(pesos)
    
    cat("Pronóstico para el siguiente período (promedio ponderado de los últimos", length(pesos), "valores):", pronostico_ponderado)
  })
  
  output$error_report <- renderTable({
    req(model_data())
    df <- model_data()$df
    promedio_movil_ponderado <- model_data()$promedio_movil_ponderado
    pesos <- model_data()$pesos
    real <- tail(df[[input$value_col]], length(pesos))
    pronosticado <- tail(promedio_movil_ponderado, length(pesos))
    
    errores(pronosticado, real)
  })
  
  output$ip_user_info <- renderPrint({
    cat("IP del usuario:", ip_address(), "\nNombre de usuario:", username)
  })
}

# Ejecutar la aplicación Shiny en un puerto específico
shinyApp(ui = ui, server = server, options = list(port = 1236))
