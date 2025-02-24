# Cargar las librerías necesarias
library(shiny)
library(forecast)
library(readxl)
library(shinyjs)
library(ggplot2)
library(plotly)
library(writexl)

# Obtener el nombre de usuario del sistema
username <- Sys.info()[["user"]]

# Configuración para abrir en el navegador predeterminado
options(shiny.launch.browser = function(url) {
  browseURL(url)
})

# Cargar las librerías necesarias
library(shiny)
library(forecast)
library(readxl)
library(shinyjs)
library(ggplot2)
library(plotly)
library(writexl)

# Interfaz de usuario (UI)
ui <- fluidPage(
  useShinyjs(),
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
      .action-buttons .btn {
        margin-bottom: 10px;
        width: 100%;
      }
      table {
        width: 100%;
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
      h1("Suavización Exponencial Doble")
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      width = 3,
      
      h3("Carga de Datos"),
      fileInput("file", "Cargar Dataset (Excel)", accept = c(".xlsx")),
      uiOutput("sheet_selector"),
      uiOutput("time_column"),
      uiOutput("data_column"),
      hr(),
      
      h3("Parámetros del Modelo"),
      checkboxInput("optimize", "Optimizar parámetros automáticamente", value = TRUE),
      
      conditionalPanel(
        condition = "!input.optimize",
        numericInput("alpha", "Ingrese el valor de alfa (0 < alfa <= 1)", value = 0.8, min = 0.01, max = 1, step = 0.01),
        numericInput("beta", "Ingrese el valor de beta (0 < beta <= 1)", value = 0.2, min = 0.01, max = 1, step = 0.01)
      ),
      
      numericInput("h", "Cantidad de períodos a predecir", value = 12, min = 1, step = 1),
      hr(),
      
      div(class = "action-buttons",
          actionButton("run_model", "Ejecutar Modelo", icon = icon("play"), class = "btn-primary")
      ),
      hr(),
      
      h3("Proporción de Entrenamiento/Prueba"),
      sliderInput("train_ratio", "Proporción de Entrenamiento:", min = 0.5, max = 0.9, value = 0.8, step = 0.05),
      hr(),
      
      p("by: ", a("Toñesky en YouTube", href = "https://www.youtube.com/@Tonyesky", target = "_blank")),
      p("Desarrollado en 2024")
    ),
    
    mainPanel(
      class = "main-panel",
      width = 9,
      tabsetPanel(
        id = "tabset",
        
        # Pestaña de Pronóstico (usando datos completos)
        tabPanel(
          title = tagList(icon("chart-line"), "Pronóstico"),
          value = "pronostico",
          fluidRow(
            column(12, 
                   h3("Parámetros Optimizados"),
                   textOutput("optimized_params")
            )
          ),
          hr(),
          fluidRow(
            column(12, 
                   h3("Gráfico del Modelo y Pronóstico Futuro"),
                   plotlyOutput("plotModel")
            )
          ),
          hr(),
          fluidRow(
            column(12, 
                   downloadButton("download_forecast", "Descargar Pronóstico en Excel", icon = icon("download"))
            )
          )
        ),
        
        # Pestaña de Errores (usando partición train/test)
        tabPanel(
          title = tagList(icon("exclamation-triangle"), "Errores"),
          value = "errores",
          fluidRow(
            column(12, 
                   h3("Comparación Pronóstico vs Real (Conjunto de Prueba)"),
                   plotlyOutput("error_forecast_plot")
            )
          ),
          hr(),
          fluidRow(
            column(12, 
                   downloadButton("download_error_data", "Descargar Valores", icon = icon("download"))
            )
          ),
          hr(),
          fluidRow(
            column(12, 
                   h3("Métricas de Error (Conjunto de Prueba)"),
                   tableOutput("error_metrics")
            )
          )
        ),
        
        # Pestaña de Historial de Ejecuciones
        tabPanel(
          title = tagList(icon("history"), "Historial"),
          value = "historial",
          h3("Historial de la Consola - IP y Usuario"),
          fluidRow(
            column(12, tags$p(textOutput("ip_address")))
          ),
          hr(),
          fluidRow(
            column(12, tableOutput("console_history"))
          )
        )
      ),
      
      div(class = "footer",
          HTML("&copy; 2024 - by <a href='https://www.youtube.com/@Tonyesky' target='_blank'>Toñesky en YouTube</a>")
      )
    )
  )
)

# Servidor (Server)
server <- function(input, output, session) {
  
  # Obtener la dirección IP del usuario
  ip_address <- session$request$REMOTE_ADDR
  
  # Mostrar la dirección IP en la pestaña de historial
  output$ip_address <- renderText({
    paste("Dirección IP:", ip_address, " - Usuario:", username)
  })
  
  # Funciones reactivas para cargar datos
  dataset <- reactive({
    req(input$file)
    sheets <- excel_sheets(input$file$datapath)
    updateSelectInput(session, "sheet", choices = sheets)
    return(sheets)
  })
  
  sheet_data <- reactive({
    req(input$sheet)
    df <- read_excel(input$file$datapath, sheet = input$sheet)
    updateSelectInput(session, "time_column", choices = names(df))
    updateSelectInput(session, "data_column", choices = names(df))
    return(df)
  })
  
  # UI Renderizado
  output$sheet_selector <- renderUI({
    selectInput("sheet", "Seleccionar hoja", choices = dataset())
  })
  
  output$time_column <- renderUI({
    req(input$sheet)
    selectInput("time_column", "Seleccione la columna de tiempo", choices = names(sheet_data()))
  })
  
  output$data_column <- renderUI({
    req(input$sheet)
    selectInput("data_column", "Seleccione la columna de observación", choices = names(sheet_data()))
  })
  
  # Variables reactivas para almacenar el historial de la consola
  console_history <- reactiveVal(data.frame(Hora = character(0), Acción = character(0)))
  
  # Función para agregar eventos al historial
  add_to_console_history <- function(accion) {
    hora_actual <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    nuevo_evento <- data.frame(Hora = hora_actual, Acción = accion)
    console_history(rbind(console_history(), nuevo_evento))
  }
  
  # Función reactiva principal para el modelo
  model_results <- eventReactive(input$run_model, {
    req(input$time_column, input$data_column, input$h)
    df <- sheet_data()
    
    # Validación del valor de h
    h_val <- as.integer(input$h)
    validate(
      need(!is.na(h_val) && h_val >= 1, "Ingrese un valor válido para h (h >= 1).")
    )
    
    # Serie de tiempo completa
    full_data <- ts(df[[input$data_column]])
    
    # División train/test para evaluación
    train_size <- floor(input$train_ratio * length(full_data))
    train_data <- head(full_data, train_size)
    test_data <- tail(full_data, length(full_data) - train_size)
    
    # Modelo para evaluación (usando train/test)
    if (input$optimize) {
      eval_model <- holt(train_data, h = length(test_data))
      alpha_opt <- eval_model$model$par["alpha"]
      beta_opt <- eval_model$model$par["beta"]
    } else {
      req(input$alpha, input$beta)
      alpha_val <- as.numeric(input$alpha)
      beta_val <- as.numeric(input$beta)
      
      validate(
        need(!is.na(alpha_val) && alpha_val > 0 && alpha_val <= 1, "Ingrese un valor válido para alfa (0 < alfa <= 1)."),
        need(!is.na(beta_val) && beta_val > 0 && beta_val <= 1, "Ingrese un valor válido para beta (0 < beta <= 1).")
      )
      
      eval_model <- holt(train_data, h = length(test_data), alpha = alpha_val, beta = beta_val)
      alpha_opt <- alpha_val
      beta_opt <- beta_val
    }
    
    # Pronóstico para evaluación
    eval_forecast <- eval_model$mean
    
    # Modelo para pronóstico futuro (usando datos completos)
    future_model <- holt(full_data, h = h_val, alpha = alpha_opt, beta = beta_opt)
    future_forecast <- future_model$mean
    
    # Métricas de error (usando conjunto de prueba)
    accuracy_metrics <- accuracy(eval_forecast, test_data)
    
    list(
      full_data = full_data,
      train_data = train_data,
      test_data = test_data,
      eval_forecast = eval_forecast,
      future_forecast = future_forecast,
      accuracy_metrics = accuracy_metrics,
      alpha = alpha_opt,
      beta = beta_opt
    )
  })
  
  # Agregar eventos al historial cuando se carga un archivo
  observeEvent(input$file, {
    add_to_console_history(paste("Archivo cargado:", input$file$name))
  })
  
  # Agregar eventos al historial cuando se ejecuta el modelo
  observeEvent(input$run_model, {
    results <- model_results()
    add_to_console_history(paste("Modelo ejecutado con alpha =", round(results$alpha,4), "y beta =", round(results$beta,4)))
    
    # Seleccionar la pestaña de pronóstico
    updateTabsetPanel(session, "tabset", selected = "pronostico")
  })
  
  # Mostrar los parámetros optimizados
  output$optimized_params <- renderText({
    req(model_results())
    results <- model_results()
    paste("Alpha:", round(results$alpha, 4), " - Beta:", round(results$beta, 4))
  })
  
  # Gráfico del modelo completo y pronóstico futuro
  output$plotModel <- renderPlotly({
    req(model_results())
    results <- model_results()
    
    # Crear data frames para el gráfico
    df_original <- data.frame(
      Time = seq_along(results$full_data),
      Valor = as.numeric(results$full_data)
    )
    
    last_time <- max(df_original$Time)
    df_forecast <- data.frame(
      Time = seq(from = last_time + 1, length.out = length(results$future_forecast)),
      Valor = as.numeric(results$future_forecast)
    )
    
    # Crear el gráfico
    p <- ggplot() +
      geom_line(data = df_original, aes(x = Time, y = Valor, color = "Datos Históricos")) +
      geom_line(data = df_forecast, aes(x = Time, y = Valor, color = "Pronóstico")) +
      scale_color_manual(values = c("Datos Históricos" = "blue", "Pronóstico" = "red")) +
      ggtitle("Serie Temporal y Pronóstico") +
      xlab("Período") +
      ylab("Valor") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Descargar pronóstico
  output$download_forecast <- downloadHandler(
    filename = function() {
      paste("pronostico_des_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      results <- model_results()
      forecast_df <- data.frame(
        Período = seq_len(length(results$future_forecast)),
        Pronóstico = round(results$future_forecast, 2)
      )
      write_xlsx(forecast_df, path = file)
    }
  )
  
  # Descargar datos de errores
  output$download_error_data <- downloadHandler(
    filename = function() {
      paste0("datos_errores_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(model_results())
      results <- model_results()
      
      df_error <- data.frame(
        Time = seq_along(results$test_data),
        Real = results$test_data,
        Predicho = results$eval_forecast
      )
      
      write_xlsx(df_error, path = file)
    }
  )
  
  # Métricas de error
  output$error_metrics <- renderTable({
    req(model_results())
    metrics <- model_results()$accuracy_metrics
    metrics_df <- as.data.frame(metrics)[1, ]
    
    data.frame(
      Métrica = c("ME", "RMSE", "MAE", "MPE", "MAPE"),
      Valor = round(as.numeric(metrics_df[c("ME", "RMSE", "MAE", "MPE", "MAPE")]), 4)
    )
  })
  
  # Gráfico de evaluación (train/test)
  output$error_forecast_plot <- renderPlotly({
    req(model_results())
    results <- model_results()
    
    df_test <- data.frame(
      Time = seq_along(results$test_data),
      Real = results$test_data,
      Predicho = results$eval_forecast
    )
    
    p <- ggplot(df_test, aes(x = Time)) +
      geom_line(aes(y = Real, color = "Real")) +
      geom_line(aes(y = Predicho, color = "Predicho")) +
      scale_color_manual(values = c("Real" = "blue", "Predicho" = "red")) +
      ggtitle("Evaluación del Modelo (Conjunto de Prueba)") +
      xlab("Período") +
      ylab("Valor") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Historial de ejecuciones
  output$console_history <- renderTable({
    console_history()
  })
}


# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
