# Cargar las librerías necesarias
library(shiny)
library(forecast)
library(readxl)
library(tseries)    # Para adf.test
library(shinyjs)
library(ggplot2)
library(plotly)
library(writexl)

# Obtener el nombre de usuario del sistema
username <- Sys.info()[["user"]]

# Configuración para abrir en el navegador predeterminado
options(shiny.launch.browser = function(url) {
  browseURL(url)  # Abre la URL en el navegador predeterminado
})

# Interfaz de usuario (UI)
ui <- fluidPage(
  useShinyjs(),  # Para habilitar o deshabilitar elementos
  tags$head(
    tags$style(HTML("
      /* Estilos personalizados */
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
  
  # Título de la aplicación
  div(class = "title-panel", 
      h1("Suavización Exponencial Simple")
  ),
  
  # Diseño principal con panel lateral y panel principal
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      width = 3,  # Ajustar el ancho del panel lateral
      
      # Sección de carga de datos
      h3("Carga de Datos"),
      fileInput("file", "Cargar Dataset (Excel)", accept = c(".xlsx")),
      uiOutput("sheet_selector"),
      uiOutput("time_column"),
      uiOutput("data_column"),
      hr(),
      
      # Sección de parámetros del modelo
      h3("Parámetros del Modelo"),
      textInput("alpha", "Ingrese el valor de alfa (0 < alfa <= 1)", value = "0.2"),
      hr(),
      
      # Botones de acción separados con iconos
      div(class = "action-buttons",
          actionButton("run_model", "Ejecutar Modelo", icon = icon("play"), class = "btn-primary")
      ),
      hr(),
      
      # Información del autor y fecha de desarrollo
      p("by: ", a("Toñesky en YouTube", href = "https://www.youtube.com/@Tonyesky", target = "_blank")),
      p("Desarrollado en 2024")
    ),
    
    mainPanel(
      class = "main-panel",
      width = 9,  # Ajustar el ancho del panel principal
      tabsetPanel(
        id = "tabset",  # ID para controlar la pestaña activa
        
        # Pestaña de Resultados
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
                   verbatimTextOutput("forecast_value")
            )
          ),
          hr(),
          fluidRow(
            column(12, 
                   h3("Gráfico del Modelo"),
                   plotlyOutput("plotModel")
            )
          ),
          hr(),
          fluidRow(
            column(12, 
                   h3("Errores de Pronóstico"),
                   div(class = "table-container",
                       tableOutput("error_metrics")
                   )
            )
          )
        )
      ),
      
      # Pie de página personalizado
      div(class = "footer",
          HTML("&copy; 2024 - by <a href='https://www.youtube.com/@Tonyesky' target='_blank'>Toñesky en YouTube</a>")
      )
    )
  )
)

# Servidor (Server)
server <- function(input, output, session) {
  
  # Obtener la dirección IP del usuario
  ip_address <- reactive({
    session$request$REMOTE_ADDR
  })
  
  # Cargar las hojas del archivo Excel
  dataset <- reactive({
    req(input$file)
    sheets <- excel_sheets(input$file$datapath)
    updateSelectInput(session, "sheet", choices = sheets)
    return(sheets)
  })
  
  # Seleccionar la hoja del Excel
  sheet_data <- reactive({
    req(input$sheet)
    df <- read_excel(input$file$datapath, sheet = input$sheet)
    updateSelectInput(session, "time_column", choices = names(df))
    updateSelectInput(session, "data_column", choices = names(df))
    return(df)
  })
  
  # Mostrar las listas desplegables para seleccionar las columnas
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
  
  # Ejecutar el modelo al presionar el botón
  model_data <- eventReactive(input$run_model, {
    req(input$time_column, input$data_column, input$alpha)
    df <- sheet_data()
    
    # Validación del valor de alpha
    alpha_val <- as.numeric(input$alpha)
    validate(
      need(!is.na(alpha_val) && alpha_val > 0 && alpha_val <= 1, "Ingrese un valor válido para alfa (0 < alfa <= 1).")
    )
    
    # Asignar la serie de tiempo
    # Si la columna de tiempo es de clase Date o POSIXct, podrías necesitar ajustar la serie temporal
    # Por ahora, usaremos ts con frecuencia 1
    data_col <- ts(df[[input$data_column]])
    
    # Ajustar el modelo SES
    ses_model <- ses(data_col, alpha = alpha_val)
    
    # Realizar el test de Dickey-Fuller
    adf_result <- adf.test(data_col)
    
    # Realizar el pronóstico de 1 periodo
    forecasted <- forecast(ses_model, h = 1)
    
    # Calcular las métricas de precisión
    # accuracy function can be used on the model
    accuracy_metrics <- accuracy(ses_model)
    
    list(
      ses_model = ses_model,
      adf_result = adf_result,
      forecasted = forecasted,
      accuracy_metrics = accuracy_metrics
    )
  })
  
  # Mostrar la IP y el nombre de usuario
  output$ip_user_info <- renderPrint({
    cat("Dirección IP:", ip_address(), "\n")
    cat("Usuario:", username, "\n")
  })
  
  # Test de Dickey-Fuller
  output$adf_test <- renderPrint({
    req(model_data())
    adf_result <- model_data()$adf_result
    adf_result
  })
  
  # Gráfico del modelo con pronóstico de 1 periodo por defecto
  output$plotModel <- renderPlotly({
    req(model_data())
    forecasted <- model_data()$forecasted
    data_col <- model_data()$ses_model$x  # Obtener la serie de tiempo original
    
    # Crear data frames para plotear
    df_original <- data.frame(
      Time = time(data_col),
      Observaciones = as.numeric(data_col)
    )
    
    df_forecast <- data.frame(
      Time = time(forecasted$mean),
      Pronostico = as.numeric(forecasted$mean)
    )
    
    # Crear el gráfico con ggplot2
    p <- ggplot() +
      geom_line(data = df_original, aes(x = Time, y = Observaciones), color = "red") +
      geom_point(data = df_forecast, aes(x = Time, y = Pronostico), color = "red", size = 3) +
      ggtitle("Suavización Exponencial Simple") +
      xlab("Tiempo") +
      ylab("Observaciones") +
      theme_minimal()
    
    # Convertir a plotly para interactividad
    ggplotly(p)
  })
  
  # Mostrar el valor del pronóstico
  output$forecast_value <- renderPrint({
    req(model_data())
    forecasted <- model_data()$forecasted
    forecast_value <- as.numeric(forecasted$mean)
    cat("Pronóstico para el siguiente periodo:\n")
    cat(forecast_value)
  })
  
  # Mostrar las métricas de precisión
  output$error_metrics <- renderTable({
    req(model_data())
    metrics <- model_data()$accuracy_metrics
    # Convertir a data frame y seleccionar la primera fila (training)
    metrics_df <- as.data.frame(metrics)
    metrics_df <- metrics_df[1, ]  # Seleccionar solo las métricas de entrenamiento
    
    # Crear un data frame con nombres de métricas y valores
    metrics_df <- data.frame(
      Métrica = colnames(metrics_df),
      Valor = as.numeric(metrics_df),
      stringsAsFactors = FALSE
    )
    
    # Identificar métricas que son porcentuales
    percentage_metrics <- c("MPE", "MAPE")
    
    # Formatear las métricas
    metrics_df$Valor <- ifelse(metrics_df$Métrica %in% percentage_metrics, 
                               paste0(round(metrics_df$Valor, 2), "%"),
                               round(metrics_df$Valor, 4))
    
    # Retornar el data frame
    metrics_df
  }, 
  digits = 4, 
  rownames = FALSE,
  align = 'c')
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
