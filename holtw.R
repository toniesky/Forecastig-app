# Carga de librerías
suppressPackageStartupMessages({
  library(shiny)
  library(readxl)
  library(writexl)       # Para escribir archivos Excel
  library(forecast)
  library(ggplot2)
  library(lubridate)
  library(shinyjs)
  library(plotly)
  library(data.table)    # Para manejo eficiente de datos
  library(tsibble)       # Para detección de frecuencia      # Para convertir a tsibble
})

# Para manipulación de datos
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
    "))
  ),
  
  # Título de la aplicación
  div(class = "title-panel", 
      titlePanel("Holt-Winters Forecasting App")
  ),
  
  # Diseño principal con panel lateral y panel principal
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      width = 3,  # Ajustar el ancho del panel lateral
      # Sección de carga de datos
      h3("Carga de Datos"),
      fileInput("file", "Sube un archivo Excel o CSV", accept = c(".xlsx", ".csv")),
      uiOutput("sheet_select"),
      uiOutput("time_col_select"),
      uiOutput("obs_col_select"),
      hr(),
      # Sección de parámetros del modelo
      h3("Parámetros del Modelo"),
      checkboxInput("use_custom_freq", "Usar frecuencia personalizada", FALSE),
      conditionalPanel(
        condition = "input.use_custom_freq == false",
        selectInput("manual_freq", "Seleccionar frecuencia", 
                    choices = list(
                      "1 (Anual)" = 1,
                      "3 (Cuatrimestral)" = 3,
                      "4 (Trimestral)" = 4,
                      "12 (Mensual)" = 12,
                      "52 (Semanal)" = 52,
                      "365 (Diario)" = 365
                    ),
                    selected = 12)  # Frecuencia por defecto mensual
      ),
      conditionalPanel(
        condition = "input.use_custom_freq == true",
        numericInput("custom_freq", "Frecuencia personalizada", value = 1, min = 1)
      ),
      numericInput("forecast_periods", "Periodos a pronosticar", value = 12, min = 1),
      hr(),
      # Botones de acción separados con íconos
      div(class = "action-buttons",
          actionButton("run_model", "Ejecutar Modelo", icon = icon("play"), class = "btn-primary")
      ),
      hr(),
      radioButtons("model_type", "Modelo", choices = list("Aditivo" = "additive", "Multiplicativo" = "multiplicative")),
      checkboxInput("use_custom_params", "Usar parámetros personalizados", FALSE),
      conditionalPanel(
        condition = "input.use_custom_params == true",
        numericInput("alpha", "Nivel de Alpha", value = NA, min = 0, max = 1, step = 0.01),
        numericInput("beta", "Nivel de Beta", value = NA, min = 0, max = 1, step = 0.01),
        numericInput("gamma", "Nivel de Gamma", value = NA, min = 0, max = 1, step = 0.01)
      ),
      hr(),
      # Sección de proporción de entrenamiento y prueba
      h3("Proporción de Entrenamiento/Prueba"),
      sliderInput("train_ratio", "Proporción de Entrenamiento:", min = 0.5, max = 0.9, value = 0.8, step = 0.05),
      hr(),
      # Información del autor y fecha de desarrollo
      p("by: ", a("Toñesky en YouTube", href = "https://www.youtube.com/@Tonyesky", target = "_blank")),
      p("Desarrollado en 2024")
    ),
    
    mainPanel(
      class = "main-panel",
      width = 9,  # Ajustar el ancho del panel principal
      tabsetPanel(
        id = "tabset",
        # Pestaña de Serie de Tiempo
        tabPanel(
          title = tagList(icon("chart-line"), "Serie de Tiempo"),
          value = "serie_tiempo",
          fluidRow(
            column(12, plotlyOutput("ts_plot"))
          ),
          hr(),
          fluidRow(
            column(12, plotlyOutput("decomp_plot"))
          )
        ),
        # Pestaña de Pronóstico
        tabPanel(
          title = tagList(icon("clipboard-list"), "Pronóstico"),
          value = "pronostico",
          fluidRow(
            column(12, verbatimTextOutput("optimized_params"))
          ),
          hr(),
          fluidRow(
            column(12, plotlyOutput("forecast_plot"))
          ),
          hr(),
          fluidRow(
            column(12, 
                   # Entrada para el nombre del archivo
                   textInput("download_name", "Nombre del archivo para descargar (sin extensión):", value = "pronostico_holt_winters"),
                   # Botón de descarga y nombre del archivo
                   downloadButton("download_forecast", "Descargar Pronóstico en Excel", icon = icon("download")),
                   br(), br(),
                   # Mostrar el nombre del archivo que se descargará
                   textOutput("download_filename")
            )
          )
        ),
        # Pestaña de Errores
        tabPanel(
          title = tagList(icon("exclamation-triangle"), "Errores"),
          value = "errores",
          fluidRow(
            column(12, plotlyOutput("error_forecast_plot"))
          ),
          hr(),
          fluidRow(
            column(12, 
                   # Botón de descarga de valores
                   downloadButton("download_errors", "Descargar Valores en Excel", icon = icon("download"))
            )
          ),
          hr(),
          fluidRow(
            column(12, tableOutput("error_table"))
          )
        ),
        # Pestaña de Historial de Ejecuciones
        tabPanel(
          title = tagList(icon("history"), "Historial"),
          value = "historial",
          h3("Historial de la Consola - IP y Usuario"),
          fluidRow(
            column(12, uiOutput("ip_address"))  # Mostrar la dirección IP
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

# Lógica del servidor
server <- function(input, output, session) {
  
  # Inicializar shinyjs: deshabilitar el botón de descarga al inicio
  shinyjs::disable("download_forecast")
  
  # Variables reactivas para almacenar los datos y el historial
  sheet_data <- reactiveVal()
  console_history <- reactiveVal(data.frame(Hora = character(0), Acción = character(0)))
  forecast_df <- reactiveVal()  # Para almacenar el pronóstico y permitir la descarga
  
  # Variable reactiva para almacenar los parámetros optimizados
  optimized_params <- reactiveVal(NULL)
  
  # Obtener la dirección IP del usuario
  ip_address <- reactive({
    session$request$REMOTE_ADDR
  })
  
  # Mostrar la dirección IP en la pestaña de historial
  output$ip_address <- renderUI({
    tags$p(paste("Dirección IP:", ip_address(), " - Usuario:", Sys.info()[["user"]]))
  })
  
  # Leer las hojas del archivo cargado
  observeEvent(input$file, {
    req(input$file)
    add_to_console_history(paste("Archivo cargado:", input$file$name))
    
    ext <- tools::file_ext(input$file$name)
    if (tolower(ext) == "csv") {
      # Uso de data.table::fread para lectura eficiente de CSV
      data <- tryCatch({
        fread(input$file$datapath)
      }, error = function(e) {
        add_to_console_history("Error al leer el archivo CSV.")
        showNotification("Error al leer el archivo CSV. Por favor, verifica el formato.", type = "error")
        return(NULL)
      })
      if (!is.null(data)) {
        sheet_data(data)
        output$sheet_select <- renderUI(NULL)
        output$time_col_select <- renderUI({
          selectInput("time_col", "Selecciona la columna de tiempo:", choices = names(data), selected = NULL)
        })
        output$obs_col_select <- renderUI({
          selectInput("obs_col", "Selecciona la columna de observación:", choices = names(data), selected = NULL)
        })
      }
    } else if (tolower(ext) %in% c("xlsx", "xls")) {
      sheets <- excel_sheets(input$file$datapath)
      output$sheet_select <- renderUI({
        selectInput("sheet", "Selecciona la hoja:", choices = sheets)
      })
    } else {
      showNotification("Formato de archivo no soportado.", type = "error")
    }
  })
  
  # Leer los datos de la hoja seleccionada
  observeEvent(input$sheet, {
    req(input$file, input$sheet)
    data <- tryCatch({
      as.data.table(read_excel(input$file$datapath, sheet = input$sheet))
    }, error = function(e) {
      add_to_console_history("Error al leer la hoja de Excel.")
      showNotification("Error al leer la hoja de Excel. Por favor, verifica el formato.", type = "error")
      return(NULL)
    })
    if (!is.null(data)) {
      sheet_data(data)  # Almacena los datos para su uso posterior
      
      # Habilita la selección de columnas
      output$time_col_select <- renderUI({
        selectInput("time_col", "Selecciona la columna de tiempo:", choices = names(data), selected = NULL)
      })
      
      output$obs_col_select <- renderUI({
        selectInput("obs_col", "Selecciona la columna de observación:", choices = names(data), selected = NULL)
      })
    }
  })
  
  # Validar los datos seleccionados
  validate_data <- function(data, time_col, obs_col) {
    # Intentar convertir la columna de tiempo a fecha automáticamente
    time_data <- parse_date_time(data[[time_col]], orders = c("ymd", "dmy", "mdy", "ym", "my", "Ymd HMS", "Ymd HM", "Ymd"))
    
    # Si la conversión a fecha falla (todos NA), tratar como índice numérico
    if (all(is.na(time_data))) {
      time_data <- as.numeric(data[[time_col]])
      is_date <- FALSE
    } else {
      is_date <- TRUE
    }
    
    # Validar que la columna de observaciones sea numérica
    obs_data <- tryCatch(as.numeric(gsub(",", ".", data[[obs_col]])),
                         error = function(e) return(NA))
    
    return(list(time_data = time_data, obs_data = obs_data, is_date = is_date))
  }
  
  # Detectar la frecuencia automáticamente cuando se selecciona la columna de observaciones
  observeEvent(input$obs_col, {
    req(input$time_col, input$obs_col)
    
    data <- sheet_data()
    
    # Validar los datos
    validated_data <- validate_data(data, input$time_col, input$obs_col)
    
    if (any(is.na(validated_data$time_data)) || any(is.na(validated_data$obs_data))) {
      showNotification("Datos inválidos en las columnas seleccionadas. Asegúrate de que la columna de tiempo y observación sean correctas.", type = "error")
      shinyjs::disable("run_model")
    } else {
      shinyjs::enable("run_model")
    }
  })
  
  # Crear la serie de tiempo
  ts_data <- reactive({
    req(input$time_col, input$obs_col)
    data <- sheet_data()
    
    validated_data <- validate_data(data, input$time_col, input$obs_col)
    time_col <- validated_data$time_data
    obs_col <- validated_data$obs_data
    is_date <- validated_data$is_date
    
    # Determinar la frecuencia
    frequency_val <- if (input$use_custom_freq) {
      input$custom_freq
    } else {
      as.numeric(input$manual_freq)
    }
    
    if (is_date) {
      # Determinar el inicio basado en la fecha y frecuencia
      start_year <- year(min(time_col))
      if (frequency_val == 1) { # Anual
        start_val <- c(start_year, 1)
      } else if (frequency_val == 4) { # Trimestral
        start_val <- c(start_year, quarter(min(time_col)))
      } else if (frequency_val == 3) { # Cuatrimestral
        start_val <- c(start_year, (month(min(time_col)) - 1) %/% 4 + 1)
      } else if (frequency_val == 12) { # Mensual
        start_val <- c(start_year, month(min(time_col)))
      } else if (frequency_val == 52) { # Semanal
        start_val <- c(start_year, week(min(time_col)))
      } else if (frequency_val == 365) { # Diario
        start_val <- c(start_year, yday(min(time_col)))
      } else {
        start_val <- c(start_year, 1)
      }
    } else {
      # Tratar como índice numérico ascendente
      start_val <- min(time_col)
    }
    
    # Crear la serie con la frecuencia determinada
    ts(obs_col, start = start_val, frequency = frequency_val)
  })
  
  # Ejecutar el modelo Holt-Winters y generar pronósticos
  forecast_model <- eventReactive(input$run_model, {
    req(ts_data())
    
    n <- length(ts_data())
    n_train <- round(n * input$train_ratio)
    train_data <- window(ts_data(), end = c(time(ts_data())[n_train]))
    test_data <- window(ts_data(), start = c(time(ts_data())[n_train + 1]))
    
    if (input$use_custom_params && !is.na(input$alpha) && !is.na(input$beta) && !is.na(input$gamma)) {
      model <- tryCatch({
        HoltWinters(train_data, alpha = input$alpha, beta = input$beta, gamma = input$gamma, seasonal = input$model_type)
      }, error = function(e) {
        showNotification("Error al ajustar el modelo con parámetros personalizados.", type = "error")
        return(NULL)
      })
    } else {
      model <- tryCatch({
        HoltWinters(train_data, seasonal = input$model_type)
      }, error = function(e) {
        showNotification("Error al ajustar el modelo Holt-Winters.", type = "error")
        return(NULL)
      })
    }
    
    if (is.null(model)) {
      return(NULL)
    }
    
    forecast_result <- tryCatch({
      forecast(model, h = length(test_data))
    }, error = function(e) {
      showNotification("Error al generar el pronóstico.", type = "error")
      return(NULL)
    })
    
    if (is.null(forecast_result)) {
      return(NULL)
    }
    
    # Guardar los parámetros optimizados en la variable reactiva
    optimized_params(paste("Alpha =", round(model$alpha, 3), 
                           ", Beta =", round(model$beta, 3), 
                           ", Gamma =", round(model$gamma, 3)))
    
    # Agregar al historial la ejecución del modelo
    add_to_console_history(paste("Modelo Holt-Winters ejecutado. Alpha =", round(model$alpha, 3), 
                                 "Beta =", round(model$beta, 3), 
                                 "Gamma =", round(model$gamma, 3), 
                                 "Tipo de Modelo =", input$model_type))
    
    # Seleccionar la pestaña "Pronóstico"
    updateTabsetPanel(session, "tabset", selected = "pronostico")
    
    list(forecast = forecast_result, test_data = test_data, train_data = train_data, full_model = model)
  })
  
  # Generar el pronóstico futuro completo y actualizar la tabla de pronóstico
  future_forecast <- eventReactive(input$run_model, {
    req(ts_data())
    
    if (input$use_custom_params && !is.na(input$alpha) && !is.na(input$beta) && !is.na(input$gamma)) {
      model_full <- tryCatch({
        HoltWinters(ts_data(), alpha = input$alpha, beta = input$beta, gamma = input$gamma, seasonal = input$model_type)
      }, error = function(e) {
        showNotification("Error al ajustar el modelo completo con parámetros personalizados.", type = "error")
        return(NULL)
      })
    } else {
      model_full <- tryCatch({
        HoltWinters(ts_data(), seasonal = input$model_type)
      }, error = function(e) {
        showNotification("Error al ajustar el modelo Holt-Winters completo.", type = "error")
        return(NULL)
      })
    }
    
    if (is.null(model_full)) {
      return(NULL)
    }
    
    future_forecast_result <- forecast(model_full, h = input$forecast_periods)
    
    # Crear la tabla de pronóstico futuro
    forecast_table_df <- data.frame(
      `Periodo pronóstico` = 1:length(future_forecast_result$mean), 
      Pronóstico = as.numeric(future_forecast_result$mean)
    )
    forecast_df(forecast_table_df)  # Asignar a forecast_df para uso en la tabla y descarga
    
    future_forecast_result
  })
  
  # Mostrar parámetros optimizados en la pestaña de Pronóstico
  output$optimized_params <- renderText({
    req(optimized_params())
    paste("Parámetros optimizados:", optimized_params())
  })
  
  # Serie de Tiempo Original
  output$ts_plot <- renderPlotly({
    req(ts_data())
    ts_df <- data.frame(Time = time(ts_data()), Value = ts_data())
    p <- ggplot(ts_df, aes(x = Time, y = Value)) +
      geom_line(color = "blue") +
      ggtitle("Serie de Tiempo Original") +
      xlab("Tiempo") + ylab("Valor")
    ggplotly(p)
  })
  
  # Gráfico de Descomposición de la Serie de Tiempo
  observeEvent(input$manual_freq, {
    req(input$manual_freq)
    # Realizar la descomposición solo cuando se cambie la frecuencia
    output$decomp_plot <- renderPlotly({
      req(ts_data(), input$manual_freq)
      validate(need(input$manual_freq, "Selecciona una frecuencia para ver la descomposición."))
      
      # Realizar la descomposición
      decomposed <- tryCatch({
        decompose(ts_data(), type = ifelse(input$model_type == "additive", "additive", "multiplicative"))
      }, error = function(e) {
        showNotification("Error al descomponer la serie de tiempo.", type = "error")
        return(NULL)
      })
      
      if (is.null(decomposed)) {
        return(NULL)
      }
      
      # Convertir a data frame para ggplot
      decomp_df <- data.frame(
        Time = time(ts_data()),
        Observado = decomposed$x,
        Tendencia = decomposed$trend,
        Estacionalidad = decomposed$seasonal,
        Residuales = decomposed$random
      )
      
      # Melt para ggplot
      decomp_melt <- melt(decomp_df, id.vars = "Time", variable.name = "Componente", value.name = "Valor")
      
      # Crear el gráfico
      p <- ggplot(decomp_melt, aes(x = Time, y = Valor, color = Componente)) +
        geom_line() +
        facet_wrap(~Componente, ncol = 1, scales = "free_y") +
        ggtitle("Descomposición de la Serie de Tiempo") +
        theme(legend.position = "none") +
        xlab("Tiempo") + ylab("Valor")
      
      ggplotly(p)
    })
  })
  
  # Gráfico de Pronóstico (Serie Histórica + Pronóstico Futuro)
  output$forecast_plot <- renderPlotly({
    req(future_forecast(), ts_data())
    
    ts_length <- length(ts_data())
    forecast_length <- length(future_forecast()$mean)
    
    # Crear un vector de tiempo que continúa después de la serie original
    time_series <- c(time(ts_data()), seq(max(time(ts_data())) + deltat(ts_data()), 
                                          by = deltat(ts_data()), length.out = forecast_length))
    
    # Combinar los datos históricos y el pronóstico en un solo dataframe
    forecast_df_plot <- data.frame(
      Time = time_series,
      Value = c(as.numeric(ts_data()), as.numeric(future_forecast()$mean)),
      Tipo = rep(c("Histórico", "Pronóstico"), times = c(ts_length, forecast_length))
    )
    
    p <- ggplot(forecast_df_plot, aes(x = Time, y = Value, color = Tipo)) +
      geom_line() +
      ggtitle("Serie de Tiempo con Pronóstico Futuro") +
      xlab("Tiempo") + ylab("Valor")
    
    ggplotly(p)
  })
  
  # Gráfico de errores: comparación entre pronóstico y conjunto de prueba
  output$error_forecast_plot <- renderPlotly({
    req(forecast_model())
    
    actual <- forecast_model()$test_data
    predicted <- forecast_model()$forecast$mean
    
    df <- data.frame(
      Time = time(actual),
      Real = as.numeric(actual),
      Predicho = as.numeric(predicted)
    )
    
    p <- ggplot(df, aes(x = Time)) +
      geom_line(aes(y = Real, color = "Real")) +
      geom_line(aes(y = Predicho, color = "Predicho")) +
      ggtitle("Pronóstico vs Datos Reales (Conjunto de Prueba)") +
      xlab("Tiempo") + ylab("Valor")
    
    ggplotly(p)
  })
  
  # Mostrar tabla de errores
  output$error_table <- renderTable({
    req(forecast_model())
    
    actual <- forecast_model()$test_data
    predicted <- forecast_model()$forecast$mean
    
    ErrorAbsoluto <- abs(actual - predicted)
    ErrorCuadrado <- ErrorAbsoluto^2
    ProporcionError <- ErrorAbsoluto / actual
    Sesgo <- mean(predicted - actual, na.rm = TRUE)  # Cálculo del sesgo (bias)
    
    MAD <- mean(ErrorAbsoluto, na.rm = TRUE)
    MAPE <- mean(ProporcionError, na.rm = TRUE)
    MSE <- mean(ErrorCuadrado, na.rm = TRUE)
    RMSE <- sqrt(MSE)
    
    data.frame(
      Error = c("MAPE", "MAD", "MSE", "RMSE", "Sesgo (Bias)"),
      Valor = c(paste0(round(MAPE * 100, 2), "%"), round(MAD, 2), round(MSE, 2), round(RMSE, 2), round(Sesgo, 2))
    )
  })
  
  # Historial de ejecuciones
  output$console_history <- renderTable({
    console_history()
  })
  
  # Mostrar el nombre del archivo a descargar
  output$download_filename <- renderText({
    req(forecast_df())
    paste("Archivo para descargar:", paste0(input$download_name, "_", Sys.Date(), ".xlsx"))
  })
  
  # Descargar el pronóstico en Excel (solo periodo y pronóstico)
  output$download_forecast <- downloadHandler(
    filename = function() {
      # Validar que el usuario haya proporcionado un nombre
      file_name <- ifelse(nchar(input$download_name) > 0, input$download_name, "pronostico_holt_winters")
      paste0(file_name, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # Escribir la tabla de pronóstico simplificada en el archivo
      write_xlsx(forecast_df(), path = file)
    }
  )
  
  # Descargar los datos de errores en Excel
  output$download_errors <- downloadHandler(
    filename = function() {
      paste0("valores_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(forecast_model())
      actual <- forecast_model()$test_data
      predicted <- forecast_model()$forecast$mean
      error_data <- data.frame(Real = as.numeric(actual), Predicho = as.numeric(predicted))
      write_xlsx(error_data, path = file)
    }
  )
  
  # Función para agregar eventos al historial
  add_to_console_history <- function(accion) {
    hora_actual <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    nuevo_evento <- data.frame(Hora = hora_actual, Acción = accion)
    console_history(rbind(console_history(), nuevo_evento))
  }
  
  # Ejecutar el modelo y habilitar el botón de descarga
  observeEvent(input$run_model, {
    forecast_model()
    future_forecast()
    
    # Habilitar el botón de descarga una vez que se haya generado el pronóstico
    shinyjs::enable("download_forecast")
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)

