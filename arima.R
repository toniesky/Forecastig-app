## Carga de funciones específicas de las librerías
suppressPackageStartupMessages({
  library(shiny)
  library(readxl)
  library(shinyjs)
  library(ggplot2)
  library(plotly)
  library(writexl)
  library(forecast)  # Necesario para forecast::Arima
  library(tseries)   # Necesario para adf.test
  library(car)       # Necesario para algunas pruebas diagnósticas
  library(lmtest)    # Para pruebas de diagnóstico
  library(stats)     # Para funciones estadísticas básicas
  library(dplyr)     # Para manipulación de datos
})

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
      h1("Modelos Box-Jenkins (ARIMA)")
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
      numericInput("p", "Valor de p:", value = 0, min = 0),
      numericInput("d", "Valor de d:", value = 0, min = 0),
      numericInput("q", "Valor de q:", value = 0, min = 0),
      numericInput("forecast_periods", "Periodos a pronosticar:", value = 12, min = 1),
      hr(),
      # Botones de acción separados
      div(class = "action-buttons",
          actionButton("run_arima", "Ejecutar ARIMA", icon = icon("play"), class = "btn-primary"),
          actionButton("auto_arima", "Auto ARIMA", icon = icon("magic"), class = "btn-primary")
      ),
      hr(),
      # Opciones Auto ARIMA
      h3("Opciones Auto ARIMA"),
      actionButton("toggle_auto_arima", "Mostrar/Ocultar Opciones", icon = icon("chevron-down")),
      hidden(
        div(id = "auto_arima_options",
            selectInput("ic", "Criterio de Información (ic)", choices = c("AIC" = "aic", "AICc" = "aicc", "BIC" = "bic"), selected = "aic"),
            checkboxInput("seasonal", "Considerar estacionalidad (seasonal)", value = FALSE),
            checkboxInput("stepwise", "Búsqueda Paso a Paso (stepwise)", value = FALSE),
            checkboxInput("approximation", "Usar Aproximación (approximation)", value = FALSE),
            checkboxInput("parallel", "Ejecución Paralela (parallel)", value = FALSE)
        )
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
        id = "tabset",  # ID para controlar la pestaña activa
        # Reordenar las pestañas
        tabPanel(
          title = tagList(icon("chart-line"), "Análisis"),
          value = "analisis",  # Asignar un valor único a la pestaña
          fluidRow(
            column(4,
                   h3("Análisis Preliminar"),
                   numericInput("diff_order", "Orden de diferenciación (d):", value = 0, min = 0),
                   actionButton("run_analysis", "Ejecutar Análisis", icon = icon("chart-bar"))
            ),
            column(8,
                   h3("Test de Dickey-Fuller"),
                   verbatimTextOutput("adf_test")
            )
          ),
          hr(),
          fluidRow(
            column(12, plotlyOutput("ts_plot"))
          ),
          hr(),
          fluidRow(
            column(6, plotlyOutput("acf_plot")),
            column(6, plotlyOutput("pacf_plot"))
          )
        ),
        tabPanel(
          title = tagList(icon("clipboard-list"), "Pronóstico"),
          value = "pronostico",  # Asignar un valor único a la pestaña
          h3("Reporte del Modelo"),
          fluidRow(
            column(12, verbatimTextOutput("error_metrics"))
          ),
          hr(),
          fluidRow(
            column(12, plotlyOutput("forecast_plot"))
          ),
          hr(),
          fluidRow(
            column(12, downloadButton("download_forecast", "Descargar Pronóstico en Excel", icon = icon("download")))
          )
        ),
        tabPanel(
          title = tagList(icon("exclamation-triangle"), "Errores de Pronóstico"),
          value = "errores",  # Asignar un valor único a la pestaña
          fluidRow(
            column(12, plotlyOutput("error_forecast_plot"))
          ),
          hr(),
          fluidRow(
            column(12, tableOutput("error_table"))
          ),
          # Add download button for error data
          div(class = "download-button",
              downloadButton("download_error_data", "Descargar Datos de Errores", icon = icon("download"))
          )
        ),
        tabPanel(
          title = tagList(icon("balance-scale"), "Supuestos de Residuos"),
          value = "residuos",  # Asignar un valor único a la pestaña
          h3("Normalidad de Residuos"),
          fluidRow(
            column(12, plotlyOutput("hist_residuals"))
          ),
          fluidRow(
            column(12, verbatimTextOutput("normality_test"))
          ),
          hr(),
          h3("Autocorrelación de Residuos"),
          fluidRow(
            column(12, plotlyOutput("residual_acf"))
          ),
          fluidRow(
            column(12, verbatimTextOutput("ljung_box_test"))
          ),
          hr(),
          h3("Gráfico de Residuos por Observación"),
          fluidRow(
            column(12, plotlyOutput("residual_time_plot"))
          )
        ),
        tabPanel(
          title = tagList(icon("history"), "Historial"),
          value = "historial",  # Asignar un valor único a la pestaña
          h3("Historial de la Consola - IP y Usuario"),
          fluidRow(
            column(12, uiOutput("ip_address"))
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
  suppressWarnings({
    # Variables reactivas para almacenar los datos y el historial
    sheet_data <- reactiveVal()
    console_history <- reactiveVal(data.frame(Hora = character(0), Acción = character(0)))
    forecast_df <- reactiveVal()  # Para almacenar el pronóstico y permitir la descarga
    
    # Variable reactiva para almacenar el orden de diferenciación utilizado en el análisis
    analysis_order <- reactiveVal(0)
    
    # Obtener la dirección IP del usuario
    ip_address <- reactive({
      session$request$REMOTE_ADDR
    })
    
    # Obtener el nombre de usuario del sistema
    username <- Sys.info()[["user"]]
    
    # Mostrar la dirección IP y el nombre de usuario en la pestaña de historial
    output$ip_address <- renderUI({
      tags$p(paste("Dirección IP:", ip_address(), " - Usuario:", username))
    })
    
    # Limpiar datos al cargar un archivo nuevo
    observeEvent(input$file, {
      sheet_data(NULL)
      
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      
      # Leer el archivo según su extensión
      if (ext == "csv") {
        data <- read.csv(input$file$datapath)
        sheet_data(data)
        output$sheet_select <- renderUI(NULL)
      } else if (ext == "xlsx") {
        sheets <- excel_sheets(input$file$datapath)
        output$sheet_select <- renderUI({
          selectInput("sheet", "Selecciona la hoja:", choices = sheets)
        })
      }
      
      add_to_console_history(paste("Archivo cargado:", input$file$name))
    })
    
    # Leer datos de la hoja seleccionada
    observeEvent(input$sheet, {
      req(input$file, input$sheet)
      data <- read_excel(input$file$datapath, sheet = input$sheet)
      sheet_data(data)
      
      add_to_console_history(paste("Hoja seleccionada:", input$sheet))
    })
    
    # Mostrar opciones de columnas para selección
    observe({
      data <- sheet_data()
      req(data)
      
      output$time_col_select <- renderUI({
        selectInput("time_col", "Selecciona la columna de tiempo:", choices = c("", names(data)), selected = NULL)
      })
      
      output$obs_col_select <- renderUI({
        selectInput("obs_col", "Selecciona la columna de observación:", choices = c("", names(data)), selected = NULL)
      })
    })
    
    # Ejecutar Análisis: Dickey-Fuller, ACF, PACF
    observeEvent(input$run_analysis, {
      req(input$time_col, input$obs_col, sheet_data())
      
      data <- sheet_data()
      ts_data <- ts(data[[input$obs_col]])
      
      # Almacenar el orden de diferenciación utilizado
      analysis_order(input$diff_order)
      
      # Diferenciación según valor seleccionado
      if (input$diff_order == 0) {
        analysis_data <- ts_data
      } else {
        analysis_data <- diff(ts_data, differences = input$diff_order)
      }
      
      # Mostrar el resultado del Test de Dickey-Fuller
      output$adf_test <- renderPrint({
        adf_test_result <- suppressWarnings(adf.test(analysis_data))
        adf_test_result
      })
      
      # Gráfico de la serie de tiempo
      output$ts_plot <- renderPlotly({
        ts_df <- data.frame(Time = 1:length(analysis_data), Value = analysis_data)
        p <- ggplot(ts_df, aes(x = Time, y = Value)) +
          geom_line(color = "blue") +
          ggtitle(paste("Serie de Tiempo Diferenciada (d =", analysis_order(), ")")) +
          xlab("Tiempo") + ylab("Valor")
        ggplotly(p)
      })
      
      # Gráfico ACF
      output$acf_plot <- renderPlotly({
        acf_data <- acf(analysis_data, plot = FALSE)
        n <- length(analysis_data)
        conf_limit <- 1.96 / sqrt(n)
        p <- ggplot(data.frame(lag = acf_data$lag[-1], acf = acf_data$acf[-1]), aes(x = lag, y = acf)) +
          geom_bar(stat = "identity", fill = "lightblue") +
          geom_hline(yintercept = conf_limit, linetype = "dashed", color = "red") +
          geom_hline(yintercept = -conf_limit, linetype = "dashed", color = "red") +
          ggtitle(paste("ACF de la Serie Diferenciada (d =", analysis_order(), ")")) +
          xlab("Lag") + ylab("ACF")
        ggplotly(p)
      })
      
      # Gráfico PACF
      output$pacf_plot <- renderPlotly({
        pacf_data <- pacf(analysis_data, plot = FALSE)
        n <- length(analysis_data)
        conf_limit <- 1.96 / sqrt(n)
        p <- ggplot(data.frame(lag = pacf_data$lag, pacf = pacf_data$acf), aes(x = lag, y = pacf)) +
          geom_bar(stat = "identity", fill = "lightblue") +
          geom_hline(yintercept = conf_limit, linetype = "dashed", color = "red") +
          geom_hline(yintercept = -conf_limit, linetype = "dashed", color = "red") +
          ggtitle(paste("PACF de la Serie Diferenciada (d =", analysis_order(), ")")) +
          xlab("Lag") + ylab("PACF")
        ggplotly(p)
      })
      
      add_to_console_history(paste("Análisis ejecutado con orden de diferenciación d =", input$diff_order))
    })
    
    # Ejecutar ARIMA manual y mostrar el pronóstico
    observeEvent(input$run_arima, {
      req(input$time_col, input$obs_col, sheet_data())
      
      data <- sheet_data()
      ts_data <- ts(data[[input$obs_col]])
      
      # División en entrenamiento y prueba según la proporción seleccionada
      n_train <- round(length(ts_data) * input$train_ratio)
      train_data <- window(ts_data, end = n_train)
      test_data <- window(ts_data, start = n_train + 1)
      
      # Ajustar modelo ARIMA
      model <- Arima(train_data, order = c(input$p, input$d, input$q))
      
      # Pronóstico sobre el conjunto de prueba
      forecast_test <- forecast(model, h = length(test_data))
      
      # Modelo ARIMA completo para pronóstico futuro
      model_full <- Arima(ts_data, order = c(input$p, input$d, input$q))
      forecast_full <- forecast(model_full, h = input$forecast_periods)
      
      # Guardar el pronóstico para descarga (sin límites de confianza)
      forecast_df_temp <- data.frame(
        Periodo = time(forecast_full$mean), 
        Pronóstico = as.numeric(forecast_full$mean)
      )
      forecast_df(forecast_df_temp)
      
      # Mostrar el orden y los coeficientes del modelo ARIMA
      output$error_metrics <- renderPrint({
        order <- arimaorder(model_full)
        coefs <- coef(model_full)
        list(
          Orden = order,
          Coeficientes = coefs,
          AIC = AIC(model_full),
          BIC = BIC(model_full)
        )
      })
      
      # Mostrar el pronóstico en tabla
      output$data <- renderTable({
        forecast_df()
      })
      
      # Gráfico de pronóstico futuro
      output$forecast_plot <- renderPlotly({
        original_data <- data.frame(Time = 1:length(ts_data), Value = as.numeric(ts_data), Tipo = "Original")
        forecast_data <- data.frame(Time = (length(ts_data) + 1):(length(ts_data) + input$forecast_periods), 
                                    Value = as.numeric(forecast_full$mean), Tipo = "Pronóstico")
        combined_data <- rbind(original_data, forecast_data)
        
        p <- ggplot(combined_data, aes(x = Time, y = Value, color = Tipo)) +
          geom_line() +
          ggtitle(paste("Pronóstico ARIMA (", input$p, ",", input$d, ",", input$q, ") usando todos los datos")) +
          xlab("Tiempo") + ylab("Valor")
        
        ggplotly(p)
      })
      
      # Calcular errores en el conjunto de prueba
      output$error_table <- renderTable({
        actual <- test_data
        predicted <- forecast_test$mean
        
        # Cálculo de errores
        ErrorAbsoluto <- abs(actual - predicted)
        ErrorCuadrado <- ErrorAbsoluto^2
        ProporcionError <- ErrorAbsoluto / actual
        Sesgo <- mean(predicted - actual, na.rm = TRUE)
        
        MAD <- mean(ErrorAbsoluto, na.rm = TRUE)
        MAPE <- mean(ProporcionError, na.rm = TRUE)
        MSE <- mean(ErrorCuadrado, na.rm = TRUE)
        RMSE <- sqrt(MSE)
        
        data.frame(
          Error = c("MAPE", "MAD", "MSE", "RMSE", "Sesgo (Bias)"),
          Valor = c(paste0(round(MAPE * 100, 2), "%"), round(MAD, 2), round(MSE, 2), round(RMSE, 2), round(Sesgo, 2))
        )
      })
      
      # Gráfico de pronóstico vs datos reales
      output$error_forecast_plot <- renderPlotly({
        df <- data.frame(
          Time = (n_train + 1):length(ts_data),
          Real = test_data,
          Predicho = forecast_test$mean
        )
        
        p <- ggplot(df, aes(x = Time)) +
          geom_line(aes(y = Real, color = "Real")) +
          geom_line(aes(y = Predicho, color = "Predicho")) +
          ggtitle("Pronóstico vs Datos Reales (Conjunto de Prueba)") +
          xlab("Tiempo") + ylab("Valor")
        
        ggplotly(p)
      })
      
      # Cambiar a la pestaña "Pronóstico" utilizando el valor asignado
      updateTabsetPanel(session, "tabset", selected = "pronostico")
      
      showNotification(paste("Modelo ARIMA ejecutado correctamente con parámetros: p =", input$p, ", d =", input$d, ", q =", input$q), type = "message")
      add_to_console_history(paste("ARIMA ejecutado manualmente. Parámetros: p =", input$p, "d =", input$d, "q =", input$q))
    })
    
    # Ejecutar Auto ARIMA y mostrar el pronóstico
    observeEvent(input$auto_arima, {
      req(input$time_col, input$obs_col, sheet_data())
      
      data <- sheet_data()
      ts_data <- ts(data[[input$obs_col]])
      
      # División en entrenamiento y prueba según la proporción seleccionada
      n_train <- round(length(ts_data) * input$train_ratio)
      train_data <- window(ts_data, end = n_train)
      test_data <- window(ts_data, start = n_train + 1)
      
      # Ajustar Auto ARIMA
      model <- auto.arima(train_data, stepwise = input$stepwise, seasonal = input$seasonal, approximation = input$approximation, parallel = input$parallel, ic = input$ic)
      forecast_test <- forecast(model, h = length(test_data))
      
      # Modelo Auto ARIMA completo para pronóstico futuro
      model_full <- auto.arima(ts_data, stepwise = input$stepwise, seasonal = input$seasonal, approximation = input$approximation, parallel = input$parallel, ic = input$ic)
      forecast_full <- forecast(model_full, h = input$forecast_periods)
      
      # Guardar el pronóstico para descarga (sin límites de confianza)
      forecast_df_temp <- data.frame(
        Periodo = time(forecast_full$mean), 
        Pronóstico = as.numeric(forecast_full$mean)
      )
      forecast_df(forecast_df_temp)
      
      # Mostrar el orden y los coeficientes del modelo Auto ARIMA
      output$error_metrics <- renderPrint({
        order <- arimaorder(model_full)
        coefs <- coef(model_full)
        list(
          Orden = order,
          Coeficientes = coefs,
          AIC = AIC(model_full),
          BIC = BIC(model_full)
        )
      })
      
      # Mostrar el pronóstico en tabla
      output$data <- renderTable({
        forecast_df()
      })
      
      # Gráfico de pronóstico futuro
      output$forecast_plot <- renderPlotly({
        original_data <- data.frame(Time = 1:length(ts_data), Value = as.numeric(ts_data), Tipo = "Original")
        forecast_data <- data.frame(Time = (length(ts_data) + 1):(length(ts_data) + input$forecast_periods), 
                                    Value = as.numeric(forecast_full$mean), Tipo = "Pronóstico")
        combined_data <- rbind(original_data, forecast_data)
        
        # Obtener los parámetros del modelo auto.arima
        model_params <- arimaorder(model_full)
        
        p <- ggplot(combined_data, aes(x = Time, y = Value, color = Tipo)) +
          geom_line() +
          ggtitle(paste("Pronóstico Auto ARIMA (", model_params[1], ",", model_params[2], ",", model_params[3], ") usando todos los datos")) +
          xlab("Tiempo") + ylab("Valor")
        
        ggplotly(p)
      })
      
      # Calcular errores en el conjunto de prueba
      output$error_table <- renderTable({
        actual <- test_data
        predicted <- forecast_test$mean
        
        # Cálculo de errores
        ErrorAbsoluto <- abs(actual - predicted)
        ErrorCuadrado <- ErrorAbsoluto^2
        ProporcionError <- ErrorAbsoluto / actual
        Sesgo <- mean(predicted - actual, na.rm = TRUE)
        
        MAD <- mean(ErrorAbsoluto, na.rm = TRUE)
        MAPE <- mean(ProporcionError, na.rm = TRUE)
        MSE <- mean(ErrorCuadrado, na.rm = TRUE)
        RMSE <- sqrt(MSE)
        
        data.frame(
          Error = c("MAPE", "MAD", "MSE", "RMSE", "Sesgo (Bias)"),
          Valor = c(paste0(round(MAPE * 100, 2), "%"), round(MAD, 2), round(MSE, 2), round(RMSE, 2), round(Sesgo, 2))
        )
      })
      
      # Gráfico de pronóstico vs datos reales
      output$error_forecast_plot <- renderPlotly({
        df <- data.frame(
          Time = (n_train + 1):length(ts_data),
          Real = test_data,
          Predicho = forecast_test$mean
        )
        
        p <- ggplot(df, aes(x = Time)) +
          geom_line(aes(y = Real, color = "Real")) +
          geom_line(aes(y = Predicho, color = "Predicho")) +
          ggtitle("Pronóstico vs Datos Reales (Conjunto de Prueba)") +
          xlab("Tiempo") + ylab("Valor")
        
        ggplotly(p)
      })
      
      # Cambiar a la pestaña "Pronóstico" utilizando el valor asignado
      updateTabsetPanel(session, "tabset", selected = "pronostico")
      
      showNotification(paste("Auto ARIMA ejecutado correctamente con las siguientes configuraciones:",
                             "Criterio de Información =", input$ic,
                             ", Estacionalidad =", input$seasonal,
                             ", Búsqueda Paso a Paso =", input$stepwise,
                             ", Aproximación =", input$approximation,
                             ", Ejecución Paralela =", input$parallel), type = "message")
      add_to_console_history(paste("Auto ARIMA ejecutado con criterio", input$ic, "y estacionalidad =", input$seasonal))
    })
    
    # Descargar el pronóstico en Excel
    output$download_forecast <- downloadHandler(
      filename = function() {
        paste("pronostico_arima_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        write_xlsx(forecast_df(), path = file)
      }
    )
    
    # Descargar los datos de errores en Excel
    output$download_error_data <- downloadHandler(
      filename = function() {
        paste("datos_errores_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        error_data <- output$error_table()
        write_xlsx(error_data, path = file)
      }
    )
    
    # Análisis de residuos
    observe({
      residuos <- reactiveVal()
      
      observeEvent(input$run_arima, {
        req(input$time_col, input$obs_col, sheet_data())
        data <- sheet_data()
        ts_data <- ts(data[[input$obs_col]])
        model_full <- Arima(ts_data, order = c(input$p, input$d, input$q))
        residuos(residuals(model_full))
      })
      
      observeEvent(input$auto_arima, {
        req(input$time_col, input$obs_col, sheet_data())
        data <- sheet_data()
        ts_data <- ts(data[[input$obs_col]])
        model_full <- auto.arima(ts_data, stepwise = input$stepwise, seasonal = input$seasonal, approximation = input$approximation, parallel = input$parallel, ic = input$ic)
        residuos(residuals(model_full))
      })
      
      # Histograma de residuos
      output$hist_residuals <- renderPlotly({
        req(residuos())
        hist_data <- hist(residuos(), probability = TRUE, plot = FALSE)
        p <- ggplot(data.frame(x = hist_data$mids, y = hist_data$density), aes(x = x, y = y)) +
          geom_bar(stat = "identity", fill = "lightblue") +
          stat_function(fun = dnorm, args = list(mean = 0, sd = sd(residuos())), color = "darkred", size = 1) +
          ggtitle("Histograma de Residuos") +
          xlab("Residuos") + ylab("Densidad")
        
        ggplotly(p)
      })
      
      # Prueba de normalidad
      output$normality_test <- renderPrint({
        req(residuos())
        if (length(residuos()) < 20) {
          shapiro.test(residuos())
        } else {
          jarque.bera.test(residuos())
        }
      })
      
      # Gráfico ACF de residuos
      output$residual_acf <- renderPlotly({
        req(residuos())
        acf_data <- acf(residuos(), plot = FALSE)
        n <- length(residuos())
        conf_limit <- 1.96 / sqrt(n)
        p <- ggplot(data.frame(lag = acf_data$lag[-1], acf = acf_data$acf[-1]), aes(x = lag, y = acf)) +
          geom_bar(stat = "identity", fill = "lightblue") +
          geom_hline(yintercept = conf_limit, linetype = "dashed", color = "red") +
          geom_hline(yintercept = -conf_limit, linetype = "dashed", color = "red") +
          ggtitle("ACF de los Residuos") +
          xlab("Lag") + ylab("ACF")
        
        ggplotly(p)
      })
      
      # Test de Ljung-Box para autocorrelación
      output$ljung_box_test <- renderPrint({
        req(residuos())
        Box.test(residuos(), type = "Ljung-Box")
      })
      
      # Gráfico de residuos por observación
      output$residual_time_plot <- renderPlotly({
        req(residuos())
        p <- ggplot(data.frame(Observacion = 1:length(residuos()), Residuos = residuos()), aes(x = Observacion, y = Residuos)) +
          geom_line(color = "tomato") +
          geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
          ggtitle("Residuos por Observación") +
          xlab("Observación") + ylab("Residuos")
        
        ggplotly(p)
      })
    })
    
    # Historial de ejecuciones
    output$console_history <- renderTable({
      console_history()
    })
    
    # Función para agregar eventos al historial
    add_to_console_history <- function(accion) {
      hora_actual <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      nuevo_evento <- data.frame(Hora = hora_actual, Acción = accion)
      console_history(rbind(console_history(), nuevo_evento))
    }
    
    # Mostrar/Ocultar opciones de Auto ARIMA
    observeEvent(input$toggle_auto_arima, {
      toggle("auto_arima_options")
    })
  })
}
# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)

