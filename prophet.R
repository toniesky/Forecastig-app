# Carga de librerías
suppressPackageStartupMessages({
  library(shiny)
  library(readxl)
  library(prophet)
  library(plotly)
  library(shinyjs)
  library(writexl)
  library(ggplot2)
  library(dplyr)     # Para manipulación de datos
  library(lubridate) # Para manejo de fechas
  library(data.table)# Para manipulación eficiente de datos
})

options(shiny.launch.browser = function(url) {
  # browseURL(url)  # Abre la URL en el navegador predeterminado
}) 
# Interfaz de usuario (UI)
ui <- fluidPage(
  useShinyjs(),  # Para habilitar o deshabilitar elementos
  
  # Estilos personalizados
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
      /* Estilo para los botones de acción */
      .action-buttons .btn {
        margin-bottom: 10px;
        width: 100%;
      }
      /* Centrar tablas */
      .table-container {
        display: flex;
        justify-content: center;
      }
      .table-container table {
        width: 80%;
      }
      /* Estilo para el botón de descarga en Tabla de Pronósticos */
      .download-button {
        margin-bottom: 20px;
        margin-top: 10px; /* Desplaza el botón un poco más abajo */
        text-align: center; /* Centra el botón */
      }
      /* Estilo para el botón de ejecutar modelo */
      #run_model {
        background-color: #1e90ff;
        color: white;
      }
      #run_model:hover {
        background-color: #1c86ee;
        color: white;
      }
    "))
  ),
  
  # Título de la aplicación
  div(class = "title-panel", 
      titlePanel("Aplicación de Pronóstico con Prophet")
  ),
  
  # Diseño principal con panel lateral y panel principal
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      width = 3,
      h3("Carga de Datos"),
      fileInput("file_input", "Cargar archivo Excel o CSV", accept = c(".xlsx", ".csv")),
      uiOutput("sheet_selector"),
      uiOutput("column_time"),
      uiOutput("column_obs"),  # Selección de columna de observaciones
      hr(),
      h3("Configuración del Modelo"),
      checkboxInput("use_custom_params", "Usar parámetros personalizados", FALSE),
      conditionalPanel(
        condition = "input.use_custom_params == true",
        h4("Parámetros de Prophet"),
        numericInput("changepoint_scale", "Changepoint Scale", value = 0.05, min = 0, max = 1),
        h4("Tipos de Estacionalidad"),
        checkboxInput("yearly_seasonality", "Estacionalidad Anual", TRUE),
        checkboxInput("weekly_seasonality", "Estacionalidad Semanal", TRUE),
        checkboxInput("daily_seasonality", "Estacionalidad Diaria", FALSE)
      ),
      hr(),
      numericInput("forecast_periods", "Periodos a predecir", value = 12, min = 1),
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
        tabPanel(
          title = tagList(icon("chart-line"), "Serie de Tiempo"),
          value = "serie_tiempo",
          plotlyOutput("historical_plot")
        ),
        tabPanel(
          title = tagList(icon("chart-bar"), "Pronóstico"),
          value = "pronostico",
          plotlyOutput("forecast_comparison_plot"),
          br(),
          plotlyOutput("forecast_future_plot", height = "400px"),
          hr(),
          div(class = "download-button",
              downloadButton("download_forecast", "Descargar Pronóstico", icon = icon("download"))
          )
        ),
        tabPanel(
          title = tagList(icon("exclamation-triangle"), "Errores"),
          value = "errores",
          fluidRow(
            column(12, plotlyOutput("error_forecast_plot"))
          ),
          # Botón de descarga ubicado entre el gráfico y la tabla
          div(class = "download-button",
              downloadButton("download_error_data", "Descargar Valores", icon = icon("download"))
          ),
          hr(),
          fluidRow(
            column(12, tableOutput("error_table"))
          )
        ),
        tabPanel(
          title = tagList(icon("history"), "Historial"),
          value = "historial",
          h3("Historial de la Consola - IP y Usuario"),
          uiOutput("ip_address"),
          tableOutput("history_table")
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
    # Inicializar shinyjs: deshabilitar el botón de descarga al inicio
    shinyjs::disable("download_forecast")
    
    # Variables reactivas para almacenar datos y promociones
    data_reactive <- reactiveVal(NULL)
    
    # Inicializar reactiveValues para forecast_train y test_data
    model_data <- reactiveValues(
      forecast_train = NULL,
      test_data = NULL
    )
    
    history_reactive <- reactiveVal(data.frame(
      Fecha = character(),
      Hora = character(),
      IP = character(),
      `Changepoint Scale` = numeric(),
      Periodos = numeric(),
      Detalles = character(),
      stringsAsFactors = FALSE
    ))
    
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
    
    # Cargar datos y seleccionar hoja
    observeEvent(input$file_input, {
      req(input$file_input)
      ext <- tools::file_ext(input$file_input$name)
      if (ext == "xlsx") {
        output$sheet_selector <- renderUI({
          sheets <- excel_sheets(input$file_input$datapath)
          selectInput("sheet", "Seleccionar Hoja", choices = sheets, selected = NULL)
        })
      } else {
        output$sheet_selector <- renderUI(NULL)
      }
      
      # Reiniciar las selecciones de columnas al cargar un nuevo archivo
      output$column_time <- renderUI({
        selectInput("time_col", "Columna de Fecha", choices = NULL, selected = NULL)
      })
      output$column_obs <- renderUI({
        selectInput("value_col", "Columna de Valores", choices = NULL, selected = NULL)
      })
    })
    
    # Cargar datos automáticamente al seleccionar hoja o archivo CSV
    observeEvent({
      input$file_input
      input$sheet
    }, {
      req(input$file_input)
      ext <- tools::file_ext(input$file_input$name)
      data <- tryCatch({
        if (ext == "xlsx") {
          req(input$sheet)
          read_excel(input$file_input$datapath, sheet = input$sheet)
        } else {
          read.csv(input$file_input$datapath)
        }
      }, error = function(e) {
        showNotification("Error al leer el archivo.", type = "error")
        return(NULL)
      })
      
      if (!is.null(data)) {
        data_reactive(data)
        output$column_time <- renderUI({
          columns <- names(data)
          selectInput("time_col", "Columna de Fecha", choices = columns, selected = NULL)
        })
        output$column_obs <- renderUI({  # Renderizar selección de columna de observaciones
          columns <- names(data)
          selectInput("value_col", "Columna de Valores", choices = columns, selected = NULL)
        })
      }
    })
    
    # Activar botón "Ejecutar Modelo" solo cuando las columnas estén seleccionadas
    observe({
      if (!is.null(input$time_col) && !is.null(input$value_col) && input$time_col != "" && input$value_col != "") {
        shinyjs::enable("run_model")
      } else {
        shinyjs::disable("run_model")
      }
    })
    
    # Mostrar el gráfico de la serie de tiempo al seleccionar las columnas
    observe({
      req(input$time_col, input$value_col)
      req(data_reactive())
      
      df <- data_reactive()[, c(input$time_col, input$value_col)]
      
      # Verificar si las columnas seleccionadas no están vacías
      if (all(!is.na(df[[input$time_col]])) && all(!is.na(df[[input$value_col]]))) {
        df_plot <- data.frame(
          ds = as.Date(df[[input$time_col]]),
          y = as.numeric(df[[input$value_col]])
        )
        
        output$historical_plot <- renderPlotly({
          plot_ly() %>%
            add_lines(x = df_plot$ds, y = df_plot$y, name = 'Datos Históricos', line = list(color = 'blue')) %>% 
            layout(title = "Serie de Tiempo Histórica", xaxis = list(title = "Fecha"), yaxis = list(title = "Valor"))
        })
      } else {
        output$historical_plot <- renderPlotly({
          plotly_empty(type = "scatter", mode = "lines") %>% 
            layout(title = "Serie de Tiempo Histórica", xaxis = list(title = "Fecha"), yaxis = list(title = "Valor"))
        })
      }
    })
    
    # Ejecutar modelo Prophet al hacer clic en "Ejecutar Modelo"
    observeEvent(input$run_model, {
      req(input$time_col, input$value_col)
      req(data_reactive())
      
      # Mostrar barra de carga
      showModal(modalDialog(
        title = "Procesando",
        "Ejecutando el modelo, por favor espera...",
        footer = NULL
      ))
      
      withProgress(message = 'Ejecutando el modelo...', value = 0, {
        incProgress(0.2)
        
        df <- data_reactive()[, c(input$time_col, input$value_col)]
        df <- df[!is.na(df[[input$time_col]]), ]  # Eliminar filas con fechas NA
        df <- df[!is.na(df[[input$value_col]]), ]  # Eliminar filas con valores NA
        colnames(df) <- c("ds", "y")
        df$ds <- as.Date(df$ds)
        df$y <- as.numeric(df$y)
        
        # División en entrenamiento y prueba según la proporción seleccionada
        split_index <- round(nrow(df) * input$train_ratio)  # Usar la proporción seleccionada por el usuario
        train_data <- df[1:split_index, ]
        test_data <- df[(split_index + 1):nrow(df), ]
        
        # Almacenar test_data en reactiveValues
        model_data$test_data <- test_data
        
        incProgress(0.3)
        
        # Crear y ajustar el modelo Prophet para el pronóstico original
        model <- prophet(
          changepoint.prior.scale = ifelse(input$use_custom_params, input$changepoint_scale, 0.05),
          yearly.seasonality = ifelse(input$use_custom_params, input$yearly_seasonality, TRUE),
          weekly.seasonality = ifelse(input$use_custom_params, input$weekly_seasonality, TRUE),
          daily.seasonality = ifelse(input$use_custom_params, input$daily_seasonality, FALSE)
        )
        model <- fit.prophet(model, df)
        
        # Crear dataframe futuro y predecir para el pronóstico original
        future <- make_future_dataframe(model, periods = input$forecast_periods)
        forecast <- predict(model, future)
        
        incProgress(0.2)
        
        # Gráfico de comparación del pronóstico
        output$forecast_comparison_plot <- renderPlotly({
          future_forecasts <- forecast[forecast$ds > max(df$ds), ]
          plot_ly() %>% 
            add_lines(x = df$ds, y = df$y, name = 'Datos Históricos', line = list(color = 'blue')) %>% 
            add_lines(x = future_forecasts$ds, y = future_forecasts$yhat, name = 'Pronóstico Futuro', line = list(color = 'red')) %>% 
            layout(title = "Comparación del Pronóstico", xaxis = list(title = "Fecha"), yaxis = list(title = "Valor"))
        })
        
        # Gráfico del pronóstico futuro
        output$forecast_future_plot <- renderPlotly({
          future_forecasts <- forecast[forecast$ds > max(df$ds), ]
          plot_ly() %>% 
            add_lines(x = future_forecasts$ds, y = future_forecasts$yhat, name = 'Pronóstico Futuro', line = list(color = 'green')) %>% 
            layout(title = "Pronóstico Futuro", xaxis = list(title = "Fecha"), yaxis = list(title = "Valor"))
        })
        
        # Tabla de pronósticos
        output$forecast_table <- renderTable({
          future_forecasts <- forecast[forecast$ds > max(df$ds), c("ds", "yhat")]
          future_forecasts$ds <- format(as.Date(future_forecasts$ds), "%Y-%m-%d")
          names(future_forecasts) <- c("Fecha", "Pronóstico")
          future_forecasts
        })
        
        # Cálculo de errores
        # Crear y ajustar el modelo Prophet solo con los datos de entrenamiento
        model_train <- prophet(
          changepoint.prior.scale = ifelse(input$use_custom_params, input$changepoint_scale, 0.05)
        )
        model_train <- fit.prophet(model_train, train_data)
        
        # Crear dataframe futuro y predecir solo para el conjunto de prueba
        future_train <- make_future_dataframe(model_train, periods = nrow(test_data))
        forecast_train <- predict(model_train, future_train)
        
        # Almacenar forecast_train en reactiveValues
        model_data$forecast_train <- forecast_train
        
        incProgress(0.2)
        
        # Tabla de errores
        output$error_table <- renderTable({
          req(model_data$forecast_train, model_data$test_data)
          
          actual <- model_data$test_data$y
          predicted <- model_data$forecast_train$yhat[(nrow(model_train$history) + 1):(nrow(model_train$history) + nrow(model_data$test_data))]
          # Asegurarse de que la longitud de predicted coincida con actual
          predicted <- predicted[1:length(actual)]
          
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
          req(model_data$forecast_train, model_data$test_data)
          
          # Extraer las predicciones correspondientes al conjunto de prueba
          predicted <- model_data$forecast_train$yhat[(nrow(model_train$history) + 1):(nrow(model_train$history) + nrow(model_data$test_data))]
          # Asegurarse de que la longitud de predicted coincida con actual
          predicted <- predicted[1:nrow(model_data$test_data)]
          
          df_plot <- data.frame(
            Time = model_data$test_data$ds,
            Real = model_data$test_data$y,
            Predicho = predicted
          )
          
          p <- ggplot(df_plot, aes(x = Time)) +
            geom_line(aes(y = Real, color = "Real")) +
            geom_line(aes(y = Predicho, color = "Predicho")) +
            ggtitle("Pronóstico vs Datos Reales (Conjunto de Prueba)") +
            xlab("Tiempo") + ylab("Valor") +
            scale_color_manual(values = c("Real" = "blue", "Predicho" = "red")) +
            theme_minimal()
          
          ggplotly(p) %>% 
            layout(legend = list(title = list(text = "Serie")))
        })
        
        # Actualizar historial
        history <- rbind(history_reactive(), data.frame(
          Fecha = format(Sys.Date(), "%Y-%m-%d"),
          Hora = format(Sys.time(), "%H:%M:%S"),
          IP = ip_address(),
          `Changepoint Scale` = ifelse(input$use_custom_params, input$changepoint_scale, 0.05),
          Periodos = input$forecast_periods,
          Detalles = paste("Modelo ejecutado con", input$forecast_periods, "periodos de predicción")
        ))
        history_reactive(history)
        
        output$history_table <- renderTable({
          history_reactive()
        })
        
        # Habilitar botón de descarga
        shinyjs::enable("download_forecast")
        
        # Descargar pronóstico
        output$download_forecast <- downloadHandler(
          filename = function() {
            paste0("pronostico_prophet_", Sys.Date(), ".xlsx")
          },
          content = function(file) {
            write_xlsx(model_data$forecast_train, path = file)
          }
        )
        
        # Descargar datos de errores
        output$download_error_data <- downloadHandler(
          filename = function() {
            paste0("datos_errores_", Sys.Date(), ".xlsx")
          },
          content = function(file) {
            req(model_data$forecast_train, model_data$test_data)
            
            # Extraer las predicciones correspondientes al conjunto de prueba
            predicted <- model_data$forecast_train$yhat[(nrow(model_train$history) + 1):(nrow(model_train$history) + nrow(model_data$test_data))]
            # Asegurarse de que la longitud de predicted coincida con actual
            predicted <- predicted[1:nrow(model_data$test_data)]
            
            df_error <- data.frame(
              Real = model_data$test_data$y,
              Predicho = predicted
            )
            
            write_xlsx(df_error, path = file)
          }
        )
        
        # Cambiar a la pestaña "Pronóstico" después de ejecutar el modelo
        updateTabsetPanel(session, "tabset", selected = "pronostico")
        
        # Cerrar la barra de carga
        removeModal()
      })
    })
  })
}

# Ejecutar la aplicación Shiny en un puerto específico
shinyApp(ui = ui, server = server, options = list(port = 1239))
