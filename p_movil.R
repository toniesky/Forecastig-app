suppressPackageStartupMessages({
  library(shiny)
  library(readxl)
  library(ggplot2)
  library(TTR)
  library(tseries)  # Para adf.test
  library(shinyjs)
  library(plotly)
  library(DT)       # Para tablas interactivas
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
      }
      a {
        color: #1e90ff;
        text-decoration: none;
      }
      a:hover {
        text-decoration: underline;
      }
    "))
  ),
  
  div(class = "title-panel", 
      titlePanel("Modelo de Promedio Móvil Simple")
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      fileInput("file", "Seleccionar archivo Excel", accept = c(".xlsx")),
      uiOutput("sheet_ui"),
      uiOutput("time_col_ui"),
      uiOutput("value_col_ui"),
      numericInput("n", "Ingrese el valor de n para el promedio móvil", value = 5, min = 1, step = 1),
      actionButton("run_model", "Ejecutar Modelo", icon = icon("play"), class = "btn-primary")
    ),
    
    mainPanel(
      class = "main-panel",
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
      plotlyOutput("plot"),
      DT::dataTableOutput("error_report")  # Utilizar DT para una tabla interactiva
    )
  ),
  
  div(class = "footer",
      HTML("by <a href='https://www.youtube.com/@Tonyesky' target='_blank'>Toñesky en YouTube</a>")
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
    Errores <- c(round(Bias, 2), paste0(round(MAPE * 100, 2), "%"), round(MAD, 2), round(MSE, 2), round(RMSE, 2))
    
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
  output$sheet_ui <- renderUI({
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
    
    # Capturar 'n' de forma aislada para evitar dependencias reactivas
    n <- isolate(as.numeric(input$n))
    req(!is.na(n) && n > 0)  # Asegurar que n es un número positivo
    
    df <- data_reactive()
    
    # Validar la columna de tiempo
    if (!is.numeric(df[[input$time_col]]) && !inherits(df[[input$time_col]], "Date") && !inherits(df[[input$time_col]], "POSIXct")) {
      showNotification("La columna de tiempo debe ser numérica o de tipo fecha.", type = "error")
      return(NULL)
    }
    
    # Crear la serie temporal
    # Detectar si la columna de tiempo es de tipo fecha para establecer el inicio y frecuencia
    if (inherits(df[[input$time_col]], "Date") || inherits(df[[input$time_col]], "POSIXct")) {
      # Intentar detectar la frecuencia mensual, anual, etc.
      # Esto puede requerir una lógica más compleja dependiendo de los datos
      # Por simplicidad, asumiremos frecuencia anual
      start_year <- as.numeric(format(min(df[[input$time_col]], na.rm = TRUE), "%Y"))
      start_period <- as.numeric(format(min(df[[input$time_col]], na.rm = TRUE), "%m"))
      serie <- ts(df[[input$value_col]], start = c(start_year, start_period), frequency = 12)
    } else {
      # Si la columna de tiempo es numérica, establecer frecuencia mensual por defecto
      serie <- ts(df[[input$value_col]], start = c(2012, 1), frequency = 12)
    }
    
    # Calcular el promedio móvil
    promedio_movil <- SMA(df[[input$value_col]], n = n)
    df$promedio_movil <- promedio_movil
    
    # Realizar el test de Dickey-Fuller
    adf_result <- tryCatch({
      adf.test(serie)
    }, error = function(e) {
      showNotification("Error al realizar el test de Dickey-Fuller: Verifica los datos de la serie temporal.", type = "error")
      return(NULL)
    })
    
    list(promedio_movil = promedio_movil, adf_result = adf_result, df = df, n = n)
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
    if (is.null(adf_result)) {
      cat("No se pudo realizar el test de Dickey-Fuller.")
    } else {
      print(adf_result)
    }
  })
  
  # Forecasting
  output$forecast <- renderPrint({
    req(model_data())
    df <- model_data()$df
    n <- model_data()$n
    
    # Asegurarse de que hay suficientes datos para el pronóstico
    if (n > nrow(df)) {
      cat("El valor de n es mayor que el número de observaciones disponibles.")
      return(NULL)
    }
    
    valores_recientes <- tail(df[[input$value_col]], n)
    pronostico <- mean(valores_recientes, na.rm = TRUE)
    
    cat("Pronóstico para el siguiente período (promedio de los últimos", n, "valores):", round(pronostico, 2))
  })
  
  # Plotting
  output$plot <- renderPlotly({
    req(model_data())
    df <- model_data()$df
    promedio_movil <- model_data()$promedio_movil
    n <- model_data()$n
    
    # Determinar el eje x
    if (inherits(df[[input$time_col]], "Date") || inherits(df[[input$time_col]], "POSIXct")) {
      eje_x <- df[[input$time_col]]
    } else {
      eje_x <- seq_along(df[[input$value_col]])
    }
    
    plot_ly() %>% 
      add_lines(x = eje_x, y = df[[input$value_col]], name = 'Serie original', line = list(color = 'blue')) %>% 
      add_lines(x = eje_x, y = promedio_movil, name = 'Promedio móvil', line = list(color = 'red', dash = 'dash'), na.rm = TRUE) %>% 
      layout(title = "Comparación de la Serie Original y el Promedio Móvil",
             xaxis = list(title = "Tiempo"),
             yaxis = list(title = "Valores"),
             legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2))
  })
  
  # Error report
  output$error_report <- DT::renderDataTable({
    req(model_data())
    df <- model_data()$df
    promedio_movil <- model_data()$promedio_movil
    n <- model_data()$n
    
    # Asegurarse de que hay suficientes datos para calcular errores
    if (n > length(promedio_movil) || n > length(df[[input$value_col]])) {
      return(NULL)
    }
    
    real <- tail(df[[input$value_col]], n)  # Valores reales
    pronosticado <- tail(promedio_movil, n)  # Valores pronosticados
    
    # Manejar posibles NA en pronosticado
    pronosticado <- ifelse(is.na(pronosticado), 0, pronosticado)
    
    df_errores <- errores(pronosticado, real)
    
    DT::datatable(df_errores, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })
}

# Ejecutar la aplicación Shiny en un puerto específico
shinyApp(ui = ui, server = server, options = list(port = 1240))
