# Carga de librerías 
suppressPackageStartupMessages({
  library(shiny)
  library(readxl)
  library(writexl)       # Para escribir archivos Excel
  library(shinyjs)
  library(ggplot2)
  library(plotly)
  library(data.table)    # Para manejo eficiente de datos
  library(MASS)
  library(car)
  library(lmtest)
  library(tseries)
  library(PerformanceAnalytics)
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
      titlePanel("Aplicación de Modelos de Regresión")
  ),
  
  # Diseño principal con panel lateral y panel principal
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      width = 3,  # Ajustar el ancho del panel lateral
      # Sección de carga de datos
      h3("Carga de Datos"),
      fileInput("datafile", "Sube un archivo Excel o CSV", accept = c(".xlsx", ".csv")),
      uiOutput("sheet_ui"),
      actionButton("load_data", "Cargar Datos", icon = icon("upload")),
      hr(),
      h3("Selección de Variables"),
      uiOutput("column_names_ui"),
      # Nueva sección para selección de variable dependiente mediante cuadro de texto
      textInput("y_var", "Variable dependiente (Y) con transformación (ej. log(Y)):", 
                placeholder = "Ejemplo: log(Y)"),
      # Nueva sección para selección de variables independientes mediante cuadro de texto
      textInput("x_var", "Variables independientes (X) con transformaciones (ej. X1 + log(X2)):", 
                placeholder = "Ejemplo: X1 + log(X2)"),
      hr(),
      h3("Estrategias de Selección"),
      radioButtons("selection_strategy", "Selecciona una estrategia:", 
                   choices = list("Personalizado" = "custom",
                                  "Todas las variables" = "all",
                                  "Selección hacia adelante" = "forward",
                                  "Selección hacia atrás" = "backward",
                                  "Selección bidireccional" = "both"),
                   selected = "custom"),
      hr(),
      # Botón de acción
      actionButton("run_model", "Ejecutar Modelo", icon = icon("play"), class = "btn-primary"),
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
        # Pestaña de Resumen del Modelo
        tabPanel(
          title = tagList(icon("clipboard-list"), "Resumen del Modelo"),
          value = "resumen_modelo",
          fluidRow(
            column(12, verbatimTextOutput("model_summary"))
          ),
          hr(),
          fluidRow(
            column(12, verbatimTextOutput("model_metrics"))
          ),
          hr(),
          fluidRow(
            column(12, 
                   # Entrada para el nombre del archivo
                   textInput("download_name", "Nombre del archivo para descargar (sin extensión):", value = "resultados_regresion"),
                   # Botón de descarga y nombre del archivo
                   downloadButton("download_results", "Descargar Resultados en Excel", icon = icon("download")),
                   br(), br(),
                   # Mostrar el nombre del archivo que se descargará
                   textOutput("download_filename"),
                   br(),
                   # Gráfico de comparación de observados y predicciones
                   plotOutput("observed_vs_predicted_plot")
            )
          )
        ),
        # Pestaña de Análisis del Modelo
        tabPanel(
          title = tagList(icon("chart-line"), "Análisis del Modelo"),
          value = "analisis_modelo",
          tabsetPanel(
            id = "model_analysis_tabs",
            tabPanel("Normalidad de Residuos",
                     plotOutput("hist_plot"),
                     verbatimTextOutput("jb_test")
            ),
            tabPanel("Homocedasticidad",
                     plotOutput("residual_plot"),
                     verbatimTextOutput("bp_test")
            ),
            tabPanel("Autocorrelación",
                     plotlyOutput("residual_time_plot"),  # Cambiar a plotlyOutput
                     verbatimTextOutput("dw_test")
            ),
            tabPanel("Multicolinealidad",
                     plotOutput("corr_matrix_plot"),
                     verbatimTextOutput("vif_test")
            ),
            tabPanel("ANOVA",
                     verbatimTextOutput("anova_table")
            )
          )
        ),
        # Pestaña de Historial de Ejecuciones
        tabPanel(
          title = tagList(icon("history"), "Historial"),
          value = "historial",
          h3("Historial de la Consola"),
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
  shinyjs::disable("download_results")
  
  # Variables reactivas para almacenar los datos y el historial
  datos <- reactiveVal(NULL)
  modelo <- reactiveVal(NULL)
  console_history <- reactiveVal(data.frame(Hora = character(0), Acción = character(0)))
  
  # Variables reactivas para almacenar las fórmulas y métricas del modelo
  formula_obj <- reactiveVal(NULL)
  model_metrics <- reactiveVal(NULL)
  stepwise_output <- reactiveVal(NULL)  # Nueva variable reactiva para pasos de stepAIC
  
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
  
  # Cargar los datos al seleccionar un archivo
  observeEvent(input$datafile, {
    req(input$datafile)
    ext <- tools::file_ext(input$datafile$name)
    add_to_console_history(paste("Archivo cargado:", input$datafile$name))
    if (tolower(ext) == "csv") {
      data <- tryCatch({
        read.csv(input$datafile$datapath)
      }, error = function(e) {
        showNotification("Error al leer el archivo CSV.", type = "error")
        return(NULL)
      })
      if (!is.null(data)) {
        datos(data)
        output$sheet_ui <- renderUI(NULL)
        update_ui_elements()
      }
    } else if (tolower(ext) %in% c("xlsx", "xls")) {
      sheets <- excel_sheets(input$datafile$datapath)
      output$sheet_ui <- renderUI({
        selectInput("sheet", "Selecciona una hoja", choices = sheets)
      })
    } else {
      showNotification("Formato de archivo no soportado.", type = "error")
    }
  })
  
  # Leer los datos de la hoja seleccionada
  observeEvent(input$load_data, {
    req(input$datafile)
    if (!is.null(input$sheet)) {
      data <- tryCatch({
        read_excel(input$datafile$datapath, sheet = input$sheet)
      }, error = function(e) {
        showNotification("Error al leer la hoja de Excel.", type = "error")
        return(NULL)
      })
      if (!is.null(data)) {
        datos(data)
        update_ui_elements()
      }
    }
  })
  
  # Función para actualizar los elementos de UI después de cargar datos
  update_ui_elements <- function() {
    output$column_names_ui <- renderUI({
      verbatimTextOutput("column_names")
    })
    output$column_names <- renderPrint({
      names(datos())
    })
  }
  
  # Función para agregar eventos al historial
  add_to_console_history <- function(accion) {
    hora_actual <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    nuevo_evento <- data.frame(Hora = hora_actual, Acción = accion)
    console_history(rbind(console_history(), nuevo_evento))
  }
  
  # Habilitar o deshabilitar el cuadro de texto de variables independientes según la casilla
  observe({
    if (input$selection_strategy == "custom") {
      shinyjs::enable("x_var")
    } else {
      shinyjs::disable("x_var")
    }
  })
  
  # Crear la fórmula basada en la selección del usuario, incluyendo la transformación de Y y X
  create_formula <- reactive({
    req(datos())
    
    # Obtener las entradas de texto
    y_input <- input$y_var
    strategy <- input$selection_strategy
    
    if (strategy == "custom") {
      x_input <- input$x_var
      if (nchar(x_input) == 0) {
        showNotification("Por favor, especifica las variables independientes (X) para la selección personalizada.", type = "error")
        return(NULL)
      }
    } else {
      # Usar todas las variables como independientes, sin transformaciones
      # Extraer el nombre de la variable dependiente sin transformaciones
      y_var_formula <- tryCatch({
        as.formula(paste("~", y_input))
      }, error = function(e) {
        showNotification("Error en la variable dependiente. Revisa las transformaciones.", type = "error")
        return(NULL)
      })
      y_var_names <- all.vars(y_var_formula)
      y_var_name <- tail(y_var_names, 1)  # Tomar la última variable, asumiendo que es la dependiente real
      independent_vars <- setdiff(names(datos()), y_var_name)
      if (length(independent_vars) == 0) {
        showNotification("No hay variables independientes disponibles para la fórmula.", type = "error")
        return(NULL)
      }
      x_input <- paste(independent_vars, collapse = " + ")
    }
    
    formula_str <- paste(y_input, "~", x_input)
    
    # Validar la fórmula
    formula_obj <- tryCatch({
      as.formula(formula_str)
    }, error = function(e) {
      showNotification("Fórmula inválida. Revisa las transformaciones de las variables.", type = "error")
      return(NULL)
    })
    
    return(formula_obj)
  })
  
  # Crear el modelo inicial y completo para stepAIC
  create_step_models <- function() {
    req(datos())
    formula_obj_local <- create_formula()
    req(formula_obj_local)
    
    # Validar que la variable dependiente esté especificada
    if (nchar(input$y_var) == 0) {
      showNotification("Por favor, especifica la variable dependiente (Y).", type = "error")
      return(NULL)
    }
    
    # Validar que todas las variables existan en los datos
    # Extraer el nombre de la variable dependiente correctamente
    y_var_names <- all.vars(formula_obj_local[[2]])
    y_var_name <- tail(y_var_names, 1)
    independent_vars <- setdiff(names(datos()), y_var_name)
    if (length(independent_vars) == 0) {
      showNotification("No hay variables independientes disponibles para la fórmula.", type = "error")
      return(NULL)
    }
    
    # Formatear las variables independientes
    x_input <- paste(independent_vars, collapse = " + ")
    
    # Construir la fórmula completa sin transformaciones
    full_formula_str <- paste(y_var_name, "~", x_input)
    full_formula <- tryCatch({
      as.formula(full_formula_str)
    }, error = function(e) {
      showNotification("Error en la fórmula completa. Revisa las transformaciones de las variables.", type = "error")
      return(NULL)
    })
    
    if (is.null(full_formula)) return(NULL)
    
    # Definir el modelo inicial (intercepto)
    initial_formula <- as.formula(paste(y_var_name, "~ 1"))
    modelo_inicial <- lm(initial_formula, data = datos())
    
    # Definir el modelo completo con todas las variables independientes
    modelo_completo <- lm(full_formula, data = datos())
    
    return(list(initial = modelo_inicial, full = modelo_completo))
  }
  
  # Ejecución del modelo
  observeEvent(input$run_model, {
    req(datos())
    formula_obj_local <- create_formula()
    req(formula_obj_local)
    
    # Validar que la variable dependiente esté especificada
    if (nchar(input$y_var) == 0) {
      showNotification("Por favor, especifica la variable dependiente (Y).", type = "error")
      return(NULL)
    }
    
    # Validar que todas las variables existan en los datos
    variables <- all.vars(formula_obj_local)
    variables_raw <- unique(gsub(".*\\(([^()]+)\\).*", "\\1", variables))
    variables_raw <- unique(c(variables_raw, variables[!grepl(".*\\(.*\\).*", variables)]))
    missing_vars <- variables_raw[!(variables_raw %in% names(datos()))]
    if (length(missing_vars) > 0) {
      showNotification(paste("Las siguientes variables no existen en los datos:", 
                             paste(missing_vars, collapse = ", ")), type = "error")
      return(NULL)
    }
    
    # Ajustar el modelo según la estrategia seleccionada
    strategy <- input$selection_strategy
    stepwise_steps <- NULL  # Variable temporal para capturar pasos
    if (strategy == "all") {
      # Usar la fórmula completa directamente
      modelo(lm(formula_obj_local, data = datos()))
      showNotification("Modelo con todas las variables ejecutado correctamente.", type = "message")
      add_to_console_history("Modelo con todas las variables ejecutado.")
    } else if (strategy == "custom") {
      # Usar la fórmula personalizada proporcionada por el usuario
      modelo(lm(formula_obj_local, data = datos()))
      showNotification("Modelo personalizado ejecutado correctamente.", type = "message")
      add_to_console_history("Modelo personalizado ejecutado.")
    } else {
      # Estrategias de selección: forward, backward, both
      models <- create_step_models()
      req(models)
      if (strategy == "forward") {
        stepwise_steps <- capture.output({
          stepwise_model <- stepAIC(models$initial, 
                                    scope = list(lower = models$initial, upper = models$full), 
                                    direction = "forward", 
                                    trace = TRUE)
        })
        showNotification("Modelo con selección hacia adelante ejecutado correctamente.", type = "message")
        add_to_console_history("Modelo con selección hacia adelante ejecutado.")
      } else if (strategy == "backward") {
        stepwise_steps <- capture.output({
          stepwise_model <- stepAIC(models$full, 
                                    direction = "backward", 
                                    trace = TRUE)
        })
        showNotification("Modelo con selección hacia atrás ejecutado correctamente.", type = "message")
        add_to_console_history("Modelo con selección hacia atrás ejecutado.")
      } else if (strategy == "both") {
        stepwise_steps <- capture.output({
          stepwise_model <- stepAIC(models$initial, 
                                    scope = list(lower = models$initial, upper = models$full), 
                                    direction = "both", 
                                    trace = TRUE)
        })
        showNotification("Modelo con selección bidireccional ejecutado correctamente.", type = "message")
        add_to_console_history("Modelo con selección bidireccional ejecutado.")
      }
      if (!is.null(stepwise_steps)) {
        modelo(stepwise_model)
        stepwise_output(paste(stepwise_steps, collapse = "\n"))  # Guardar los pasos
      }
    }
    
    # Calcular métricas adicionales solo si se ha ejecutado un modelo
    if (!is.null(modelo())) {
      predicciones <- predict(modelo())
      y_var_name <- all.vars(formula_obj_local)[1]
      observados <- datos()[[y_var_name]]
      
      # Calcular métricas
      bias_val <- mean(predicciones - observados)
      mse_val <- mean((predicciones - observados)^2)
      rmse_val <- sqrt(mse_val)
      mae_val <- mean(abs(predicciones - observados))
      mape_val <- mean(abs((predicciones - observados) / observados)) * 100
      aic_val <- AIC(modelo())
      bic_val <- BIC(modelo())
      
      # Guardar las métricas en una variable reactiva
      model_metrics(list(
        bias = bias_val,
        mse = mse_val,
        rmse = rmse_val,
        mae = mae_val,
        mape = mape_val,
        aic = aic_val,
        bic = bic_val
      ))
      
      # Guardar la fórmula en una variable reactiva
      formula_obj(formula_obj_local)
      
      shinyjs::enable("download_results")
      updateTabsetPanel(session, "tabset", selected = "resumen_modelo")
    }
  })
  
  # Resumen del modelo
  output$model_summary <- renderPrint({
    req(modelo())
    summary(modelo())
  })
  
  # Mostrar la fórmula del modelo
  output$model_formula <- renderPrint({
    req(formula_obj())
    cat("Fórmula del Modelo:\n")
    print(formula_obj())
  })
  
  # Mostrar los pasos de selección de variables
  output$stepwise_steps <- renderPrint({
    req(stepwise_output())
    cat("Pasos de Selección de Variables (stepAIC):\n")
    cat(stepwise_output())
  })
  
  # Métricas adicionales de ajuste
  output$model_metrics <- renderPrint({
    req(model_metrics())
    metrics <- model_metrics()
    cat("Sesgo (Bias):", metrics$bias, "\n")
    cat("MSE:", metrics$mse, "\n")
    cat("RMSE:", metrics$rmse, "\n")
    cat("MAE:", metrics$mae, "\n")
    cat("MAPE:", metrics$mape, "%\n")
    cat("AIC:", metrics$aic, "\n")
    cat("BIC:", metrics$bic, "\n")
  })
  
  # Análisis de multicolinealidad
  output$corr_matrix_plot <- renderPlot({
    req(modelo())
    independents <- names(coef(modelo()))[-1]  # Variables en el modelo actual
    # Verificar el número de variables independientes
    if (length(independents) < 2) {
      plot.new()
      text(0.5, 0.5, "Se requieren al menos 2 variables independientes para el análisis de multicolinealidad.", cex = 1.5)
    } else {
      datos_filtrados <- datos()[, independents, drop = FALSE]
      chart.Correlation(datos_filtrados, histogram = TRUE, pch = 19)
    }
  })
  
  output$vif_test <- renderPrint({
    req(modelo())
    independents <- names(coef(modelo()))[-1]  # Excluye el intercepto
    # Verificar si hay suficientes variables independientes
    if (length(independents) < 2) {
      cat("VIF no disponible. Se requieren al menos 2 variables independientes para el análisis de multicolinealidad.")
    } else {
      cat("Factor de inflación de la varianza (VIF):\n")
      vif_values <- vif(modelo())
      print(vif_values)
    }
  })
  
  output$residual_time_plot <- renderPlotly({
    req(modelo())
    plot_ly(x = ~seq_along(residuals(modelo())), y = ~residuals(modelo()), type = 'scatter', mode = 'lines',
            line = list(color = 'tomato')) %>%
      layout(title = "Residuos por Observación",
             xaxis = list(title = "Observación"),
             yaxis = list(title = "Residuo"),
             shapes = list(
               list(type = "line", 
                    x0 = 0, x1 = length(residuals(modelo())), 
                    y0 = 0, y1 = 0, 
                    line = list(color = "blue", dash = "dash"))
             ))
  })
  
  output$hist_plot <- renderPlot({
    req(modelo())
    residuos <- residuals(modelo())
    hist(residuos, probability = TRUE, main = "Histograma de Residuos",
         xlab = "Residuos", ylab = "Densidad", col = "lightblue")
    curve(dnorm(x, mean = mean(residuos), sd = sd(residuos)),
          col = "darkred", lwd = 2, add = TRUE)
  })
  
  output$jb_test <- renderPrint({
    req(modelo())
    test_result <- jarque.bera.test(residuals(modelo()))
    cat("Prueba de normalidad de Jarque-Bera\n")
    cat("Hipótesis nula (H0): Los residuos siguen una distribución normal.\n")
    cat("Hipótesis alternativa (H1): Los residuos no siguen una distribución normal.\n")
    print(test_result)
  })
  
  output$residual_plot <- renderPlot({
    req(modelo())
    residuos <- residuals(modelo())
    ajuste <- predict(modelo())
    plot(ajuste, residuos, pch = 20, col = "blue", main = "Residuos vs Predicciones",
         xlab = "Predicciones", ylab = "Residuos")
    lines(lowess(residuos ~ ajuste), col = "red", lwd = 2)
  })
  
  output$bp_test <- renderPrint({
    req(modelo())
    test_result <- bptest(modelo())
    cat("Prueba de homocedasticidad de Breusch-Pagan\n")
    cat("Hipótesis nula (H0): Los residuos tienen varianza constante (homocedasticidad).\n")
    cat("Hipótesis alternativa (H1): Los residuos no tienen varianza constante (heterocedasticidad).\n")
    print(test_result)
  })
  
  output$dw_test <- renderPrint({
    req(modelo())
    test_result <- durbinWatsonTest(modelo())
    cat("Prueba de autocorrelación de Durbin-Watson\n")
    cat("Hipótesis nula (H0): No hay autocorrelación en los residuos.\n")
    cat("Hipótesis alternativa (H1): Hay autocorrelación en los residuos.\n")
    print(test_result)
  })
  
  output$anova_table <- renderPrint({
    req(modelo())
    anova_result <- anova(modelo())
    print(anova_result)
  })
  
  # Historial de ejecuciones
  output$console_history <- renderTable({
    console_history()
  })
  
  # Generar gráfico de comparación de observados y predicciones
  output$observed_vs_predicted_plot <- renderPlot({
    req(modelo())
    predicciones <- predict(modelo())
    y_var_name <- all.vars(formula_obj())[1]
    observados <- datos()[[y_var_name]]
    
    df_plot <- data.frame(
      Observado = observados,
      Predicho = predicciones
    )
    
    ggplot(df_plot, aes(x = Predicho, y = Observado)) +  # Cambiar los ejes
      geom_point(color = "blue") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      ggtitle("Comparación de Predicciones vs Observados") +
      xlab("Predicho") + ylab("Observado") +
      theme_minimal()
  })
  
  # Mostrar el nombre del archivo a descargar
  output$download_filename <- renderText({
    req(modelo())
    paste("Archivo para descargar:", paste0(input$download_name, "_", Sys.Date(), ".xlsx"))
  })
  
  # Descargar los resultados en Excel
  output$download_results <- downloadHandler(
    filename = function() {
      file_name <- ifelse(nchar(input$download_name) > 0, input$download_name, "resultados_regresion")
      paste0(file_name, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(modelo())
      predicciones <- predict(modelo())
      y_var_name <- all.vars(formula_obj())[1]
      observados <- datos()[[y_var_name]]
      resultados_df <- data.frame(
        `Nº Observación` = 1:nrow(datos()),
        Observado = observados,
        Predicción = predicciones
      )
      write_xlsx(resultados_df, path = file)
    }
  )
}



# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
