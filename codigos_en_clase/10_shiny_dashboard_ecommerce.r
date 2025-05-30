# Dashboard de Ventas Ecommerce - Shiny App
# Instalar paquetes necesarios si no están instalados:
# install.packages(c("shiny", "shinydashboard", "DT", "plotly", "dplyr", "lubridate", "readr"))

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(lubridate)
library(readr)

# Datos de ejemplo (en producción, cargarías desde CSV o base de datos)
create_sample_data <- function() {
  set.seed(123)  # Para reproducibilidad
  
  # Configuración
  regiones <- c('Norte', 'Sur', 'Centro', 'Este', 'Oeste')
  categorias <- c('Electrónicos', 'Ropa', 'Hogar', 'Deportes', 'Libros')
  productos <- list(
    'Electrónicos' = c('Smartphone', 'Laptop', 'Auriculares', 'Tablet', 'Smartwatch'),
    'Ropa' = c('Camiseta', 'Jeans', 'Zapatos', 'Chaqueta', 'Vestido'),
    'Hogar' = c('Sofá', 'Mesa', 'Lámpara', 'Espejo', 'Cojín'),
    'Deportes' = c('Zapatillas', 'Pelota', 'Raqueta', 'Pesas', 'Bicicleta'),
    'Libros' = c('Novela', 'Técnico', 'Cocina', 'Historia', 'Biografía')
  )
  canales <- c('Web', 'Móvil', 'Marketplace')
  vendedores <- c('Ana García', 'Carlos Ruiz', 'María López', 'José Martín', 'Laura Díaz')
  
  # Generar datos
  n_records <- 300
  datos <- data.frame(
    fecha = seq(from = as.Date("2024-05-01"), 
                to = as.Date("2025-05-01"), 
                length.out = n_records),
    region = sample(regiones, n_records, replace = TRUE),
    categoria = sample(categorias, n_records, replace = TRUE),
    canal = sample(canales, n_records, replace = TRUE),
    vendedor = sample(vendedores, n_records, replace = TRUE),
    unidades_vendidas = sample(1:8, n_records, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Agregar productos basados en categoría
  datos$producto <- sapply(1:nrow(datos), function(i) {
    sample(productos[[datos$categoria[i]]], 1)
  })
  
  # Precios base por categoría
  precios_base <- list(
    'Electrónicos' = c(200, 1500),
    'Ropa' = c(25, 150),
    'Hogar' = c(50, 800),
    'Deportes' = c(30, 300),
    'Libros' = c(15, 45)
  )
  
  # Calcular métricas
  datos$precio_unitario <- sapply(1:nrow(datos), function(i) {
    rango <- precios_base[[datos$categoria[i]]]
    round(runif(1, rango[1], rango[2]))
  })
  
  datos$ingresos <- datos$precio_unitario * datos$unidades_vendidas
  
  # Costos por categoría (% del ingreso)
  costos_pct <- list(
    'Electrónicos' = 0.65,
    'Ropa' = 0.45,
    'Hogar' = 0.55,
    'Deportes' = 0.50,
    'Libros' = 0.60
  )
  
  datos$costo <- sapply(1:nrow(datos), function(i) {
    round(datos$ingresos[i] * costos_pct[[datos$categoria[i]]])
  })
  
  datos$margen <- datos$ingresos - datos$costo
  datos$porcentaje_margen <- round((datos$margen / datos$ingresos) * 100)
  
  return(datos)
}

# Crear datos
ventas_data <- create_sample_data()

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Ventas Ecommerce"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Principal", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Datos Detallados", tabName = "datos", icon = icon("table")),
      menuItem("Análisis Estadístico", tabName = "analisis", icon = icon("calculator"))
    ),
    
    # Filtros
    hr(),
    h4("Filtros", style = "margin-left: 15px; color: white;"),
    
    dateRangeInput("fechas",
                   "Rango de Fechas:",
                   start = min(ventas_data$fecha),
                   end = max(ventas_data$fecha),
                   format = "yyyy-mm-dd",
                   language = "es"),
    
    selectInput("region_filter",
                "Región:",
                choices = c("Todas" = "todas", unique(ventas_data$region)),
                selected = "todas"),
    
    selectInput("categoria_filter",
                "Categoría:",
                choices = c("Todas" = "todas", unique(ventas_data$categoria)),
                selected = "todas"),
    
    selectInput("canal_filter",
                "Canal:",
                choices = c("Todos" = "todos", unique(ventas_data$canal)),
                selected = "todos")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .small-box .icon-large {
          font-size: 50px;
        }
      "))
    ),
    
    tabItems(
      # Dashboard Principal
      tabItem(tabName = "dashboard",
        fluidRow(
          # KPIs - Scorecards
          valueBoxOutput("total_ingresos"),
          valueBoxOutput("total_unidades"),
          valueBoxOutput("ticket_promedio")
        ),
        
        fluidRow(
          # Tendencia temporal
          box(
            title = "Tendencia de Ventas Mensual", 
            status = "primary", 
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("tendencia_plot")
          ),
          
          # Top categorías
          box(
            title = "Ventas por Categoría", 
            status = "success", 
            solidHeader = TRUE,
            width = 4,
            plotlyOutput("categorias_plot")
          )
        ),
        
        fluidRow(
          # Performance por región
          box(
            title = "Ventas por Región", 
            status = "info", 
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("regiones_plot")
          ),
          
          # Distribución por canal
          box(
            title = "Distribución por Canal", 
            status = "warning", 
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("canales_plot")
          )
        ),
        
        fluidRow(
          # Performance vendedores
          box(
            title = "Performance de Vendedores", 
            status = "danger", 
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("vendedores_table")
          )
        )
      ),
      
      # Datos Detallados
      tabItem(tabName = "datos",
        fluidRow(
          box(
            title = "Datos Completos de Ventas", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("datos_completos")
          )
        )
      ),
      
      # Análisis Estadístico
      tabItem(tabName = "analisis",
        fluidRow(
          box(
            title = "Configuración de Regresión Lineal", 
            status = "primary", 
            solidHeader = TRUE,
            width = 4,
            
            h4("Seleccionar Variables:"),
            
            selectInput("var_dependiente",
                        "Variable Dependiente (Y):",
                        choices = c("Ingresos" = "ingresos",
                                   "Margen" = "margen", 
                                   "% Margen" = "porcentaje_margen",
                                   "Unidades Vendidas" = "unidades_vendidas"),
                        selected = "ingresos"),
            
            checkboxGroupInput("vars_independientes",
                              "Variables Independientes (X):",
                              choices = c("Precio Unitario" = "precio_unitario",
                                         "Unidades Vendidas" = "unidades_vendidas", 
                                         "Costo" = "costo",
                                         "Región" = "region",
                                         "Categoría" = "categoria",
                                         "Canal" = "canal"),
                              selected = c("precio_unitario", "unidades_vendidas")),
            
            br(),
            actionButton("ejecutar_regresion", "Ejecutar Regresión", 
                        class = "btn-primary", icon = icon("play")),
            
            br(), br(),
            
            h5("Información:", style = "color: #3498db;"),
            p("• Selecciona una variable dependiente (Y)", style = "font-size: 12px;"),
            p("• Selecciona una o más variables independientes (X)", style = "font-size: 12px;"),
            p("• Haz clic en 'Ejecutar Regresión' para ver resultados", style = "font-size: 12px;"),
            p("• Los filtros del sidebar afectan el análisis", style = "font-size: 12px; color: #e74c3c;")
          ),
          
          box(
            title = "Resultados de la Regresión", 
            status = "success", 
            solidHeader = TRUE,
            width = 8,
            
            conditionalPanel(
              condition = "output.mostrar_resultados",
              
              h4("Resumen del Modelo:"),
              verbatimTextOutput("resumen_regresion"),
              
              br(),
              
              h4("Interpretación:"),
              htmlOutput("interpretacion_regresion")
            ),
            
            conditionalPanel(
              condition = "!output.mostrar_resultados",
              div(
                style = "text-align: center; padding: 50px;",
                icon("info-circle", style = "font-size: 50px; color: #bdc3c7;"),
                h4("Configura las variables y ejecuta la regresión", 
                   style = "color: #7f8c8d; margin-top: 20px;")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Gráfico de Dispersión con Línea de Regresión", 
            status = "info", 
            solidHeader = TRUE,
            width = 6,
            
            conditionalPanel(
              condition = "output.mostrar_resultados",
              plotlyOutput("grafico_regresion")
            ),
            
            conditionalPanel(
              condition = "!output.mostrar_resultados",
              div(
                style = "text-align: center; padding: 50px;",
                p("El gráfico aparecerá después de ejecutar la regresión", 
                  style = "color: #7f8c8d;")
              )
            )
          ),
          
          box(
            title = "Análisis de Residuos", 
            status = "warning", 
            solidHeader = TRUE,
            width = 6,
            
            conditionalPanel(
              condition = "output.mostrar_resultados",
              plotlyOutput("grafico_residuos")
            ),
            
            conditionalPanel(
              condition = "!output.mostrar_resultados",
              div(
                style = "text-align: center; padding: 50px;",
                p("El análisis de residuos aparecerá después de ejecutar la regresión", 
                  style = "color: #7f8c8d;")
              )
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Datos reactivos (filtrados)
  datos_filtrados <- reactive({
    datos <- ventas_data
    
    # Filtro de fechas
    datos <- datos %>%
      filter(fecha >= input$fechas[1] & fecha <= input$fechas[2])
    
    # Filtro de región
    if (input$region_filter != "todas") {
      datos <- datos %>% filter(region == input$region_filter)
    }
    
    # Filtro de categoría
    if (input$categoria_filter != "todas") {
      datos <- datos %>% filter(categoria == input$categoria_filter)
    }
    
    # Filtro de canal
    if (input$canal_filter != "todos") {
      datos <- datos %>% filter(canal == input$canal_filter)
    }
    
    return(datos)
  })
  
  # KPIs - Value Boxes
  output$total_ingresos <- renderValueBox({
    total <- sum(datos_filtrados()$ingresos, na.rm = TRUE)
    valueBox(
      value = paste("$", format(total, big.mark = ",", scientific = FALSE)),
      subtitle = "Ingresos Totales",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$total_unidades <- renderValueBox({
    total <- sum(datos_filtrados()$unidades_vendidas, na.rm = TRUE)
    valueBox(
      value = format(total, big.mark = ","),
      subtitle = "Unidades Vendidas",
      icon = icon("boxes"),
      color = "blue"
    )
  })
  
  output$ticket_promedio <- renderValueBox({
    promedio <- mean(datos_filtrados()$ingresos, na.rm = TRUE)
    valueBox(
      value = paste("$", round(promedio, 0)),
      subtitle = "Ticket Promedio",
      icon = icon("receipt"),
      color = "yellow"
    )
  })
  
  # Gráfico de tendencia temporal
  output$tendencia_plot <- renderPlotly({
    datos <- datos_filtrados() %>%
      mutate(mes = floor_date(fecha, "month")) %>%
      group_by(mes) %>%
      summarise(ingresos = sum(ingresos, na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(datos, aes(x = mes, y = ingresos)) +
      geom_line(color = "#3498db", size = 2) +
      geom_point(color = "#2980b9", size = 3) +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(x = "Mes", y = "Ingresos", title = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>%
      layout(hovermode = 'x unified')
  })
  
  # Gráfico de categorías
  output$categorias_plot <- renderPlotly({
    datos <- datos_filtrados() %>%
      group_by(categoria) %>%
      summarise(ingresos = sum(ingresos, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(ingresos))
    
    p <- plot_ly(datos, x = ~ingresos, y = ~reorder(categoria, ingresos), 
                 type = 'bar', orientation = 'h',
                 marker = list(color = '#27ae60')) %>%
      layout(xaxis = list(title = "Ingresos ($)"),
             yaxis = list(title = ""))
    
    return(p)
  })
  
  # Gráfico de regiones
  output$regiones_plot <- renderPlotly({
    datos <- datos_filtrados() %>%
      group_by(region) %>%
      summarise(ingresos = sum(ingresos, na.rm = TRUE), .groups = 'drop')
    
    p <- plot_ly(datos, x = ~region, y = ~ingresos, 
                 type = 'bar',
                 marker = list(color = '#3498db')) %>%
      layout(xaxis = list(title = "Región"),
             yaxis = list(title = "Ingresos ($)"))
    
    return(p)
  })
  
  # Gráfico de canales (pie chart)
  output$canales_plot <- renderPlotly({
    datos <- datos_filtrados() %>%
      group_by(canal) %>%
      summarise(ingresos = sum(ingresos, na.rm = TRUE), .groups = 'drop')
    
    p <- plot_ly(datos, labels = ~canal, values = ~ingresos, 
                 type = 'pie',
                 textinfo = 'label+percent',
                 marker = list(colors = c('#e74c3c', '#f39c12', '#9b59b6'))) %>%
      layout(showlegend = TRUE)
    
    return(p)
  })
  
  # Tabla de vendedores
  output$vendedores_table <- DT::renderDataTable({
    datos <- datos_filtrados() %>%
      group_by(vendedor) %>%
      summarise(
        Ventas = n(),
        `Unidades Vendidas` = sum(unidades_vendidas, na.rm = TRUE),
        `Ingresos Totales` = sum(ingresos, na.rm = TRUE),
        `Margen Total` = sum(margen, na.rm = TRUE),
        `% Margen Promedio` = round(mean(porcentaje_margen, na.rm = TRUE), 1),
        .groups = 'drop'
      ) %>%
      arrange(desc(`Ingresos Totales`))
    
    # Formatear números
    datos$`Ingresos Totales` <- paste("$", format(datos$`Ingresos Totales`, big.mark = ","))
    datos$`Margen Total` <- paste("$", format(datos$`Margen Total`, big.mark = ","))
    datos$`% Margen Promedio` <- paste(datos$`% Margen Promedio`, "%")
    
    DT::datatable(datos, 
                  options = list(pageLength = 10, 
                                scrollX = TRUE,
                                dom = 't'),
                  rownames = FALSE) %>%
      DT::formatStyle(columns = 1:ncol(datos), fontSize = '14px')
  })
  
  # Tabla de datos completos
  output$datos_completos <- DT::renderDataTable({
    datos <- datos_filtrados() %>%
      mutate(
        fecha = as.character(fecha),
        ingresos = paste("$", format(ingresos, big.mark = ",")),
        costo = paste("$", format(costo, big.mark = ",")),
        margen = paste("$", format(margen, big.mark = ",")),
        porcentaje_margen = paste(porcentaje_margen, "%")
      )
    
    DT::datatable(datos, 
                  options = list(pageLength = 15, 
                                scrollX = TRUE,
                                search = list(regex = TRUE, caseInsensitive = TRUE)),
                  filter = 'top',
                  rownames = FALSE)
  })
  
  # === ANÁLISIS ESTADÍSTICO - REGRESIÓN LINEAL ===
  
  # Variables reactivas para la regresión
  modelo_regresion <- reactiveValues(
    modelo = NULL,
    ejecutado = FALSE,
    datos_modelo = NULL
  )
  
  # Preparar datos para regresión
  datos_para_regresion <- reactive({
    datos <- datos_filtrados()
    
    # Convertir variables categóricas a factores
    datos$region <- as.factor(datos$region)
    datos$categoria <- as.factor(datos$categoria)
    datos$canal <- as.factor(datos$canal)
    
    return(datos)
  })
  
  # Ejecutar regresión cuando se presiona el botón
  observeEvent(input$ejecutar_regresion, {
    req(input$var_dependiente, input$vars_independientes)
    
    tryCatch({
      datos <- datos_para_regresion()
      
      # Verificar que hay suficientes datos
      if (nrow(datos) < 10) {
        showNotification("Se necesitan al menos 10 observaciones para la regresión", 
                        type = "warning", duration = 5)
        return()
      }
      
      # Crear fórmula dinámicamente
      vars_x <- paste(input$vars_independientes, collapse = " + ")
      formula_str <- paste(input$var_dependiente, "~", vars_x)
      formula_obj <- as.formula(formula_str)
      
      # Ejecutar regresión
      modelo <- lm(formula_obj, data = datos)
      
      # Verificar que el modelo es válido
      if (any(is.na(coef(modelo)))) {
        showNotification("Advertencia: Algunas variables pueden estar correlacionadas", 
                        type = "warning", duration = 5)
      }
      
      # Guardar resultados
      modelo_regresion$modelo <- modelo
      modelo_regresion$ejecutado <- TRUE
      modelo_regresion$datos_modelo <- datos
      
      # Mostrar notificación de éxito
      showNotification("Regresión ejecutada exitosamente!", 
                      type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error en la regresión:", e$message), 
                      type = "error", duration = 5)
      modelo_regresion$ejecutado <- FALSE
    })
  })
  
  # Mostrar resultados (para condicionales)
  output$mostrar_resultados <- reactive({
    return(modelo_regresion$ejecutado)
  })
  outputOptions(output, "mostrar_resultados", suspendWhenHidden = FALSE)
  
  # Resumen de la regresión
  output$resumen_regresion <- renderPrint({
    req(modelo_regresion$modelo)
    summary(modelo_regresion$modelo)
  })
  
  # Interpretación automática
  output$interpretacion_regresion <- renderUI({
    req(modelo_regresion$modelo)
    
    modelo <- modelo_regresion$modelo
    r_squared <- summary(modelo)$r.squared
    adj_r_squared <- summary(modelo)$adj.r.squared
    p_value <- summary(modelo)$fstatistic
    
    # Calcular p-value del modelo
    if (!is.null(p_value)) {
      model_p <- pf(p_value[1], p_value[2], p_value[3], lower.tail = FALSE)
    } else {
      model_p <- 1
    }
    
    # Interpretaciones
    interpretaciones <- tags$div(
      tags$h5("📊 Interpretación de Resultados:", style = "color: #2c3e50;"),
      
      tags$p(tags$strong("R-cuadrado: "), 
             sprintf("%.3f (%.1f%%)", r_squared, r_squared * 100),
             tags$br(),
             "El modelo explica el ", 
             tags$strong(sprintf("%.1f%%", r_squared * 100)),
             " de la variabilidad en ", input$var_dependiente),
      
      tags$p(tags$strong("R-cuadrado ajustado: "), 
             sprintf("%.3f", adj_r_squared),
             tags$br(),
             "Considerando el número de variables, el ajuste es del ",
             tags$strong(sprintf("%.1f%%", adj_r_squared * 100))),
      
      if (model_p < 0.001) {
        tags$p(tags$strong("Significancia del modelo: "), 
               tags$span("Altamente significativo (p < 0.001)", 
                        style = "color: #27ae60; font-weight: bold;"),
               tags$br(),
               "El modelo es estadísticamente válido.")
      } else if (model_p < 0.05) {
        tags$p(tags$strong("Significancia del modelo: "), 
               tags$span("Significativo (p < 0.05)", 
                        style = "color: #f39c12; font-weight: bold;"),
               tags$br(),
               "El modelo es estadísticamente válido.")
      } else {
        tags$p(tags$strong("Significancia del modelo: "), 
               tags$span("No significativo (p ≥ 0.05)", 
                        style = "color: #e74c3c; font-weight: bold;"),
               tags$br(),
               "El modelo podría no ser confiable.")
      },
      
      tags$hr(),
      
      tags$h6("💡 Variables significativas:", style = "color: #34495e;"),
      tags$ul(
        lapply(names(coef(modelo))[-1], function(var) {
          coef_p <- summary(modelo)$coefficients[var, 4]
          coef_val <- summary(modelo)$coefficients[var, 1]
          
          if (coef_p < 0.001) {
            significance <- tags$span("***", style = "color: #27ae60;")
          } else if (coef_p < 0.01) {
            significance <- tags$span("**", style = "color: #f39c12;")
          } else if (coef_p < 0.05) {
            significance <- tags$span("*", style = "color: #e67e22;")
          } else {
            significance <- tags$span("ns", style = "color: #95a5a6;")
          }
          
          tags$li(
            tags$strong(var), ": ",
            sprintf("%.4f", coef_val), " ",
            significance,
            sprintf(" (p = %.4f)", coef_p)
          )
        })
      ),
      
      tags$small(
        tags$em("Significancia: *** p<0.001, ** p<0.01, * p<0.05, ns no significativo"),
        style = "color: #7f8c8d;"
      )
    )
    
    return(interpretaciones)
  })
  
  # Gráfico de dispersión con línea de regresión
  output$grafico_regresion <- renderPlotly({
    req(modelo_regresion$modelo, modelo_regresion$datos_modelo)
    
    modelo <- modelo_regresion$modelo
    datos <- modelo_regresion$datos_modelo
    
    # Para gráfico simple, usar solo la primera variable independiente si hay múltiples
    if (length(input$vars_independientes) == 1) {
      var_x <- input$vars_independientes[1]
      var_y <- input$var_dependiente
      
      # Verificar si la variable X es numérica
      if (is.numeric(datos[[var_x]])) {
        p <- plot_ly(datos, x = ~get(var_x), y = ~get(var_y), 
                     type = 'scatter', mode = 'markers',
                     marker = list(color = '#3498db', opacity = 0.6),
                     name = 'Datos observados') %>%
          add_lines(x = ~get(var_x), y = ~fitted(modelo),
                   line = list(color = '#e74c3c', width = 3),
                   name = 'Línea de regresión') %>%
          layout(xaxis = list(title = var_x),
                 yaxis = list(title = var_y),
                 title = paste("Regresión:", var_y, "vs", var_x))
      } else {
        # Para variables categóricas, hacer un boxplot
        p <- plot_ly(datos, x = ~get(var_x), y = ~get(var_y), 
                     type = 'box',
                     marker = list(color = '#3498db')) %>%
          layout(xaxis = list(title = var_x),
                 yaxis = list(title = var_y),
                 title = paste("Distribución:", var_y, "por", var_x))
      }
    } else {
      # Para múltiples variables, mostrar valores observados vs predichos
      p <- plot_ly(x = ~fitted(modelo), y = ~datos[[input$var_dependiente]], 
                   type = 'scatter', mode = 'markers',
                   marker = list(color = '#3498db', opacity = 0.6),
                   name = 'Datos') %>%
        add_lines(x = ~fitted(modelo), y = ~fitted(modelo),
                 line = list(color = '#e74c3c', width = 2, dash = 'dash'),
                 name = 'Línea perfecta') %>%
        layout(xaxis = list(title = 'Valores Predichos'),
               yaxis = list(title = paste('Valores Observados -', input$var_dependiente)),
               title = 'Valores Observados vs Predichos')
    }
    
    return(p)
  })
  
  # Gráfico de residuos
  output$grafico_residuos <- renderPlotly({
    req(modelo_regresion$modelo)
    
    modelo <- modelo_regresion$modelo
    residuos <- residuals(modelo)
    predichos <- fitted(modelo)
    
    p <- plot_ly(x = ~predichos, y = ~residuos, 
                 type = 'scatter', mode = 'markers',
                 marker = list(color = '#e74c3c', opacity = 0.6),
                 name = 'Residuos') %>%
      add_lines(x = ~predichos, y = 0,
               line = list(color = '#34495e', width = 2, dash = 'dash'),
               name = 'Línea de referencia') %>%
      layout(xaxis = list(title = 'Valores Predichos'),
             yaxis = list(title = 'Residuos'),
             title = 'Análisis de Residuos',
             annotations = list(
               list(x = 0.02, y = 0.98, 
                    text = "Los residuos deben distribuirse aleatoriamente alrededor de 0",
                    showarrow = FALSE, xref = 'paper', yref = 'paper',
                    font = list(size = 10, color = '#7f8c8d'))
             ))
    
    return(p)
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

# INSTRUCCIONES DE USO:
# 1. Instalar paquetes necesarios (ver línea 3-4)
# 2. Ejecutar este código en RStudio
# 3. La aplicación se abrirá en tu navegador
# 4. Usar los filtros del sidebar para interactuar con los datos
# 5. Explorar las tres pestañas: Dashboard Principal, Datos Detallados y Análisis Estadístico

# NOTAS PARA LA DEMO:
# - Los datos se generan automáticamente (300 registros)
# - Todos los filtros son interactivos y actualizan todos los gráficos Y la regresión
# - Los gráficos son interactivos gracias a Plotly (hover, zoom, etc.)
# - La regresión lineal permite seleccionar variables dependientes e independientes
# - Interpretación automática de resultados estadísticos
# - Gráficos de dispersión y análisis de residuos incluidos
# - Responsive design que se adapta a diferentes tamaños de pantalla

# FUNCIONALIDADES DE REGRESIÓN:
# - Variables dependientes: Ingresos, Margen, % Margen, Unidades Vendidas
# - Variables independientes: Precio, Unidades, Costo, Región, Categoría, Canal
# - Soporte para variables numéricas y categóricas
# - Análisis de significancia estadística automático
# - Visualización de residuos para validar supuestos del modelo
# - Los filtros del dashboard afectan también el análisis de regresión