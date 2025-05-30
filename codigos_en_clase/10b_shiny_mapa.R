# Mapa Interactivo de Ventas por Región - Shiny App
# Instalar paquetes necesarios:
# install.packages(c("shiny", "shinydashboard", "leaflet", "dplyr", "DT", "plotly", "htmltools", "RColorBrewer"))

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(DT)
library(plotly)
library(htmltools)
library(RColorBrewer)

# Crear datos de ejemplo con coordenadas geográficas
create_geo_data <- function() {
  set.seed(123)
  
  # Datos de ventas por región con coordenadas (ejemplo para España)
  regiones_data <- data.frame(
    region = c('Norte', 'Sur', 'Centro', 'Este', 'Oeste'),
    lat = c(43.3614, 37.3886, 40.4168, 41.6516, 42.1734),
    lng = c(-5.8593, -5.9823, -3.7038, 0.8896, -8.6959),
    ciudad_principal = c('Gijón', 'Córdoba', 'Madrid', 'Barcelona', 'Santiago'),
    stringsAsFactors = FALSE
  )
  
  # Generar datos de ventas por mes y región
  fechas <- seq(from = as.Date("2024-01-01"), to = as.Date("2024-12-01"), by = "month")
  
  datos_completos <- expand.grid(
    fecha = fechas,
    region = regiones_data$region,
    stringsAsFactors = FALSE
  )
  
  # Simular métricas por región y mes
  set.seed(123)
  datos_completos$ventas_totales <- round(runif(nrow(datos_completos), 15000, 80000))
  datos_completos$num_clientes <- round(runif(nrow(datos_completos), 150, 800))
  datos_completos$ticket_promedio <- round(datos_completos$ventas_totales / datos_completos$num_clientes, 2)
  datos_completos$margen_pct <- round(runif(nrow(datos_completos), 25, 55), 1)
  datos_completos$satisfaccion_cliente <- round(runif(nrow(datos_completos), 3.2, 4.8), 1)
  datos_completos$num_tiendas <- sample(c(2, 3, 4, 5, 6), nrow(datos_completos), replace = TRUE)
  
  # Unir con coordenadas
  datos_geo <- merge(datos_completos, regiones_data, by = "region")
  
  return(datos_geo)
}

# Crear datos
ventas_geo <- create_geo_data()

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Mapa Interactivo de Ventas"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapa Principal", tabName = "mapa", icon = icon("map")),
      menuItem("Datos por Región", tabName = "datos", icon = icon("table"))
    ),
    
    hr(),
    h4("Controles del Mapa", style = "margin-left: 15px; color: white;"),
    
    # Filtro temporal
    selectInput("mes_seleccionado",
                "Mes:",
                choices = setNames(1:12, c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                                           "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")),
                selected = format(Sys.Date(), "%m")),
    
    # Métrica a visualizar
    selectInput("metrica_mapa",
                "Métrica a Visualizar:",
                choices = list(
                  "Ventas Totales" = "ventas_totales",
                  "Número de Clientes" = "num_clientes", 
                  "Ticket Promedio" = "ticket_promedio",
                  "% Margen" = "margen_pct",
                  "Satisfacción Cliente" = "satisfaccion_cliente",
                  "Número de Tiendas" = "num_tiendas"
                ),
                selected = "ventas_totales"),
    
    # Estilo del mapa
    selectInput("estilo_mapa",
                "Estilo del Mapa:",
                choices = list(
                  "OpenStreetMap" = "OpenStreetMap",
                  "Satelital" = "Esri.WorldImagery",
                  "Topográfico" = "OpenTopoMap",
                  "Oscuro" = "CartoDB.DarkMatter"
                ),
                selected = "OpenStreetMap"),
    
    br(),
    
    # Información
    div(
      style = "margin: 15px; padding: 10px; background: rgba(255,255,255,0.1); border-radius: 5px;",
      h5("💡 Instrucciones:", style = "color: white; margin-bottom: 10px;"),
      p("• Selecciona un mes y métrica", style = "color: white; font-size: 12px; margin: 5px 0;"),
      p("• Haz clic en los círculos del mapa", style = "color: white; font-size: 12px; margin: 5px 0;"),
      p("• Los colores indican el rendimiento", style = "color: white; font-size: 12px; margin: 5px 0;"),
      p("• Usa el zoom para navegar", style = "color: white; font-size: 12px; margin: 5px 0;")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .leaflet-container {
          background: #f4f4f4;
        }
        .info-box {
          margin-bottom: 15px;
        }
        .metric-card {
          background: white;
          border-radius: 8px;
          padding: 15px;
          margin: 10px 0;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
      "))
    ),
    
    tabItems(
      # Mapa Principal
      tabItem(tabName = "mapa",
              fluidRow(
                # KPIs resumidos
                valueBoxOutput("total_ventas_mes"),
                valueBoxOutput("total_clientes_mes"),
                valueBoxOutput("promedio_satisfaccion")
              ),
              
              fluidRow(
                # Mapa principal
                box(
                  title = textOutput("titulo_mapa"), 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 8,
                  height = "600px",
                  leafletOutput("mapa_ventas", height = "530px")
                ),
                
                # Panel de información
                box(
                  title = "Información de Región", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 4,
                  height = "600px",
                  
                  # Información de región seleccionada
                  conditionalPanel(
                    condition = "output.region_seleccionada != ''",
                    div(id = "info_region",
                        h4(textOutput("nombre_region_seleccionada")),
                        hr(),
                        htmlOutput("detalles_region")
                    )
                  ),
                  
                  # Mensaje cuando no hay selección
                  conditionalPanel(
                    condition = "output.region_seleccionada == ''",
                    div(
                      style = "text-align: center; padding: 50px 20px;",
                      icon("mouse-pointer", style = "font-size: 40px; color: #bdc3c7;"),
                      h5("Haz clic en una región del mapa", 
                         style = "color: #7f8c8d; margin-top: 20px;"),
                      p("para ver información detallada", 
                        style = "color: #95a5a6;")
                    )
                  ),
                  
                  br(),
                  
                  # Ranking de regiones
                  h5("🏆 Ranking del Mes"),
                  DT::dataTableOutput("ranking_regiones", height = "250px")
                )
              ),
              
              fluidRow(
                # Gráfico de tendencia
                box(
                  title = "Evolución Anual por Región", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("tendencia_anual")
                )
              )
      ),
      
      # Datos por Región
      tabItem(tabName = "datos",
              fluidRow(
                box(
                  title = "Resumen de Métricas por Región", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  DT::dataTableOutput("resumen_regiones")
                ),
                
                box(
                  title = "Datos Mensuales Completos", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 6,
                  DT::dataTableOutput("datos_mensuales")
                )
              ),
              
              fluidRow(
                box(
                  title = "Análisis Comparativo", 
                  status = "danger", 
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("analisis_comparativo")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Variables reactivas
  region_seleccionada <- reactiveVal("")
  
  # Datos filtrados por mes
  datos_mes_actual <- reactive({
    ventas_geo %>%
      filter(format(fecha, "%m") == sprintf("%02d", as.numeric(input$mes_seleccionado)))
  })
  
  # KPIs del mes
  output$total_ventas_mes <- renderValueBox({
    total <- sum(datos_mes_actual()$ventas_totales)
    valueBox(
      value = paste("€", format(total, big.mark = ".", decimal.mark = ",")),
      subtitle = "Ventas Totales del Mes",
      icon = icon("euro-sign"),
      color = "green"
    )
  })
  
  output$total_clientes_mes <- renderValueBox({
    total <- sum(datos_mes_actual()$num_clientes)
    valueBox(
      value = format(total, big.mark = "."),
      subtitle = "Clientes Atendidos",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$promedio_satisfaccion <- renderValueBox({
    promedio <- mean(datos_mes_actual()$satisfaccion_cliente)
    valueBox(
      value = paste(round(promedio, 1), "★"),
      subtitle = "Satisfacción Promedio",
      icon = icon("star"),
      color = "yellow"
    )
  })
  
  # Título dinámico del mapa
  output$titulo_mapa <- renderText({
    mes_nombre <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")[as.numeric(input$mes_seleccionado)]
    
    metrica_nombre <- switch(input$metrica_mapa,
                             "ventas_totales" = "Ventas Totales",
                             "num_clientes" = "Número de Clientes",
                             "ticket_promedio" = "Ticket Promedio", 
                             "margen_pct" = "% Margen",
                             "satisfaccion_cliente" = "Satisfacción Cliente",
                             "num_tiendas" = "Número de Tiendas"
    )
    
    paste("Mapa de", metrica_nombre, "•", mes_nombre, "2024")
  })
  
  # Mapa principal
  output$mapa_ventas <- renderLeaflet({
    datos <- datos_mes_actual()
    metrica <- input$metrica_mapa
    
    # Obtener valores de la métrica seleccionada
    valores_metrica <- datos[[metrica]]
    
    # Paleta de colores
    pal <- colorNumeric(
      palette = c("#ff4444", "#ffaa00", "#44aa44", "#0088cc", "#4444ff"),
      domain = valores_metrica
    )
    
    # Crear popups informativos
    popups <- sprintf(
      "<div style='font-family: Arial; font-size: 14px;'>
        <h4 style='margin: 0 0 10px 0; color: #2c3e50;'>%s</h4>
        <p style='margin: 5px 0;'><strong>Ciudad:</strong> %s</p>
        <p style='margin: 5px 0;'><strong>Ventas:</strong> €%s</p>
        <p style='margin: 5px 0;'><strong>Clientes:</strong> %s</p>
        <p style='margin: 5px 0;'><strong>Ticket Promedio:</strong> €%s</p>
        <p style='margin: 5px 0;'><strong>Margen:</strong> %s%%</p>
        <p style='margin: 5px 0;'><strong>Satisfacción:</strong> %s ★</p>
        <p style='margin: 5px 0;'><strong>Tiendas:</strong> %s</p>
      </div>",
      datos$region, datos$ciudad_principal,
      format(datos$ventas_totales, big.mark = "."),
      format(datos$num_clientes, big.mark = "."),
      format(datos$ticket_promedio, big.mark = ".", decimal.mark = ","),
      datos$margen_pct, datos$satisfaccion_cliente, datos$num_tiendas
    )
    
    # Crear mapa
    leaflet(datos) %>%
      addProviderTiles(input$estilo_mapa) %>%
      setView(lng = -3.7, lat = 40.4, zoom = 6) %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = scales::rescale(valores_metrica, to = c(8, 25)),
        fillColor = pal(valores_metrica),
        color = "white",
        weight = 2,
        opacity = 1,
        fillOpacity = 0.8,
        popup = popups,
        layerId = ~region,
        label = paste(datos$region, ":", format(valores_metrica, big.mark = "."))
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = valores_metrica,
        title = switch(metrica,
                       "ventas_totales" = "Ventas (€)",
                       "num_clientes" = "Clientes",
                       "ticket_promedio" = "Ticket (€)", 
                       "margen_pct" = "% Margen",
                       "satisfaccion_cliente" = "Satisfacción",
                       "num_tiendas" = "Tiendas"
        ),
        opacity = 1
      )
  })
  
  # Observar clicks en el mapa
  observeEvent(input$mapa_ventas_marker_click, {
    click <- input$mapa_ventas_marker_click
    if (!is.null(click$id)) {
      region_seleccionada(click$id)
    }
  })
  
  # Información de región seleccionada
  output$region_seleccionada <- reactive({
    region_seleccionada()
  })
  outputOptions(output, "region_seleccionada", suspendWhenHidden = FALSE)
  
  output$nombre_region_seleccionada <- renderText({
    if (region_seleccionada() != "") {
      datos <- datos_mes_actual() %>% filter(region == region_seleccionada())
      paste("Región", datos$region[1])
    }
  })
  
  output$detalles_region <- renderUI({
    if (region_seleccionada() != "") {
      datos <- datos_mes_actual() %>% filter(region == region_seleccionada())
      
      tags$div(
        class = "metric-card",
        tags$h5("📍 Ubicación", style = "color: #3498db; margin-bottom: 10px;"),
        tags$p(paste("Ciudad principal:", datos$ciudad_principal[1])),
        
        tags$h5("💰 Métricas Financieras", style = "color: #27ae60; margin: 15px 0 10px 0;"),
        tags$p(paste("Ventas totales: €", format(datos$ventas_totales[1], big.mark = "."))),
        tags$p(paste("Ticket promedio: €", format(datos$ticket_promedio[1], decimal.mark = ","))),
        tags$p(paste("Margen: ", datos$margen_pct[1], "%")),
        
        tags$h5("👥 Métricas de Cliente", style = "color: #e74c3c; margin: 15px 0 10px 0;"),
        tags$p(paste("Clientes atendidos:", format(datos$num_clientes[1], big.mark = "."))),
        tags$p(paste("Satisfacción: ", datos$satisfaccion_cliente[1], " ★")),
        
        tags$h5("🏪 Infraestructura", style = "color: #9b59b6; margin: 15px 0 10px 0;"),
        tags$p(paste("Número de tiendas:", datos$num_tiendas[1]))
      )
    }
  })
  
  # Ranking de regiones
  output$ranking_regiones <- DT::renderDataTable({
    datos <- datos_mes_actual() %>%
      select(region, ventas_totales, satisfaccion_cliente) %>%
      arrange(desc(ventas_totales)) %>%
      mutate(
        posicion = row_number(),
        ventas_formateadas = paste("€", format(ventas_totales, big.mark = ".")),
        satisfaccion_formateada = paste(satisfaccion_cliente, "★")
      ) %>%
      select(`#` = posicion, Región = region, Ventas = ventas_formateadas, 
             Satisfacción = satisfaccion_formateada)
    
    DT::datatable(datos, 
                  options = list(pageLength = 5, dom = 't', ordering = FALSE),
                  rownames = FALSE) %>%
      DT::formatStyle(columns = 1:4, fontSize = '12px')
  })
  
  # Evolución anual
  output$tendencia_anual <- renderPlotly({
    metrica_seleccionada <- input$metrica_mapa
    
    datos_tendencia <- ventas_geo %>%
      group_by(fecha, region) %>%
      summarise(valor = sum(get(metrica_seleccionada), na.rm = TRUE), .groups = 'drop')
    
    p <- plot_ly(datos_tendencia, x = ~fecha, y = ~valor, color = ~region, 
                 type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = paste("Evolución de", switch(metrica_seleccionada,
                                             "ventas_totales" = "Ventas Totales",
                                             "num_clientes" = "Número de Clientes",
                                             "ticket_promedio" = "Ticket Promedio", 
                                             "margen_pct" = "% Margen",
                                             "satisfaccion_cliente" = "Satisfacción Cliente",
                                             "num_tiendas" = "Número de Tiendas")),
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Valor"),
        hovermode = 'x unified'
      )
    
    return(p)
  })
  
  # Datos detallados
  output$resumen_regiones <- DT::renderDataTable({
    datos <- ventas_geo %>%
      group_by(region, ciudad_principal) %>%
      summarise(
        ventas_anuales = sum(ventas_totales),
        clientes_anuales = sum(num_clientes),
        ticket_promedio = mean(ticket_promedio),
        margen_promedio = mean(margen_pct),
        satisfaccion_promedio = mean(satisfaccion_cliente),
        .groups = 'drop'
      ) %>%
      arrange(desc(ventas_anuales))
    
    DT::datatable(datos, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatCurrency(c("ventas_anuales", "ticket_promedio"), currency = "€", digits = 0) %>%
      DT::formatRound(c("margen_promedio", "satisfaccion_promedio"), 1)
  })
  
  output$datos_mensuales <- DT::renderDataTable({
    datos <- ventas_geo %>%
      mutate(mes_nombre = format(fecha, "%B %Y")) %>%
      select(mes_nombre, region, ventas_totales, num_clientes, satisfaccion_cliente)
    
    DT::datatable(datos, 
                  options = list(pageLength = 15, scrollX = TRUE),
                  filter = 'top',
                  rownames = FALSE) %>%
      DT::formatCurrency("ventas_totales", currency = "€", digits = 0)
  })
  
  output$analisis_comparativo <- renderPlotly({
    datos <- datos_mes_actual()
    
    p <- plot_ly(datos, x = ~ventas_totales, y = ~satisfaccion_cliente, 
                 size = ~num_clientes, color = ~region,
                 text = ~paste("Región:", region, "<br>Ventas: €", format(ventas_totales, big.mark = "."),
                               "<br>Satisfacción:", satisfaccion_cliente, "★"),
                 hovertemplate = "%{text}<extra></extra>") %>%
      add_markers() %>%
      layout(title = "Relación Ventas vs Satisfacción (tamaño = clientes)",
             xaxis = list(title = "Ventas Totales (€)"),
             yaxis = list(title = "Satisfacción Cliente (★)"))
    
    return(p)
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

# CARACTERÍSTICAS DEL MAPA INTERACTIVO:
# 🗺️ Mapa base con múltiples estilos (OpenStreetMap, Satelital, etc.)
# 📍 Marcadores interactivos con información detallada
# 🎨 Codificación de colores basada en métricas seleccionadas
# 📊 Panel de información que actualiza al hacer clic
# 📈 Gráficos de tendencia temporal
# 📋 Ranking dinámico de regiones
# 🔄 Filtros temporales y de métricas
# 📱 Diseño responsive
# 🎯 Popups informativos con hover

# ESTO ES IMPOSIBLE EN LOOKER STUDIO:
# ❌ Mapas interactivos complejos como este
# ❌ Clicks que actualicen otros elementos
# ❌ Múltiples estilos de mapa
# ❌ Popups personalizados con HTML
# ❌ Integración total entre mapa, gráficos y tablas