# app.R
library(shiny)
library(leaflet)
library(DT)


source("funciones.R")
source("global.R")

ui <- fluidPage(
  titlePanel("📊 Monitor de Afiliaciones - México Estratégico"),
  
  sidebarLayout(
    sidebarPanel(
      h4("🔄 Actualización de Datos"),
      # ❌ Eliminamos fileInput porque ahora usamos 32 archivos CSV por estado
      # fileInput("csv_upload", "Subir nuevo archivo CSV (.csv)", accept = ".csv"),
      actionButton("reload_data", "Actualizar datos", class = "btn-primary btn-success", style = "margin-top: 10px;"),
      
      hr(),
      h5("🔎 Filtrar por"),
      selectInput("filtro_estado", "Estado", choices = c("Todos", estados_lista), selected = "Todos"),
      selectInput("filtro_estatus", "Estatus", choices = c("Todos", unique(distritos_federales$cve_estado)), selected = "Todos"),
      radioButtons("nivel_agregacion", "Nivel de visualización:", 
                   choices = c("Nacional" = "nacional", "Estatal" = "estatal", "Distrital" = "distrital"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("🗺️ Mapa Interactivo", leafletOutput("mapa", height = "650px")),
        tabPanel("📋 Tabla de Resúmenes", DT::dataTableOutput("tabla_resumen")),
        tabPanel("📄 Datos Crudos", DT::dataTableOutput("tabla_cruda"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactivo único: Carga TODOS los CSVs de afiliaciones por estado
  datos_afiliados <- reactive({
    req(input$reload_data)
    cargar_afiliaciones_todos()  # ← Función nueva en funciones.R
  })
  
  # Agregaciones
  resumen_reactivo <- reactive({
    df <- datos_afiliados()
    generar_resumen(df)
  })
  
  # MAPA INTERACTIVO — VERSION FINAL CON SEMÁFORO DINÁMICO Y VISTA FOCALIZADA EN MÉXICO
  output$mapa <- renderLeaflet({
    df_agg <- resumen_reactivo()
    
    # Filtrar por nivel seleccionado
    nivel_seleccionado <- input$nivel_agregacion
    
    if (nivel_seleccionado == "nacional") {
      df_agg_filt <- df_agg %>%
        group_by(cve_estado, distrito_num, estatus) %>%
        summarise(total = sum(total), .groups = 'drop')
    } else if (nivel_seleccionado == "estatal") {
      df_agg_filt <- df_agg %>%
        filter(nivel == "estatal") %>%
        select(cve_estado, estatus, total)
    } else { # distrital
      df_agg_filt <- df_agg %>%
        filter(nivel == "distrital") %>%
        select(cve_estado, distrito_num, estatus, total)
    }
    
    # Unir con geometría y metas estatales
    map_data <- merge(
      distritos_federales,
      df_agg_filt,
      by.x = c("cve_estado", "distrito_num"),
      by.y = c("cve_estado", "distrito_num"),
      all.x = TRUE
    )
    
    # Calcular porcentaje respecto a la meta estatal
    map_data <- map_data %>%
      mutate(
        porcentaje = ifelse(!is.na(meta_estatal) & meta_estatal > 0 & !is.na(total),
                            total / meta_estatal * 100,
                            NA_real_),
        
        # Asignar color según semáforo (solo si hay meta > 0)
        color = case_when(
          meta_estatal == 0 ~ "#cccccc",                     # Gris: meta = 0 (incluye no monitoreados)
          is.na(porcentaje) ~ "#cccccc",                     # Gris: sin datos
          porcentaje < 60 ~ "#D10F3F",                       # Rojo: <60%
          porcentaje >= 60 & porcentaje < 81 ~ "#6BA4C6",    # Azul: 61-80%
          porcentaje >= 81 & porcentaje < 100 ~ "#FFDE6F",   # Amarillo: 81-99%
          porcentaje >= 100 ~ "#99C374",                     # Verde: ≥100%
          TRUE ~ "#cccccc"                                   # Por seguridad
        ),
        
        # Texto del popup
        popup_text = paste0(
          "<b>", estado_nombre, "</b><br>",
          ifelse(meta_estatal > 0,
                 paste0("Meta: ", format(meta_estatal, big.mark = ","), " | Afiliados: ",
                        ifelse(is.na(total), "0", format(total, big.mark = ",")), 
                        " | ", round(porcentaje, 1), "%"), 
                 "Sin meta asignada"),
          "<br>Estatus: ", ifelse(is.na(estatus), "N/A", estatus)
        )
      )
    
    # Crear mapa — ¡Ajustar automáticamente a todo México!
    leaflet(map_data) %>%
      addTiles() %>%
      fitBounds(                                  # ← ¡NUEVO!
        lng1 = -118.5, lat1 = 14.5,
        lng2 = -86.5,  lat2 = 32.7
      ) %>%
      addPolygons(
        weight = 1,
        color = "#444",
        fillColor = ~color,
        fillOpacity = 0.8,
        stroke = TRUE,
        layerId = ~paste(cve_estado, distrito_num),
        popup = ~popup_text
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("#D10F3F", "#6BA4C6", "#FFDE6F", "#99C374", "#cccccc"),
        labels = c("<60%", "61-80%", "81-99%", "≥100%", "Sin meta"),
        title = "Avance Estatal",
        opacity = 1
      )
  })
  
  # Tabla de resúmenes
  output$tabla_resumen <- DT::renderDataTable({
    df <- resumen_reactivo()
    
    if (input$filtro_estado != "Todos") {
      df <- df %>% filter(grepl(input$filtro_estado, estado_nombre, ignore.case = TRUE))  # ✅ ¡ESTO YA ESTÁ BIEN!
    }
    if (input$filtro_estatus != "Todos") {
      df <- df %>% filter(estatus == input$filtro_estatus)
    }
    
    df %>%
      datatable(
        options = list(pageLength = 10, lengthMenu = c(10, 25, 50)),
        rownames = FALSE,
        class = "display cell-border"
      )
  })
  
  # Tabla cruda
  output$tabla_cruda <- DT::renderDataTable({
    df <- datos_afiliados()
    
    df %>%
      datatable(
        options = list(pageLength = 10, lengthMenu = c(10, 25, 50)),
        rownames = FALSE,
        class = "display cell-border"
      )
  })
}

shinyApp(ui = ui, server = server)