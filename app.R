# app.R
library(shiny)
library(leaflet)
library(DT)
library(dplyr)
library(sf)

# ✅ ¡CARGAR PRIMERO LAS FUNCIONES!
source("funciones.R")

# ✅ LUEGO CARGAR global.R (que usa las funciones)
source("global.R")

ui <- fluidPage(
  titlePanel("📊 Monitor de Afiliaciones - México Estratégico"),
  
  sidebarLayout(
    sidebarPanel(
      h4("🔄 Actualización de Datos"),
      actionButton("reload_data", "Actualizar datos", class = "btn-primary btn-success", style = "margin-top: 10px;"),
      
      hr(),
      h5("🔎 Filtrar por"),
      selectInput("filtro_estado", "Estado", choices = c("Todos", estados_lista), selected = "Todos"),
      
      selectInput("filtro_estatus", "Estatus", 
                  choices = c("Todos", "Afiliado", "Contacto", "Proceso"), 
                  selected = "Todos"),
      
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
  
  datos_afiliados <- reactive({
    req(input$reload_data)
    cargar_afiliaciones_todos()
  })
  
  resumen_reactivo <- reactive({
    df <- datos_afiliados()
    generar_resumen(df)
  })
  
  output$mapa <- renderLeaflet({
    df_agg <- resumen_reactivo()
    
    nivel_seleccionado <- input$nivel_agregacion
    filtro_estado <- input$filtro_estado
    filtro_estatus <- input$filtro_estatus
    
    if (filtro_estado != "Todos") {
      df_agg <- df_agg %>% filter(estado_nombre == filtro_estado)
    }
    if (filtro_estatus != "Todos") {
      df_agg <- df_agg %>% filter(estatus == filtro_estatus)
    }
    
    if (nivel_seleccionado == "nacional") {
      df_agg_filt <- df_agg %>%
        group_by(cve_estado, distrito_num, estatus) %>%
        summarise(total = sum(total), .groups = 'drop')
      map_data <- merge(
        distritos_federales,
        df_agg_filt,
        by.x = c("cve_estado", "distrito_num"),
        by.y = c("cve_estado", "distrito_num"),
        all.x = TRUE
      )
    } else if (nivel_seleccionado == "estatal") {
      df_agg_filt <- df_agg %>%
        filter(nivel == "estatal") %>%
        select(cve_estado, estatus, total)
      map_data <- merge(
        distritos_federales,
        df_agg_filt,
        by.x = "cve_estado",
        by.y = "cve_estado",
        all.x = TRUE
      )
    } else {
      df_agg_filt <- df_agg %>%
        filter(nivel == "distrital") %>%
        select(cve_estado, distrito_num, estatus, total)
      map_data <- merge(
        distritos_federales,
        df_agg_filt,
        by.x = c("cve_estado", "distrito_num"),
        by.y = c("cve_estado", "distrito_num"),
        all.x = TRUE
      )
    }
    
    map_data <- map_data %>%
      mutate(
        porcentaje = ifelse(!is.na(meta_estatal) & meta_estatal > 0 & !is.na(total),
                            total / meta_estatal * 100,
                            NA_real_),
        color = case_when(
          meta_estatal == 0 ~ "#cccccc",
          is.na(porcentaje) ~ "#cccccc",
          porcentaje < 60 ~ "#D10F3F",
          porcentaje >= 60 & porcentaje < 81 ~ "#6BA4C6",
          porcentaje >= 81 & porcentaje < 100 ~ "#FFDE6F",
          porcentaje >= 100 ~ "#99C374",
          TRUE ~ "#cccccc"
        ),
        popup_text = paste0(
          "<b>", estado_nombre, "</b><br>",
          "Distrito: ", distrito_num, " ", distrito, "<br>",   # ← ¡¡¡CLAVE!!! ¡AÑADE EL NÚMERO!
          ifelse(meta_estatal > 0,
                 paste0("Meta: ", format(meta_estatal, big.mark = ","), " | Afiliados: ",
                        ifelse(is.na(total), "0", format(total, big.mark = ",")), 
                        " | ", round(porcentaje, 1), "%"), 
                 "Sin meta asignada"),
          "<br>Estatus: ", ifelse(is.na(estatus), "N/A", estatus)
        )
      )
    
    leaflet(map_data) %>%
      addTiles() %>%
      fitBounds(
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
  
  output$tabla_resumen <- DT::renderDataTable({
    df <- resumen_reactivo()
    if (input$filtro_estado != "Todos") {
      df <- df %>% filter(estado_nombre == input$filtro_estado)
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
  
  output$tabla_cruda <- DT::renderDataTable({
    df <- datos_afiliados()
    if (input$filtro_estado != "Todos") {
      df <- df %>% filter(estado == input$filtro_estado)
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
}

shinyApp(ui = ui, server = server)