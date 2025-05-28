library(dplyr)
library(shiny)
library(DT)
library(openxlsx)

# Función central de conciliación
conciliar_datos <- function(dfA, dfB, llaves, montoA, montoB) {
  # Preparar datos
  dfA_prep <- dfA %>% 
    mutate(across(all_of(llaves), as.character)) %>%
    select(all_of(c(llaves, montoA))) %>%
    rename(monto = !!montoA) %>%
    mutate(fuente = "Sistema A")
  
  dfB_prep <- dfB %>% 
    mutate(across(all_of(llaves), as.character)) %>%
    select(all_of(c(llaves, montoB))) %>%
    rename(monto = !!montoB) %>%
    mutate(fuente = "Sistema B")
  
  # Combinar datos
  datos_combinados <- bind_rows(dfA_prep, dfB_prep) %>%
    group_by(across(all_of(llaves))) %>%
    mutate(
      n_registros = n(),
      diferencia = monto - first(monto)
    ) %>%
    ungroup()
  
  # Identificar diferencias
  solo_A <- datos_combinados %>% 
    filter(n_registros == 1, fuente == "Sistema A")
  
  solo_B <- datos_combinados %>% 
    filter(n_registros == 1, fuente == "Sistema B")
  
  diferencias <- datos_combinados %>% 
    filter(n_registros > 1, diferencia != 0) %>%
    arrange(across(all_of(llaves)))
  
  # Crear resumen
  resumen <- list(
    registros_A = nrow(dfA),
    registros_B = nrow(dfB),
    solo_A = nrow(solo_A),
    solo_B = nrow(solo_B),
    coincidencias = nrow(dfA) - nrow(solo_A) - nrow(diferencias)/2,
    diferencias = nrow(diferencias)/2
  )
  
  return(list(
    resumen = resumen,
    detalles = list(
      solo_A = solo_A,
      solo_B = solo_B,
      diferencias = diferencias
    )
  ))
}

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Herramienta de Conciliación Contable"),
  sidebarLayout(
    sidebarPanel(
      fileInput("fileA", "Subir Sistema A (CSV/Excel)"),
      fileInput("fileB", "Subir Sistema B (CSV/Excel)"),
      textInput("llaves", "Columnas llave (separar por coma)", "id, fecha"),
      textInput("montoA", "Columna monto Sistema A", "monto"),
      textInput("montoB", "Columna monto Sistema B", "valor"),
      actionButton("conciliar", "Conciliar", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen", tableOutput("resumen")),
        tabPanel("Solo en A", DTOutput("soloA")),
        tabPanel("Solo en B", DTOutput("soloB")),
        tabPanel("Diferencias", DTOutput("diferencias")),
        tabPanel("Exportar", 
                 downloadButton("reporte", "Descargar Reporte Completo"))
      )
    )
  )
)

# Servidor
server <- function(input, output) {
  datos <- reactiveValues()
  
  observeEvent(input$conciliar, {
    req(input$fileA, input$fileB)
    
    # Leer archivos
    extA <- tools::file_ext(input$fileA$name)
    dfA <- if(extA == "csv") {
      read.csv(input$fileA$datapath)
    } else {
      read.xlsx(input$fileA$datapath)
    }
    
    extB <- tools::file_ext(input$fileB$name)
    dfB <- if(extB == "csv") {
      read.csv(input$fileB$datapath)
    } else {
      read.xlsx(input$fileB$datapath)
    }
    
    # Procesar parámetros
    llaves <- trimws(unlist(strsplit(input$llaves, ",")))
    montoA <- trimws(input$montoA)
    montoB <- trimws(input$montoB)
    
    # Ejecutar conciliación
    resultados <- conciliar_datos(dfA, dfB, llaves, montoA, montoB)
    
    # Almacenar resultados
    datos$resumen <- resultados$resumen
    datos$soloA <- resultados$detalles$solo_A
    datos$soloB <- resultados$detalles$solo_B
    datos$diferencias <- resultados$detalles$diferencias
  })
  
  # Renderizar resultados
  output$resumen <- renderTable({
    data.frame(
      Métrica = c("Registros Sistema A", "Registros Sistema B", 
                  "Solo en A", "Solo en B", 
                  "Coincidencias", "Diferencias"),
      Valor = unlist(datos$resumen)
    )
  })
  
  output$soloA <- renderDT({ datatable(datos$soloA, options = list(pageLength = 5)) })
  output$soloB <- renderDT({ datatable(datos$soloB, options = list(pageLength = 5)) })
  output$diferencias <- renderDT({ datatable(datos$diferencias, options = list(pageLength = 5)) })
  
  # Generar reporte
  output$reporte <- downloadHandler(
    filename = function() { paste0("reporte_conciliacion_", Sys.Date(), ".xlsx") },
    content = function(file) {
      wb <- createWorkbook()
      
      addWorksheet(wb, "Resumen")
      writeData(wb, "Resumen", data.frame(
        Métrica = names(datos$resumen),
        Valor = unlist(datos$resumen)
      )
      
      addWorksheet(wb, "Solo_en_A")
      writeData(wb, "Solo_en_A", datos$soloA)
      
      addWorksheet(wb, "Solo_en_B")
      writeData(wb, "Solo_en_B", datos$soloB)
      
      addWorksheet(wb, "Diferencias")
      writeData(wb, "Diferencias", datos$diferencias)
      
      saveWorkbook(wb, file)
    }
  )
}

# Ejecutar aplicación
shinyApp(ui, server)