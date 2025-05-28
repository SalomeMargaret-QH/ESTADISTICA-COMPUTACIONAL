library(shiny)
library(pdftools)
library(httr)
library(jsonlite)
library(DT)

# API Key (¡no compartas esto públicamente!)
api_key <- "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"

# Función con manejo de errores
analizar_con_gpt <- function(seccion, texto) {
  if (nchar(texto) < 100) return("Contenido insuficiente.")
  
  prompt <- paste0(
    "Resume la siguiente sección de un artículo científico, indicando: tema, lugar, técnicas, cronología, hallazgos importantes.\n\n",
    "SECCIÓN: ", seccion, "\n\nTEXTO:\n", substr(texto, 1, 3000)
  )
  
  tryCatch({
    res <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type_json(),
      encode = "json",
      body = list(
        model = "gpt-4",
        messages = list(list(role = "user", content = prompt)),
        temperature = 0.3
      )
    )
    parsed <- content(res, as = "parsed")
    if (!is.null(parsed$choices)) {
      return(parsed$choices[[1]]$message$content)
    } else if (!is.null(parsed$error$message)) {
      return(paste("Error API:", parsed$error$message))
    } else {
      return("Respuesta vacía o no estructurada.")
    }
  }, error = function(e) {
    return(paste("Error de conexión:", e$message))
  })
}

# Interfaz con estilo
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .title-custom { font-size: 28px; font-weight: bold; margin-top: 20px; margin-bottom: 20px; }
      .table-style td, .table-style th { text-align: center; vertical-align: middle; }
      .well-custom { background-color: #f7f7f7; padding: 15px; border-radius: 10px; border: 1px solid #ccc; }
    "))
  ),
  
  titlePanel(
    div(icon("book-open", lib = "font-awesome"), span(" Meta-Análisis Automático de PDFs", class = "title-custom"))
  ),
  
  fluidRow(
    column(4,
           wellPanel(
             fileInput("pdfs", "📂 Subir artículos en PDF", multiple = TRUE, accept = ".pdf"),
             actionButton("analizar", "🔍 Analizar Artículos", class = "btn btn-primary btn-block"),
             br(),
             textOutput("estado")
           )
    ),
    column(8,
           wellPanel(
             h4("📊 Resultados del Análisis"),
             DTOutput("tabla_resultados")
           )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  resultados <- reactiveVal(data.frame())
  
  observeEvent(input$analizar, {
    req(input$pdfs)
    output$estado <- renderText("⏳ Analizando documentos...")
    
    lista_final <- list()
    for (i in seq_along(input$pdfs$datapath)) {
      texto <- paste(pdf_text(input$pdfs$datapath[i]), collapse = "\n")
      secciones <- list(
        Título = substr(texto, 1, 300),
        Abstract = sub("(?i).*Abstract", "", texto),
        Introducción = sub("(?i).*Introduction", "", texto),
        Metodología = sub("(?i).*Material and methods|Methods", "", texto),
        Resultados = sub("(?i).*Results", "", texto),
        Discusión = sub("(?i).*Discussion", "", texto),
        Conclusiones = sub("(?i).*Conclusion", "", texto)
      )
      analisis <- sapply(names(secciones), function(seccion) {
        analizar_con_gpt(seccion, secciones[[seccion]])
      }, USE.NAMES = TRUE)
      tabla <- data.frame(
        Documento = rep(input$pdfs$name[i], length(analisis)),
        Seccion = names(analisis),
        Analisis = unname(analisis),
        stringsAsFactors = FALSE
      )
      lista_final[[i]] <- tabla
    }
    
    resultado_total <- do.call(rbind, lista_final)
    resultados(resultado_total)
    output$estado <- renderText("✅ ¡Análisis completado con éxito!")
  })
  
  output$tabla_resultados <- renderDT({
    req(resultados())
    datatable(resultados(), options = list(pageLength = 10, autoWidth = TRUE), class = "table table-bordered table-style")
  })
}

shinyApp(ui = ui, server = server)
