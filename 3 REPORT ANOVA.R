library(shiny)
library(readxl)
library(ggplot2)
library(bslib)
library(report)
library(DT)

# Función para detectar comas y convertirlas a punto decimal
handle_decimal_separator <- function(x) {
  x <- gsub(",", ".", x)
  as.numeric(x)
}

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty", primary = "#3D9970", base_font = font_google("Poppins")),
  
  titlePanel("Análisis Inteligente: Prueba T o ANOVA con Interpretación"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Sube tu archivo CSV o Excel", accept = c(".csv", ".xlsx")),
      tags$hr(),
      uiOutput("var_select"),
      actionButton("run", "Ejecutar análisis", class = "btn btn-success")
    ),
    
    mainPanel(
      h4("Vista previa de los datos"),
      DTOutput("preview"),
      tags$hr(),
      
      h4("Resultado del análisis"),
      verbatimTextOutput("analysis_result"),
      
      h4("Interpretación"),
      verbatimTextOutput("interpretacion"),
      
      h4("Gráfico"),
      plotOutput("boxplot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Leer archivo subido
  data_input <- reactive({
    req(input$datafile)
    ext <- tools::file_ext(input$datafile$name)
    
    if (ext == "csv") {
      df <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
    } else if (ext == "xlsx") {
      df <- read_excel(input$datafile$datapath)
    } else {
      showNotification("❌ Formato no compatible", type = "error")
      return(NULL)
    }
    
    # Convertir comas a puntos decimales si corresponde
    df[] <- lapply(df, function(col) if (is.character(col)) handle_decimal_separator(col) else col)
    return(df)
  })
  
  # Vista previa
  output$preview <- renderDT({
    req(data_input())
    datatable(data_input(), options = list(pageLength = 5))
  })
  
  # Mostrar variables numéricas disponibles
  output$var_select <- renderUI({
    df <- data_input()
    num_vars <- names(df)[sapply(df, is.numeric)]
    if (length(num_vars) < 2) {
      return(helpText("⚠️ No hay suficientes variables numéricas para analizar."))
    }
    checkboxGroupInput("vars", "✅ Selecciona variables numéricas para analizar:", choices = num_vars)
  })
  
  # Análisis
  observeEvent(input$run, {
    
    output$analysis_result <- renderPrint({
      df <- data_input()
      req(input$vars)
      
      selected <- input$vars
      df_sel <- df[, selected, drop = FALSE]
      
      if (!all(sapply(df_sel, is.numeric))) {
        cat("❌ Las variables seleccionadas no son numéricas.")
        return()
      }
      
      if (ncol(df_sel) < 2) {
        cat("❗ Necesitas seleccionar al menos 2 variables numéricas.")
      } else if (ncol(df_sel) == 2) {
        cat("✔️ Se realiza una prueba T de Student:\n\n")
        print(t.test(df_sel[[1]], df_sel[[2]]))
      } else {
        cat("✔️ Se realiza un ANOVA para varias variables:\n\n")
        df_long <- stack(df_sel)
        modelo <- aov(values ~ ind, data = df_long)
        print(summary(modelo))
      }
    })
    
    # Interpretación automática con traducción
    output$interpretacion <- renderPrint({
      df <- data_input()
      req(input$vars)
      selected <- input$vars
      df_sel <- df[, selected, drop = FALSE]
      
      if (ncol(df_sel) < 2 || !all(sapply(df_sel, is.numeric))) {
        cat("⚠️ No se puede interpretar: se requieren al menos dos variables numéricas.")
        return()
      }
      
      if (ncol(df_sel) >= 3) {
        df_long <- stack(df_sel)
        modelo <- aov(values ~ ind, data = df_long)
        rep <- capture.output(report(modelo))
      } else {
        modelo <- t.test(df_sel[[1]], df_sel[[2]])
        rep <- capture.output(report(modelo))
      }
      
      # Traducción básica
      rep <- gsub("The ANOVA \\(Analysis of Variance\\)", "El ANOVA (Análisis de Varianza)", rep)
      rep <- gsub("Student's t-test", "Prueba T de Student", rep)
      rep <- gsub("found a statistically significant effect", "encontró un efecto estadísticamente significativo", rep)
      rep <- gsub("did not find a statistically significant effect", "no encontró un efecto estadísticamente significativo", rep)
      rep <- gsub("p = ", "valor p = ", rep)
      rep <- gsub("CI = ", "IC = ", rep)
      
      cat(paste(rep, collapse = "\n"))
    })
    
    # Boxplot
    output$boxplot <- renderPlot({
      df <- data_input()
      req(input$vars)
      selected <- input$vars
      df_sel <- df[, selected, drop = FALSE]
      
      if (!all(sapply(df_sel, is.numeric)) || ncol(df_sel) < 2) return()
      
      if (ncol(df_sel) == 2) {
        boxplot(df_sel, col = c("#FF6F61", "#6A5ACD"), main = "Boxplot Comparativo", ylab = "Valores")
      } else {
        df_long <- stack(df_sel)
        boxplot(values ~ ind, data = df_long, col = rainbow(ncol(df_sel)), main = "Boxplot ANOVA", ylab = "Valores")
      }
    })
    
  })
}

# Ejecutar aplicación
shinyApp(ui, server)

