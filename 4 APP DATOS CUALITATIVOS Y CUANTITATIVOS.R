library(shiny)
library(ggplot2)
library(DT)
library(readxl)
library(bslib)
library(car)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly", primary = "#2C3E50", base_font = font_google("Lato")),
  
  titlePanel("App Completa de Pruebas Estadísticas"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "Sube tu archivo CSV o Excel", accept = c(".csv", ".xlsx")),
      uiOutput("var_response"),
      uiOutput("var_group"),
      uiOutput("prueba")
    ),
    mainPanel(
      h4("Vista previa de los datos"),
      DTOutput("tabla"),
      hr(),
      h4("Resultado estadístico"),
      verbatimTextOutput("resultado"),
      h4("Gráfico"),
      plotOutput("grafico")
    )
  )
)

server <- function(input, output, session) {
  
  datos <- reactive({
    req(input$archivo)
    ext <- tools::file_ext(input$archivo$name)
    if (ext == "csv") {
      read.csv(input$archivo$datapath, stringsAsFactors = FALSE)
    } else if (ext == "xlsx") {
      read_excel(input$archivo$datapath)
    } else {
      showNotification("Formato no compatible", type = "error")
      return(NULL)
    }
  })
  
  output$tabla <- renderDT({
    req(datos())
    datatable(datos(), options = list(pageLength = 5))
  })
  
  output$var_response <- renderUI({
    req(datos())
    selectInput("respuesta", "Selecciona variable de análisis:", choices = names(datos()))
  })
  
  output$var_group <- renderUI({
    req(datos())
    selectInput("grupo", "Selecciona variable de grupo:", choices = names(datos()))
  })
  
  output$prueba <- renderUI({
    req(input$respuesta, input$grupo)
    selectInput("test", "Selecciona la prueba estadística:",
                choices = c(
                  "Resumen estadístico",
                  "Prueba de Normalidad (Shapiro-Wilk)",
                  "Chi-cuadrado",
                  "Fisher Exacta",
                  "Prueba t de Student",
                  "Prueba de Welch",
                  "ANOVA",
                  "Kruskal-Wallis",
                  "Mann–Whitney",
                  "Wilcoxon",
                  "Correlación de Pearson",
                  "Correlación de Spearman"
                ))
  })
  
  output$resultado <- renderPrint({
    req(input$test)
    df <- datos()
    y <- df[[input$respuesta]]
    g <- as.factor(df[[input$grupo]])
    
    switch(input$test,
           "Resumen estadístico" = summary(y),
           "Prueba de Normalidad (Shapiro-Wilk)" = shapiro.test(y),
           "Chi-cuadrado" = chisq.test(table(g, y)),
           "Fisher Exacta" = fisher.test(table(g, y)),
           "Prueba t de Student" = t.test(y ~ g, var.equal = TRUE),
           "Prueba de Welch" = t.test(y ~ g, var.equal = FALSE),
           "ANOVA" = summary(aov(y ~ g)),
           "Kruskal-Wallis" = kruskal.test(y ~ g),
           "Mann–Whitney" = wilcox.test(y ~ g),
           "Wilcoxon" = wilcox.test(y, paired = TRUE),
           "Correlación de Pearson" = cor.test(df[[input$respuesta]], df[[input$grupo]], method = "pearson"),
           "Correlación de Spearman" = cor.test(df[[input$respuesta]], df[[input$grupo]], method = "spearman")
    )
  })
  
  output$grafico <- renderPlot({
    req(input$respuesta, input$grupo)
    df <- datos()
    y <- df[[input$respuesta]]
    g <- as.factor(df[[input$grupo]])
    
    if (is.numeric(y)) {
      boxplot(y ~ g, col = rainbow(length(unique(g))),
              main = paste("Boxplot de", input$respuesta, "por", input$grupo),
              xlab = input$grupo, ylab = input$respuesta)
    } else {
      barplot(table(y), col = "steelblue", main = paste("Frecuencia de", input$respuesta))
    }
  })
}

shinyApp(ui, server)
