library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Comparación de Grupos: ANOVA y t-test"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "Sube tu archivo CSV", accept = ".csv"),
      uiOutput("columna_respuesta"),
      uiOutput("columna_grupo")
    ),
    
    mainPanel(
      h4("Resumen Descriptivo"),
      tableOutput("resumen"),
      h4("Gráfico Comparativo"),
      plotOutput("grafico"),
      h4("Resultado Estadístico"),
      verbatimTextOutput("resultado")
    )
  )
)

server <- function(input, output, session) {
  
  datos <- reactive({
    req(input$archivo)
    read.csv(input$archivo$datapath)
  })
  
  output$columna_respuesta <- renderUI({
    req(datos())
    num_vars <- names(datos())[sapply(datos(), is.numeric)]
    selectInput("respuesta", "Variable cuantitativa:", choices = num_vars)
  })
  
  output$columna_grupo <- renderUI({
    req(datos())
    cat_vars <- names(datos())[sapply(datos(), function(x) is.factor(x) || is.character(x))]
    selectInput("grupo", "Variable categórica (factor):", choices = cat_vars)
  })
  
  output$resumen <- renderTable({
    req(input$respuesta, input$grupo)
    datos() %>%
      group_by(.data[[input$grupo]]) %>%
      summarise(
        Media = mean(.data[[input$respuesta]], na.rm = TRUE),
        Mediana = median(.data[[input$respuesta]], na.rm = TRUE),
        Desv.Estandar = sd(.data[[input$respuesta]], na.rm = TRUE),
        n = n()
      )
  })
  
  output$grafico <- renderPlot({
    req(input$respuesta, input$grupo)
    ggplot(datos(), aes(x = .data[[input$grupo]], y = .data[[input$respuesta]], fill = .data[[input$grupo]])) +
      geom_boxplot() +
      labs(x = input$grupo, y = input$respuesta, title = "Boxplot por grupo") +
      theme_minimal()
  })
  
  output$resultado <- renderPrint({
    req(input$respuesta, input$grupo)
    
    df <- datos()
    y <- df[[input$respuesta]]
    g <- as.factor(df[[input$grupo]])
    
    if (nlevels(g) == 2) {
      cat("Comparación entre dos grupos (prueba t de Student):\n\n")
      print(t.test(y ~ g))
    } else if (nlevels(g) > 2) {
      cat("Comparación entre más de dos grupos (ANOVA):\n\n")
      modelo <- aov(y ~ g)
      print(summary(modelo))
      cat("\nPrueba post-hoc (Tukey):\n")
      print(TukeyHSD(modelo))
    } else {
      cat("La variable de grupo debe tener al menos 2 niveles.")
    }
  })
}

shinyApp(ui, server)
