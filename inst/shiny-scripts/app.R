library(shiny)

ui_impute <- sidebarLayout(
    sidebarPanel(

      fileInput(inputId = "impute", label = "Choose EXCEL File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
                ),

      selectInput(inputId = "replace",
                label = "Choose the value to be used to replace the missing values:",
                choices = list("mean",
                               "median"))
    ),

    mainPanel(
      tableOutput("imputed")
    )
)

ui_model <- fluidPage({
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "dependentVarIndex",
                   label = "Enter the index of the dependent variable column:",
                   min = 1),

      textInput(inputId = "deleteColumns",
                label = "Enter one or more column index to be deleted for model fitting, separated by commas:"),

      numericInput(inputId = "newDependentVarIndex",
                   label = "Enter the updated index of the dependent variable column:",
                   min = 1),

      sliderInput(inputId = "kVal",
                  label = "Number of groups that a given data set is to be split into (K)",
                  value = 5, min = 2, max = 50),

      sliderInput(inputId = "colIndex",
                  label = "Index of the dependent variable column in the data set",
                  value = 1, min = 1, max = 50)
    ),

    mainPanel(
      textOutput("models"),
      plotOutput("ROC"),
      plotOutput("PR")
      )
)
})


server <- function(input, output) {
  dataImputation <- reactive({
    inFile <- input$impute
    colToDelete <- as.numeric(unlist(strsplit(input$deleteColumns, ", ")))

    if (is.null(inFile)) {
      return(NULL)
    }

    dat <- read.csv(inFile$datapath)
    imputedDat <- impute(data = dat, replace = input$replace)

    return(imputedDat)
  })

  modelBuilding <- reactive({
    imputedDat <- dataImputation()
    newDat <- preProcessData(data = imputedDat,
                             dependentVarIndex = input$dependentVarIndex,
                             deleteColumns = colToDelete)

    results <- trainCV(data = newDat,
                       colIndex = input$newDependentVarIndex,
                       K = input$kVal)
    return
  })

  # Impute ---------------------------------------------------------------------
  output$imputed <- renderTable({
    imputedDat <- dataImputation()
    print(head(imputedDat))
  })

  # Modeling -------------------------------------------------------------------
  output$models <- renderText({
    results <- trainCV(data = newDat,
                       colIndex = input$newDependentVarIndex,
                       K = input$kVal)

    print(results$models)
  })

  output$ROC <- renderPlot({

  })

}


shiny::shinyApp(ui = ui, server = server)
# [END]
