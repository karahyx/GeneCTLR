library(shiny)
library(DT)
library(cowplot)

options(shiny.maxRequestSize=300*1024^2)


ui <- shiny::fluidPage(

  titlePanel("GeneCTLR: Data Imputation, Model Building, and Plotting"),

  sidebarPanel(

    fileInput(inputId = "impute", label = "Choose CSV File",
              accept = c("text/csv",
                         "text/comma-separated-values",
                         ".csv")),

    selectInput(inputId = "replace",
                label = "Choose the value to be used to replace the missing values:",
                choices = list("mean", "median")),

    numericInput(inputId = "dependentVarIndex",
                 label = "Enter the index of the original dependent variable column:",
                 value = 1,
                 min = 1),

    textInput(inputId = "deleteColumns",
              label = "Enter one or more column index to be deleted for model fitting, separated by commas:"),

    numericInput(inputId = "newDependentVarIndex",
                 label = "Enter the updated index of the dependent variable column:",
                 value = 1,
                 min = 1),

    sliderInput(inputId = "kVal",
                label = "Number of groups that a given data set is to be split into (K)",
                value = 5, min = 2, max = 50)

  ),

  mainPanel(

    tabsetPanel(type = "tabs",
                tabPanel("Data Imputation", DT::dataTableOutput("imputed")),
                tabPanel("Modelling", verbatimTextOutput("model")),
                tabPanel("Plot", plotOutput("plots")))
  )
)


server <- function(input, output) {

  # Impute the data set
  dataImputation <- reactive({

    inFile <- input$impute

    if (is.null(inFile)) {
      return(NULL)
    }

    dat <- read.csv(inFile$datapath, header = TRUE)
    imputedDat <- GeneCTLR::impute(data = dat, replace = input$replace)
    return(imputedDat)
  })

  # Impute ---------------------------------------------------------------------
  output$imputed <- DT::renderDataTable({
    if (!is.null(dataImputation)) {
      imputedDat <- dataImputation()
      DT::datatable(imputedDat)
    }

  })


  # Build the regression model
  modelBuilding <- reactive({

    if (!is.null(dataImputation)) {
      imputedDat <- dataImputation()
      colToDelete <- as.numeric(unlist(strsplit(input$deleteColumns, ", ")))

      newDat <- preProcessData(data = imputedDat,
                               dependentVarIndex = input$dependentVarIndex,
                               deleteColumns = colToDelete)

      results <- trainCV(data = newDat,
                         dependentVarIndex = input$newDependentVarIndex,
                         K = input$kVal)

      return(results)
    }


  })

  # Modelling ------------------------------------------------------------------
  output$model <- renderPrint({

    if (!is.null(modelBuilding)) {
      results <- modelBuilding()
      print(results$models)
    }
  })

  # Plot the ROC and PR curves
  plotting <- reactive({

    results <- modelBuilding()

    rocCurve <- plotROC(results = results,
                        dependentVarIndex = input$newDependentVarIndex)

    prCurve <- plotPR(results = results,
                      dependentVarIndex = input$newDependentVarIndex)

    rocPr <- plot_grid(rocCurve, prCurve)

    return(rocPr)

  })

  # Plotting -------------------------------------------------------------------
  output$plots <- renderPlot({

    rocPr <- plotting()
    print(rocPr)

  })

}


shiny::shinyApp(ui = ui, server = server)
# [END]
