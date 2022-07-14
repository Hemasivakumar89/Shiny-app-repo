
library(shiny)

ui <- fluidPage(
  
  titlePanel("Linear Modelling App"),
  
  sidebarLayout(
    sidebarPanel(
      
      fileInput("data", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      actionButton("lm_run", "Run Regression"),
      
    ),
    
    
    mainPanel(
      tableOutput('table'),
      plotOutput('scatterplot'),
      verbatimTextOutput('modelSummary')
      
    )
  )
)


server <- function(input, output) {
  
  model = reactiveValues()
  
  
  dataInput = reactive({
    
    req(input$data)
    
    
    read.csv(input$data$datapath,
             header = T,
             #header = input$header,
             stringsAsFactors = FALSE)
    
  })
  
  observeEvent(input$lm_run, {
    
    model$linear_model = lm(isolate(dataInput()$y) ~ isolate(dataInput()$x) )
    
  })
  
  output$table <- renderTable({
    dataInput()
  })
  
  output$scatterplot <- renderPlot({
    plot(dataInput()$x,dataInput()$y)
    abline(model$linear_model)
  })
 
  output$modelSummary <- renderPrint({summary(model$linear_model)})
  

}


shinyApp(ui = ui, server = server)