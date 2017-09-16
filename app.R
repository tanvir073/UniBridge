library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(shinyUI(navbarPage(
  "My App",
  # Data upload tab ----
  tabPanel(
    "Data",
    titlePanel("Upload Data"),
    
    # Sidebar layout with a input and output definitions ----
    sidebarLayout(
      
      sidebarPanel(
        h3("Chose Bridge Table"),
        selectInput(
          inputId = "idbConType",
          label = "Connection Type:",
          choices = c("Flat File", "SQL DB"),
          selected = "SQL DB"
        ),
        
        
        conditionalPanel(
          condition = "input.idbConType=='SQL DB'",
          textInput(
            inputId = "idbdbConStr",
            value = "",
            label = "Bridge DB Connection"
          )
          
        ),
        conditionalPanel(
          condition = "input.idbConType=='Flat File'",
          fileInput(
            inputId = "idbFF",
            label = "Bridge File",
            accept = c(".csv")
          )
          
        ),
        
        
        h3("Chose Data Table"),
        selectInput(
          inputId = "iddConType",
          label = "Connection Type:",
          choices = c("Flat File", "SQL DB")
        ),
        
        conditionalPanel(
          condition = "input.iddConType=='Flat File'",
          fileInput(
            inputId = "iddFF",
            label = "Data File",
            accept = c(".csv")
          )
          
        ),
        conditionalPanel(
          condition = "input.iddConType=='SQL DB'",
          textInput(
            inputId = "idddbConStr",
            value = "",
            label = "Data DB Connection"
          )
        )
      ),
      
      
      # Main panel for displaying outputs ----
      mainPanel(
        tableOutput("idbData"),
        tableOutput("iddData")
        
      )
    )
  ),
  tabPanel("Result")
)))

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  ractData<-reactiveValues()
  
  ## Bridge file load and output
  ractData$bridgeConType<-reactive({input$idbConType})
  ractData$dataConType<-reactive({input$iddConType})
  
  ractData$bridgeFF<-reactive({input$idbFF})
  
  ractData$bridgeTable<-reactive({
                        if(is.null(ractData$bridgeFF()$datapath)) return(NULL) 
                          read.csv(file = ractData$bridgeFF()$datapath,header = T, stringsAsFactors = FALSE)
                        
                        
                      })
    
  output$idbData <- renderTable({
    head(ractData$bridgeTable())
  })
  
  ## Data file load and output
  
  ractData$dataFF<-reactive({input$iddFF})
  ractData$dataTable<-reactive({
    if(is.null(ractData$dataFF()$datapath)) return(NULL) 
    read.csv(file = ractData$dataFF()$datapath,header = T, stringsAsFactors = FALSE)
    
    
  })
  
  output$iddData <- renderTable({
    head(ractData$dataTable())
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
