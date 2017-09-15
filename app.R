


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
        tableOutput("bridgeTable")
        # verbatimTextOutput("summary")
      )
    )
  ),
  tabPanel("Result")
)))

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  bridgeConType<-reactive({input$idbConType})
  dataConType<-reactive({input$iddConType})
  bridgeFF<-reactive({input$idbFF})
  
  if(length(bridgeFF)!=0) output$summary<-reactive({bridgeFF()$datapath})
  output$bridgeTable<-reactive({
                        if(is.null(bridgeFF)) return(NULL) 
                          read.csv(file = bridgeFF()$datapath,header = T)
                        
                        
                      })
    
  
  
  # ,input$idbdbConStr,input$idbFF
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)