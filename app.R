library(shiny)

runSql<-function(conn,sql){
  
  channel=odbcDriverConnect(conn)
  tbl=sqlQuery(channel, sql, errors = TRUE)
  close(channel)
  return(tbl)
  
}

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
          ),
          conditionalPanel(
            condition = "input.idbConType=='SQL DB'",
            textInput(
              inputId = "idbsql",
              value = "",
              label = "SQL Query"
            )
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
            value = "Driver=MySQL ODBC 5.3 Unicode Driver;Server=localhost;Database=test;Uid=root;Pwd=",
            label = "Data DB Connection"
          )
        ),
        conditionalPanel(
          condition = "input.iddConType=='SQL DB'",
          textInput(
            inputId = "iddsql",
            value = "",
            label = "SQL Query"
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
  
  ractData$bridgeConType<-reactive({input$idbConType})
  ractData$dataConType<-reactive({input$iddConType})
  
  ## Bridge file load and output
  
  ractData$bridgeFF<-reactive({input$idbFF})
  ractData$bridgedbConn<-reactive({input$idbdbConStr})
  ractData$bridgesql<- reactive(input$idbsql)
  
  ractData$bridgeTable<-reactive({
                        if(ractData$bridgeConType()=="SQL DB" & (ractData$bridgedbConn()=="" | ractData$bridgesql()=="")){
                          return(NULL) 
                        }
                        else if(ractData$bridgeConType()=="Flat File" & !is.null(ractData$bridgeFF()$datapath)){ 
                          read.csv(file = ractData$bridgeFF()$datapath,header = T, stringsAsFactors = FALSE)
                        }
                        else if(!(is.null(ractData$bridgedbConn()) | is.null(ractData$bridgesql()))){
                          runSql(ractData$bridgedbConn(),ractData$bridgesql())
                        }
                        else return(NULL) 
                        
                        
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
