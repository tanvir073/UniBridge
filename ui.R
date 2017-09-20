library(shinydashboard)f
library(shiny)

dashboardPage(
    skin = "black",
    dashboardHeader(
        title = "Bridging Tools",
        dropdownMenu(type = "notifications", notificationItem(text = "Execution Time", icon("exclamation-triangle"), status = "info"))
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data Loading Configuration", tabName = "file_loading", icon = icon("th")),
            menuItem("Bridging Configuration", tabName = "bridge_config", icon = icon("cogs")),
            menuItem("Results", tabName = "results", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "file_loading",
                h2("Source Data Configuration"),
                
                #data load layout
                selectInput(inputId = "source_load_type", label = "Select Source Method", choices = c("File System", "SQL Server", ""), selected = "File System"),
                
                conditionalPanel(condition = 'input.source_load_type=="File System"',
                    fileInput(inputId = "source_file_load", label = "Choose Excel File", accept = c(".xlsx",".xlx",".csv"))
                ),
                
                conditionalPanel(condition = 'input.source_load_type=="SQL Server"',
                    fluidRow(
                        column(width = 4, textInput(inputId = "source_sql_server", label = "Server Name")),
                        column(width = 4, textInput(inputId = "source_sql_user",label = "User Name")),
                        column(width = 4, passwordInput(inputId = "source_sql_pass",label = "Password"))
                    ),
                    textareaInput(id = "source_sql_query", label = "SQL Query", rows = 10, cols = 60)
                ),
                #view master table head
                tableOutput("source_table_view"),
                
                h2("Master Data Configuration"),
                
                #data load layout
                selectInput(inputId = "master_load_type", label = "Select Source Method", choices = c("File System", "SQL Server", ""), selected = "SQL Server"),
                conditionalPanel(condition = 'input.master_load_type=="File System"',
                    fileInput(inputId = "master_file_load", label = "Choose Excel File", accept = c(".xlsx",".xlx",".csv"))
                ),
                conditionalPanel(condition = 'input.master_load_type=="SQL Server"',
                    fluidRow(
                        column(width = 4, textInput(inputId = "master_sql_server", label = "Server Name"
                            )
                        ),
                        column(width = 4, textInput(inputId = "master_sql_user",label = "User Name"
                            )
                        ),
                        column(width = 4, passwordInput(inputId = "master_sql_pass",label = "Password"
                            )
                        )
                    ),
                    fluidRow(column(width = 6,
                        textareaInput(id = "master_sql_query", label = "SQL Query", rows = 10, cols = 60)
                    ))
                )
            ),
            tabItem(
                tabName = "bridge_config",
                h2("Bridge Config tab content"),
                fluidRow(
                    column(width = 3, 
                           numericInput(inputId = "number_attrib_match", label = "Matching Field", value = 1, min = 1, width = '150px')),
                    column(width = 3, 
                           selectInput(inputId = "weight_factor_method", label = "Weight Factor Method", 
                                       choices = c("Equal Weight", "Inverse Varience", "Loadings from PCA"), width = '150px'))
                ),
                br(),
                fluidRow(
                    column(width = 4, uiOutput("source_field")),
                    column(width = 4, uiOutput("master_field")),
                    column(width = 2, uiOutput("stringdistmatrix_method_field")),
                    column(width = 1, uiOutput("score_weight_field"))
                )
            ),
            tabItem(
                tabName = "results",
                navbarPage(
                    title = "Matching Results", 
                    id = 'x0',
                    tabPanel(title = 'Matching Table', value = 'matching_table',
                             fluidRow(
                                 column(width = 2, actionButton("matching_button", "Action")),
                                 column(width = 2, actionButton("edit_button", "Edit")),
                                 column(width = 2, actionButton("delete_button", "Delete Filtered"))
                             ),
                             hr(),
                             DT::dataTableOutput("matched_data_table"),
                             hr(),
                             p(class = 'text-center', downloadButton('down_matched_data_filtered', 'Download Filtered Data'))
                    ),
                    tabPanel(value = 'edit_mode', title = 'Edit Mode', 
                             tableOutput("edit_data_table_source"),
                             hr(),
                             actionButton("update_button", "Update to Matching Table"),
                             hr(),
                             DT::dataTableOutput("edit_data_table_master")
                    ),
                    tabPanel(value = 'view', title = 'View Mode', 
                             verbatimTextOutput("sample_view")
                    )
                )
            )
        )
    )
)