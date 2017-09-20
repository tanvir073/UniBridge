library(shiny)
library(shinydashboard)
library(xlsx)
library(RODBC)
library(stringdist)
library(xtable)
library(DT)

load("C://Users//Arif.Rahman//Desktop//Bridging Mechanism Tools//sample_dat_Master_Source.RData")
r_matching_methods <- c("osa", "lv", "dl", "hamming", "lcs", "qgram","cosine", "jaccard", "jw", "soundex")

shinyServer(function(input, output, session) {
    
# master_sql_connection <- reactive({odbcDriverConnect(connection=paste("Driver={SQL Server};server=",input$master_sql_server,";UID=",input$master_sql_user,";PWD=",input$master_sql_pass,";", sep = ""))})

#load master data
master_data_full <- reactive({master_dat}) # reactive({as.data.frame(sqlQuery(master_sql_connection(),input$master_sql_query))})


    #get source file
    source_file <- reactive({input$source_file_load})
    #get source data
    source_data <- reactive({source_dat}) #reactive({read.xlsx(source_file()$datapath,1)})
    #get countrywise master data
    
    output$source_field <- renderUI({
        if (is.null(source_data())){return(NULL)}
        else{
            gen_code <- NULL
            for(i in 1:input$number_attrib_match){
                gen_code <- paste(
                    gen_code, 
                    paste('selectizeInput(inputId = "get_source_field_',i,'", label = "',
                          ifelse(i==1,"Select Source Field"," "),'", choices = c(as.vector(names(source_data())),""), 
                          multiple = TRUE, 
                          options = list(maxItems = length(names(source_data()))))', 
                          sep = "")
                    ,sep = ",")
            }
            gen_code <- paste('list(',substr(gen_code,2,nchar(gen_code)),')', sep = "")
            eval(parse(text = paste(gen_code)))
        } 
    })
    
    output$master_field <- renderUI({
        if (is.null(master_data_full())){return(NULL)}
        else{
            gen_code <- NULL
            for(i in 1:input$number_attrib_match){
                gen_code <- paste(
                    gen_code,
                    paste('selectizeInput(inputId = "get_master_field_',i,'", label = "',
                          ifelse(i==1,"Select Master Field"," "),'", choices = c(as.vector(names(master_data_full())),""), 
                          multiple = TRUE, 
                          options = list(maxItems = length(names(master_data_full()))))', 
                          sep = "")
                    ,sep = ",")
            }
            gen_code <- paste('list(',substr(gen_code,2,nchar(gen_code)),')', sep = "")
            eval(parse(text = paste(gen_code)))
        } 
    })
    
    output$stringdistmatrix_method_field <- renderUI({
        gen_code <- NULL
        for(i in 1:input$number_attrib_match){
            gen_code <- paste(
                gen_code, 
                paste('selectInput(inputId = "stringdistmatrix_method_field_',i,'", label = "',
                      ifelse(i==1,"Methods"," "),'", choices = r_matching_methods, width = \'100px\')', 
                      sep = "")
                ,sep = ",")
        }
        gen_code <- paste('list(',substr(gen_code,2,nchar(gen_code)),')', sep = "")
        eval(parse(text = paste(gen_code)))
    })
    
    output$score_weight_field <- renderUI({
        gen_code <- NULL
        for(i in 1:input$number_attrib_match){
            gen_code <- paste(
                gen_code, 
                paste('textInput(inputId = "score_weight_field_',i,'", label = "',
                      ifelse(i==1,"Weight"," "),'", value = "1", width = \'50px\')',
                      sep = "")
                ,sep = ",")
        }
        gen_code <- paste('list(',substr(gen_code,2,nchar(gen_code)),')', sep = "")
        eval(parse(text = paste(gen_code)))
    })
    
    matched_data_final <- reactiveValues()
    
    observeEvent(input$matching_button,{
        gen_code <- NULL
        for(i in 1:input$number_attrib_match){
            gen_code <- paste("matched_data_final$scor_mat_",i," <- as.numeric(input$score_weight_field_",i,")*stringdistmatrix(
                a = tolower(eval(parse(text = paste(\'with(data = source_data() ,paste(\',paste(input$get_source_field_",i,", collapse = \",\"),\'))\',sep = \"\")))), 
                b = tolower(eval(parse(text = paste(\'with(data = master_data_full() ,paste(\',paste(input$get_master_field_",i,", collapse = \",\"),\'))\',sep = \"\")))), method = input$stringdistmatrix_method_field_",i,")",
                sep = "")
            eval(parse(text =  paste(gen_code)))
        }
        # gen_code <- substr(gen_code,3,nchar(gen_code))
        
        
        scor_mat_array <- eval(parse(text = paste("array(data = c(",noquote(paste(paste("as.vector(matched_data_final$scor_mat_",1:input$number_attrib_match,")", sep = ""), collapse = ",")),"), dim = c(nrow(source_data()),nrow(master_data_full()),input$number_attrib_match))",sep = "")))
        
        dist_mat <- switch(input$weight_factor_method,
               "Equal Weight" = eval(parse(text =  paste(paste("as.matrix(matched_data_final$scor_mat_",1:input$number_attrib_match,")", sep = ""),collapse = " + "))), 
               "Inverse Varience" = , 
               "Loadings from PCA" = t(apply(scor_mat_array,1, function(y) (prcomp(y, scale. = FALSE))$x[,1]))
        )
        
        match_index <- apply(dist_mat, 1,which.min)
        match_index <- mapply(function(x,y) c(x,y)[1], match_index, 0)
        
        matched_data_final$dist_mat <- dist_mat
        
        gen_code <- NULL
        for(i in 1:input$number_attrib_match){
            gen_code <- paste(
                gen_code,
                paste("source_field_",i," = eval(parse(text = paste(\'with(data = source_dat ,paste(\',paste(input$get_source_field_",i,", collapse = \",\"),\'))\',sep = \"\"))), 
                      master_field_",i," = eval(parse(text = paste(\'with(data = master_data_full() ,paste(\',paste(input$get_master_field_",i,", collapse = \",\"),\'))\',sep = \"\")))[match_index],
                      score = mapply(function(x,y) c(x,y)[1], apply(matched_data_final$scor_mat_",i,", 1,min),0)", 
                      sep = ""),
                sep = " , "
            )
        }
        
        gen_code <- paste('data.frame(',substr(gen_code,3,nchar(gen_code)),', score = mapply(function(x,y) c(x,y)[1], apply(dist_mat, 1,min),0))', sep = "")
        matched_data_all <- eval(parse(text =  paste(gen_code)))
        
        gen_code <- NULL
        for(i in 1:input$number_attrib_match){
            gen_code <- paste(
                gen_code,
                paste("paste(input$get_source_field_",i,", collapse = \"_\"),
                      paste(input$get_master_field_",i,", collapse = \"_\"),
                      paste(\"score_",i,"\", sep = \"\")",
                      sep = ""),
                sep = " , "
            )
        }
        
        colnames(matched_data_all) <- eval(parse(text =  paste(paste('c(',substr(gen_code,4,nchar(gen_code)),',"Overall Score")', sep = ""))))
        
        matched_data_final$matched_data_main <- matched_data_all
        
        
    })
    
    observeEvent(input$edit_button, {
        info = input$matched_data_table_rows_selected
        if (is.null(info)) return()
        
        matched_data_final$edit_source_data <- matched_data_final$matched_data_main[info[length(info)],]
        
        gen_code <- NULL
        for(i in 1:input$number_attrib_match){
            gen_code <- paste(
                gen_code,
                paste("master_field_",i," = eval(parse(text = paste(\'with(data = master_data_full() ,paste(\',paste(input$get_master_field_",i,", collapse = \",\"),\'))\',sep = \"\")))[sort.int(matched_data_final$dist_mat[info[length(info)],],index.return = TRUE)$ix[1:20]],
                      score = matched_data_final$scor_mat_",i,"[info[length(info)],sort.int(matched_data_final$dist_mat[info[length(info)],],index.return = TRUE)$ix[1:20]]", 
                      sep = ""),
                sep = " , "
            )
        }
        
        gen_code <- paste('data.frame(',substr(gen_code,3,nchar(gen_code)),', score = matched_data_final$dist_mat[info[length(info)],sort.int(matched_data_final$dist_mat[info[length(info)],],index.return = TRUE)$ix[1:20]])', sep = "")
        matched_data_final$edit_master_data <- eval(parse(text =  paste(gen_code)))
        
        gen_code <- NULL
        for(i in 1:input$number_attrib_match){
            gen_code <- paste(
                gen_code,
                paste("paste(input$get_master_field_",i,", collapse = \"_\"),
                      paste(\"score_",i,"\", sep = \"\")",
                      sep = ""),
                sep = " , "
                      )
        }
        
        colnames(matched_data_final$edit_master_data) <- eval(parse(text =  paste(paste('c(',substr(gen_code,4,nchar(gen_code)),',"Overall Score")', sep = ""))))
        
        updateTabsetPanel(session, 'x0', selected = 'edit_mode')
    })
    
    observeEvent(input$update_button, {
        info1 = input$edit_data_table_master_rows_selected
        info2 = input$matched_data_table_rows_selected
        if (is.null(info1) || is.null(info2)) return()
        
        matched_data_final$matched_data_main[,1:(3*input$number_attrib_match+1) %!in% seq(from = 1, by = 3, length.out = input$number_attrib_match)] = apply(matched_data_final$matched_data_main[,1:(3*input$number_attrib_match+1) %!in% seq(from = 1, by = 3, length.out = input$number_attrib_match)], 2, function(x) as.character(x))
        gen_code <- NULL
        for(i in 1:input$number_attrib_match){
            eval(parse(text = paste("matched_data_final$matched_data_main[info2[length(info2)],(3*",i,"-1)] <- as.character(matched_data_final$edit_master_data[info1[length(info1)],]$",colnames(matched_data_final$matched_data_main)[3*i-1],")", sep = "")))
            eval(parse(text = paste("matched_data_final$matched_data_main[info2[length(info2)],(3*",i,")] <- as.character(matched_data_final$edit_master_data[info1[length(info1)],]$",colnames(matched_data_final$matched_data_main)[3*i],")", sep = "")))
        }
        
        matched_data_final$matched_data_main[info2[length(info2)],(3*input$number_attrib_match + 1)] <- as.character(matched_data_final$edit_master_data[info1[length(info1)],(2*input$number_attrib_match + 1)])
        
        updateTabsetPanel(session, 'x0', selected = 'matching_table')
    })
    
    observeEvent(input$delete_button, {
        info = input$matched_data_table_rows_all
        if (is.null(info)) return()
        
        matched_data_final$matched_data_main <- matched_data_final$matched_data_main[-info,]
        
        updateTabsetPanel(session, 'x0', selected = 'matching_table')
    })
    
    output$matched_data_table <- DT::renderDataTable({
        if(is.null(matched_data_final$matched_data_main))return()
        DT::datatable(data = matched_data_final$matched_data_main, filter = 'top', rownames = TRUE, selection = 'single',
                      options = list(lengthMenu = c(10,30,50), pageLength = 30)
        ) %>% formatStyle(0, cursor = 'pointer')
    }, server = FALSE)
    
    output$edit_data_table_source <- renderTable({
        data = matched_data_final$edit_source_data
    })
    
    output$edit_data_table_master <- DT::renderDataTable({
        if(is.null(matched_data_final$edit_master_data))return()
        DT::datatable(data = matched_data_final$edit_master_data, rownames = TRUE, selection = 'single',
                      options = list(lengthMenu = c(10,30,50), pageLength = 30)
        ) %>% formatStyle(0, cursor = 'pointer')
    }, server = FALSE)
    
    output$sample_view <- renderPrint({
        info1 = input$edit_data_table_master_rows_selected
        info2 = input$matched_data_table_rows_selected
        colnames(matched_data_final$matched_data_main[info2[length(info2)],])
        eval(parse(text =  paste(paste("matched_data_final$scor_mat_",1:3, sep = ""),collapse = " + ")))[1,]
    })
    
    output$down_matched_data_filtered <- downloadHandler(
        filename = 'matched_filtered.csv', 
        content = function(file) {
            write.table(matched_data_final$matched_data_main[input$matched_data_table_rows_all,], file, sep = "|", row.names = FALSE)
        },
        contentType = ".csv"
    )
    
    output$down_matched_data_selected <- downloadHandler(
        filename = 'matched_selected.csv', 
        content = function(file) {
            write.table(matched_data_final$matched_data_main[input$matched_data_table_rows_selected,], file, sep = "|", row.names = FALSE)
        },
        contentType = ".csv"
    )
    
})
