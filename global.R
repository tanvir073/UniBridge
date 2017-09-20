textareaInput <- function(id, label, value=NULL, rows=20, cols=35, class="form-control"){
    tags$div(
        class="form-group shiny-input-container",
        tags$label('for'=id,label),
        tags$textarea(id=id,class=class,rows=rows,cols=cols,value))
}


'%!in%' <- function(x,y)!('%in%'(x,y))



