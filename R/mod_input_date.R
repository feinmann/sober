# Module UI
  
#' @title   mod_input_date_ui and mod_input_date_server
#' @description  A shiny Module to input a date using a date-selector
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_input_date
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_input_date_ui <- function(id){
  ns <- NS(id)
  tagList(
    dateInput(
      inputId = ns("date"), 
      label = "Input date"
    ),
    actionButton(
      inputId = ns("calculate"),
      label = "Calculate statistics"
    ),
    hr(),
    DT::dataTableOutput(outputId = ns('result_data'))
  )
}
    
# Module Server
    
#' @rdname mod_input_date
#' @export
#' @keywords internal
    
mod_input_date_server <- function(input, output, session){
  ns <- session$ns
  
  observeEvent( input$calculate , {
    result_data <- calculate_triples(input$date)
    output$result_data <- DT::renderDataTable({ result_data })
  })
  
}
    
## To be copied in the UI
# mod_input_date_ui("input_date_ui_1")
    
## To be copied in the server
# callModule(mod_input_date_server, "input_date_ui_1")
 
