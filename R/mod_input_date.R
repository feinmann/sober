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
    textOutput(ns("timediff"))
  )
}
    
# Module Server
    
#' @rdname mod_input_date
#' @export
#' @keywords internal
    
mod_input_date_server <- function(input, output, session){
  ns <- session$ns
  
  return_parole <- function(time_diff_value) {
    if (time_diff_value == 0) {
      "You are sober for 0 days."
    }  
    
    if (time_diff_value == 1) {
      "You are sober for 1 day."
    }
    
    if (time_diff_value > 1) {
      paste0("You are sober for ", time_diff_value, " days.")
    }
  }
  
  observeEvent( input$calculate , {
    time_diff_value <- difftime(Sys.Date(), input$date)
    output$timediff <- renderText(return_parole(time_diff_value))
  })
  
}
    
## To be copied in the UI
# mod_input_date_ui("input_date_ui_1")
    
## To be copied in the server
# callModule(mod_input_date_server, "input_date_ui_1")
 
