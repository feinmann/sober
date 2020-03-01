# Module UI
  
#' @title   mod_display_date_statistic_ui and mod_display_date_statistic_server
#' @description  A shiny Module to display a selected date and related statistic
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_display_date_statistic
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_display_date_statistic_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(outputId = ns("date_statistic"))
  )
}
    
# Module Server
    
#' @rdname mod_display_date_statistic
#' @export
#' @keywords internal
    
mod_display_date_statistic_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_display_date_statistic_ui("display_date_statistic_ui_1")
    
## To be copied in the server
# callModule(mod_display_date_statistic_server, "display_date_statistic_ui_1")
 
