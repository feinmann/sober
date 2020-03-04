# Module UI
  
#' @title   mod_choose_substance_ui and mod_choose_substance_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_choose_substance
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_choose_substance_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             selectInput(ns("select"), label = "Choose substance: ", 
                         choices = list("Please select ..." = 0, 
                                        "Chocolate" = 1, 
                                        "Meat" = 2, 
                                        "Drugs" = 3), 
                         selected = 0),
             conditionalPanel(
               condition = "input.select != '0'", ns = ns,
               textInput(ns("daily_consumption"), "Daily cost: ", value = "...")
             )
      ),
      column(8,
             conditionalPanel(
               condition = "input.select != '0'", ns = ns,
               infoBoxOutput(ns("saved_cost"))
             )
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_choose_substance
#' @export
#' @keywords internal
    
mod_choose_substance_server <- function(input, output, session, sober_time){
  ns <- session$ns
  
  daily_costs <- data.table(
    substance_id = c(1, 2, 3),
    substance_name = c("Chocolate", "Meat", "Drugs"),
    daily_cost_message = c("1.20 € per day.", "3.30 € per day.", "10.00 € per day."),
    daily_cost = c(1.2, 3.3, 10.0))
  
  observeEvent(input$select, {
    new_value <- daily_costs[substance_id == input$select, daily_cost_message]
    updateTextInput(session, "daily_consumption", value = new_value)
    daily_cost <- daily_costs[substance_id == input$select, daily_cost]
      output$saved_cost <- renderValueBox({ 
        valueBox(
          value = "$ CASH:",
          subtitle = paste("You saved ", daily_cost * sober_time(), "€ so far."),
          icon = icon("money-bill-alt"),
          width = 8,
          color = "green"
        ) 
      })
    })
}
    
## To be copied in the UI
# mod_choose_substance_ui("choose_substance_ui_1")
    
## To be copied in the server
# callModule(mod_choose_substance_server, "choose_substance_ui_1")
 
