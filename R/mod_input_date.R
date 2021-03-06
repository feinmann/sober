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
    fluidRow(
      
      column(4,
             dateInput(
               inputId = ns("date"), 
               label = "Clean date",
               value = "2018-04-07"
             ),
             actionButton(
               inputId = ns("calculate"),
               label = "Calculate statistics"
             )       
      ),
      
      column(8, 
             mod_choose_substance_ui("choose_substance_ui_1")
      )
    ),
    br(),
    fluidRow(valueBoxOutput(ns("sober_time"))), 
    fluidRow(
      valueBoxOutput(ns("singles")),
      valueBoxOutput(ns("doubles")),
      valueBoxOutput(ns("triples")),
      valueBoxOutput(ns("quartruples")) 
    ),
    hr(),
    h3("Today events"),
    hr(),
    DT::dataTableOutput(outputId = ns('today_events')),
    h3("Last events"),
    DT::dataTableOutput(outputId = ns('last_events')),
    hr(),
    h3("Upcoming events..."),
    DT::dataTableOutput(outputId = ns('next_events')),
    hr(),
    h3("All events"),
    DT::dataTableOutput(outputId = ns('all_events'))
  )
}
    
# Module Server
    
#' @rdname mod_input_date
#' @export
#' @keywords internal
    
mod_input_date_server <- function(input, output, session){
  ns <- session$ns
  
  observeEvent( input$calculate , {
    
    all_events <- calculate_events(input$date)
    
    last_events <- get_last_events(all_events)
    next_events <- get_next_events(all_events)
    today_events <- get_today_events(all_events)
    
    output$today_events <- DT::renderDataTable({ 
        
        validate(need(nrow(today_events) > 0, "Today no events, I'm afraid. Keep going!"))
        
        today_events }, options = list(scrollX = TRUE))
    
    output$all_events <- DT::renderDataTable({ all_events },
                                             options = list(scrollX = TRUE))
    output$last_events <- DT::renderDataTable({ last_events },
                                              options = list(scrollX = TRUE))
    output$next_events <- DT::renderDataTable({ next_events },
                                              options = list(scrollX = TRUE))
    output$sober_time <- renderValueBox({ 
                            valueBox(
                              value = "You can do it",
                              subtitle = tags$p(paste("You are sober for ", 
                                               lubridate::today() - input$date, 
                                               " days"), style = "font-size: 200%;"),
                              icon = icon("refresh"),
                              width = 8,
                              color = "green"
                            ) 
                         })
    output$singles <- renderValueBox({ 
      valueBox(
        value = "Singles",
        subtitle = tags$p(paste("You collected ", 
                         count_singles(all_events), 
                         " singles so far"), style = "font-size: 200%;"),
        icon = icon("gratipay"),
        width = 8,
        color = "purple"
      ) 
    })
    output$doubles <- renderValueBox({ 
                           valueBox(
                             value = "Doubles",
                             subtitle = tags$p(paste("You collected ", 
                                              count_doubles(all_events), 
                                              " doubles so far"), style = "font-size: 200%;"),
                             icon = icon("check-double"),
                             width = 8,
                             color = "light-blue"
                           ) 
                         })
    output$triples <- renderValueBox({ 
                           valueBox(
                             value = "Triples",
                             subtitle = tags$p(paste("You collected ", 
                                              count_triples(all_events), 
                                              " triples so far"), style = "font-size: 200%;"),
                             icon = icon("dice-three"),
                             width = 8,
                             color = "teal"
                           ) 
                         })
    output$quartruples <- renderValueBox({ 
                           valueBox(
                             value = "Quartruples",
                             subtitle = tags$p(paste("You collected ", 
                                              count_quartruples(all_events), 
                                              " quartruple so far"), style = "font-size: 200%;"),
                             icon = icon("fort-awesome"),
                             width = 8,
                             color = "olive"
                           ) 
                         })
  })

  reactive(return(lubridate::today() - input$date))
}
    
## To be copied in the UI
# mod_input_date_ui("input_date_ui_1")
    
## To be copied in the server
# callModule(mod_input_date_server, "input_date_ui_1")
 
