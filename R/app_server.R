#' @import shiny
app_server <- function(input, output, session) {
  # List the first level callModules here
  callModule(mod_input_date_server, id = "input_date_ui_1")
  callModule(mod_choose_substance_server, id = "choose_substance_ui_1")
}
