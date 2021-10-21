mod_traffic_uc_ui <- function(id){
  ns <- NS(id)
  
  tagList(
     selectInput(ns("var_choice"), "Pick an Input", choices = var_choices)
    ,selectInput(ns("lag_choice"), "Lagged?", choices = lag_choices)
  )
  
}

mod_traffic_uc_srv <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}