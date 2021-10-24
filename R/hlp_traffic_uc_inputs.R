hlp_traffic_uc_inputs <- function(v_vars, ns){
  lapply(v_vars, function(d){
   tagList( 
    selectInput( ns( paste0("var_choice_", d, collapse = "") ), "Pick an Input", choices = var_choices)
    ,selectInput(ns(paste0("lag_choice_", d, collapse = "")), "Lagged?", choices = lag_choices)
    ,sliderInput(ns(paste0("slider_", d, collapse = "")), "Pick a Value", min = 0, max = 1, value = 1)
   )
  })  
}