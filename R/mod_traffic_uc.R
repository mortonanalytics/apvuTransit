mod_traffic_uc_ui <- function(id, controls){
  ns <- NS(id)
  
  tagList(
    actionButton(ns("update_model"), "Update Map!")
    ,br()
    ,br()
    ,radioButtons(ns("lag_choice"), "Forecast Period", choices = lag_choices, inline = TRUE)
    ,hlp_traffic_uc_inputs(var_choices, ns)
  )
  
}

mod_traffic_uc_srv <- function(id, controls) {
  moduleServer(
    id,
    function(input, output, session) {
      
      predictions <- reactiveValues(pred = NULL)
      
      observeEvent(input$update_model,{
        
        var_names <- lapply(var_choices, function(i){
          if(grepl("_log", i)){
            split_name <- unlist(strsplit(i, split = "_"))[1]
            var_name <- switch(
              as.character(input[["lag_choice"]])
              , "1" = split_name
              , "2" = paste0(split_name, "_7l_log", collapse = "")
              , "3" = paste0(split_name, "_14l_log", collapse = "")
            )
          } else {
            var_name <- switch(
              as.character(input[["lag_choice"]])
              , "1" = i
              , "2" = paste0(i, "_l7", collapse = "")
              , "3" = paste0(i, "_l14", collapse = "")
            )
          }
          
          return(var_name)
        })
        
        
        row_to_use <- df_rides %>% 
          select(-date, -rides_inbound, -county) %>%
          summarise(across(.fns = ~ mean(.x, na.rm = TRUE)))
        
        for(i in 1:length(var_choices)){
          row_to_use[[ var_names[[i]] ]] <- input[[paste0("slider_", var_choices[i], collapse = "")]]
        }
        
        predicted_cases <- map_df(crswlk, function(d){
          temp <- row_to_use %>%
            mutate(county = d) %>%
            mutate(rides_inbound = predict(fit, .)) %>%
            select(county, rides_inbound)
          
          return(temp)
        }) %>%
          mutate(rides_inbound = case_when(
            rides_inbound > 1600 ~ 1600
            ,rides_inbound <= 1600  ~ rides_inbound 
          ))
        
        predictions$pred <- predicted_cases
        
      })
      
      return(list(
        pred = reactive({
          predictions$pred
          })
      ))
    }
  )
}