mod_traffic_uc_ui <- function(id, controls){
  ns <- NS(id)
  
  tagList(
    actionButton(ns("update_model"), "Update Map!")
    ,hlp_traffic_uc_inputs(1:controls, ns)
  )
  
}

mod_traffic_uc_srv <- function(id, controls) {
  moduleServer(
    id,
    function(input, output, session) {
      
      lapply(1:controls, function(i){
        observeEvent({
          input[[paste0("var_choice_", i, collapse = "")]]
        }, {
          
          current_selection <- input[[paste0("var_choice_", i, collapse = "")]]
          available_selections <- var_choices[var_choices != current_selection]
          
          controls_to_update <- c(1:controls)
          controls_to_update <- controls_to_update[controls_to_update != i]
          
          walk(controls_to_update,function(e){
            
            if(input[[paste0("var_choice_", e, collapse = "")]] == current_selection){
              updateSelectInput(
                session
                ,paste0("var_choice_", e, collapse = "")
                ,choices = available_selections
              )
            }
            
          })
          
          
        })
      })
      
      lapply(1:controls, function(i){
        observeEvent({
          input[[paste0("var_choice_", i, collapse = "")]]
          input[[paste0("lag_choice_", i, collapse = "")]]
        }, {
          
          var_name <- switch(
            as.character(input[[paste0("lag_choice_", i, collapse = "")]])
            , "1" = input[[paste0("var_choice_", i, collapse = "")]]
            , "2" = paste0(input[[paste0("var_choice_", i, collapse = "")]], "_l7", collapse = "")
            , "3" = paste0(input[[paste0("var_choice_", i, collapse = "")]], "_l14", collapse = "")
          )
          
          s_values <- df_rides[[var_name]]
          
          updateSliderInput(
            session = session
            , paste0("slider_", i, collapse = "")
            , min = min(s_values, na.rm = T)
            , max = max(s_values, na.rm = T)
            , value = mean(s_values, na.rm = T))
        })
      })
      
      predictions <- reactiveValues(pred = NULL)
      
      observeEvent(input$update_model,{
        
        var_names <- lapply(1:controls, function(i){
          var_name <- switch(
            as.character(input[[paste0("lag_choice_", i, collapse = "")]])
            , "1" = input[[paste0("var_choice_", i, collapse = "")]]
            , "2" = paste0(input[[paste0("var_choice_", i, collapse = "")]], "_l7", collapse = "")
            , "3" = paste0(input[[paste0("var_choice_", i, collapse = "")]], "_l14", collapse = "")
          )
        })
        
        
        row_to_use <- df_rides %>% 
          select(-date, -rides_inbound, -county) %>%
          summarise(across(.fns = ~ mean(.x, na.rm = TRUE)))
        
        for(i in 1:controls){
          row_to_use[[ var_names[[i]] ]] <- input[[paste0("slider_", i, collapse = "")]]
        }
        
        predicted_cases <- map_df(crswlk, function(d){
          temp <- row_to_use %>%
            mutate(county = d) %>%
            mutate(rides_inbound = predict(fit, .)) %>%
            select(county, rides_inbound)
          
          return(temp)
        })
        
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