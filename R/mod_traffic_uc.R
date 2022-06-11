mod_traffic_uc_ui <- function(id, controls){
  ns <- NS(id)
  
  tagList(
    actionButton(ns("reset_inputs"), "Reset")
    ,br()
    ,hlp_traffic_uc_inputs(config, ns, df_final)
  )
  
}

mod_traffic_uc_srv <- function(id, controls) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      observeEvent(input$reset_inputs,{
        
        for(i in 1:length(var_choices)){
          
          item_to_reset <- paste0("slider_", var_choices[i], collapse = "")
          shinyjs::reset(item_to_reset)
        }
        
      })
      
      predictions <- reactive({
        
        var_names <- var_choices

        row_to_use <- df_final %>%
          select(-rides_inbound, -county) %>%
          summarise(across(.fns = ~ mean(.x, na.rm = TRUE)))

        for(i in 1:length(var_choices)){

          this <-  input[[paste0("slider_", var_choices[i], collapse = "")]]

          if(var_choices[i] == "pctPosSent") {
            this <- this / 100
          } else if(grepl("log", var_choices[i])){
            this <- log(this)
          }
          row_to_use[[ var_names[[i]] ]] <- this
        }
        
        calc_choices <- config %>%
          filter(variable_use =='calc')
        
        for(i in 1:nrow(calc_choices)){
          calc_to_consider <- calc_choices[i, ]
          
          if(grepl("_sq", calc_to_consider$variable_name)){
            this <-  input[[paste0("slider_", jsonlite::fromJSON(calc_to_consider$preset_parameters), collapse = "")]] ^ 2
          } else if(calc_to_consider$preset_method == "default-value") {
            this <- jsonlite::fromJSON(calc_to_consider$preset_parameters)
          }
          
          row_to_use[[ calc_to_consider$variable_name ]] <- this
        }
        
        
        predicted_cases <- map_df(shps@data$Route, function(d){
          if(d %in% c("LAUS")) return(data.frame(county = d, Route = d))
          
          county_name <- crswlk[names(crswlk) == d]
          
          predictions_kept  <- predict(models[[county_name]], row_to_use, predict.all = TRUE)
          
          temp <- row_to_use %>%
            mutate(
              rides_inbound = predictions_kept$aggregate 
              ,rides_low = t.test(predictions_kept$individual)$conf.int[1]
              ,rides_high = t.test(predictions_kept$individual)$conf.int[2]
              ) %>%
            mutate(county = county_name) 
          
          temp <- temp %>%
            select(county, rides_inbound, rides_low, rides_high) %>%
            mutate(Route = d)

          return(temp)
        })

        return(predicted_cases)

      })

      return(list(
        pred = reactive({
          predictions()
          })
      ))
    }
  )
}