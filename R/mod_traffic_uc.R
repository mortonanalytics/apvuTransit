mod_traffic_uc_ui <- function(id, controls){
  ns <- NS(id)
  
  tagList(
    actionButton(ns("reset_inputs"), "Reset")
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
      
      observeEvent(input$reset_inputs,{
        lapply(var_choices, function(d){
          if(d == "pctPosSent") {
            s2_values <- round( df_rides[[d]] * 100 )
            mean_value <- round(mean(s2_values, na.rm = TRUE))
          } else if(grepl("log",d)){
            s_values <- exp(df_rides[[d]])
            mean_value <- round(mean(s_values, na.rm = T), digits = 2)
          } else{
            s_values <- df_rides[[d]]
            mean_value <- round(mean(s_values, na.rm = T), digits = 2)
          }
      
            updateSliderInput(session = session
              ,inputId = paste0("slider_", d, collapse = "")
              ,value = mean_value
            )
          
        })  
        
        updateRadioButtons(session = session, inputId = "lag_choice", selected = lag_choices[1])
      })
      
      predictions <- reactive({
        
        var_names <- lapply(var_choices, function(i){
          if(grepl("log", i)){
            split_name <- unlist(strsplit(i, split = "_"))[1]
            var_name <- switch(
              as.character(input[["lag_choice"]])
              , "1" = paste0(split_name,"_log", collapse = "")
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
          
          this <-  input[[paste0("slider_", var_choices[i], collapse = "")]]
          
          if(var_choices[i] == "pctPosSent") {
            this <- this / 100
          } else if(grepl("log", var_choices[i])){
            this <- log(this)
          }
          row_to_use[[ var_names[[i]] ]] <- this
        }
        
        predicted_cases <- map_df(shps@data$Route, function(d){
          if(d %in% c("LAUS")) return(data.frame(county = d, Route = d))
          temp <- row_to_use %>%
            mutate(county = crswlk[names(crswlk) == d]) %>%
            mutate(rides_inbound = predict(fit, .) ) %>%
            select(county, rides_inbound) %>%
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