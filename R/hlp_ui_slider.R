hlp_ui_slider <- function(config_to_use, ns, data){
 
  params <- jsonlite::fromJSON(config_to_use$preset_parameters)
  
  min_value <- ifelse(
    config_to_use$preset_method == "default-range"
    ,min(params)
    ,round( quantile(data[[config_to_use$variable_name]], probs = min(params)/100, na.rm = T) )
  )
  
  max_value <- ifelse(
    config_to_use$preset_method == "default-range"
    ,max(params)
    ,round( quantile(data[[config_to_use$variable_name]], probs = max(params)/100, na.rm = T) )
  )
  
  
  mid_value <- round( quantile(data[[config_to_use$variable_name]], probs = .5, na.rm = T) )
  
  tagList(
    sliderInput(
      ns(paste0("slider_", config_to_use$variable_name, collapse = ""))
      ,config_to_use$variable_desc
      ,min = min_value
      ,max = max_value
      ,value = mid_value
      ,ticks = FALSE
      ,round = if(config_to_use$variable_name %in% c("clicks", "reach", "cases", "eng_sum")) TRUE else FALSE
      ,post = if(config_to_use$variable_name %in% c("pctPosSent", "unem_i")) "%" else if(grepl("temp", config_to_use$variable_name)) intToUtf8(176) else ""
      ,pre = if(config_to_use$variable_name %in% c("spend", "gasprice")) "$" else ""
      ,width = "100%"
    )
  )
}