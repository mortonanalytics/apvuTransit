hlp_traffic_uc_inputs <- function(v_vars, ns){
  final <- tagList(
    lapply(v_vars, function(d){
      if(d == "pctPosSent") {
        s_values <- 0:100
        s2_values <- round( df_rides[[d]] * 100 )
        mean_value <- round(mean(s2_values, na.rm = TRUE))
        min_value <- 0
        max_value <- 100
      } else if(grepl("log",d)){
        s_values <- exp(df_rides[[d]])
        mean_value <- round(mean(s_values, na.rm = T), digits=2)
        min_value <- max(round(mean_value - ( sd(s_values, na.rm = T)), digits = 2), round(min(s_values, na.rm = T), digits = 2))
        max_value <- min(round(mean_value + ( sd(s_values, na.rm = T)), digits = 2), round(max(s_values, na.rm = T), digits = 2))
        
      }else{
        s_values <- df_rides[[d]]
        mean_value <- round(mean(s_values, na.rm = T), digits = 2)
        min_value <- max(round(mean_value - ( sd(s_values, na.rm = T)), digits = 2), round(min(s_values, na.rm = T), digits = 2))
        max_value <- min(round(mean_value + ( sd(s_values, na.rm = T)), digits = 2), round(max(s_values, na.rm = T), digits = 2))
      }
      
      tagList( 
        sliderInput(
          ns(paste0("slider_", d, collapse = ""))
          ,names(v_vars[v_vars == d])
          ,min = min_value
          ,max = if(max_value> 1) round(max_value) else max_value
          ,value = mean_value
          ,ticks = FALSE
          ,round = if(d %in% c("clicks_log", "reach_log", "cases")) TRUE else FALSE
          ,post = if(d %in% c("pctPosSent", "unem_i")) "%" else if(grepl("temp", d)) intToUtf8(176) else ""
          ,pre = if(d %in% c("spend_log", "gas_i")) "$" else ""
          ,width = "100%"
          )
        )
    })  
  )
  return(final)
  
}
