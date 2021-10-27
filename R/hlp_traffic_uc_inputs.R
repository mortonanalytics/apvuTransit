hlp_traffic_uc_inputs <- function(v_vars, ns){
  final <- tagList(
    lapply(v_vars, function(d){
      s_values <- df_rides[[d]]
      tagList( 
        sliderInput(
          ns(paste0("slider_", d, collapse = ""))
          ,names(v_vars[v_vars == d])
          ,min = min(s_values, na.rm = T)
          ,max = max(s_values, na.rm = T)
          ,value = mean(s_values, na.rm = T)
          ,width = "100%"
          )
        )
    })  
  )
  return(final)
  
}
