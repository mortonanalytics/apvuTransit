hlp_traffic_uc_inputs <- function(config, ns, data){
  
  ui_vars <- config$variable_name[config$variable_use == 'ui']
  
  final <- tagList(
    
    purrr::map(ui_vars, function(d){
      
      config_to_use <- config[config$variable_name == d,]
      
      switch(
        config_to_use$ui_elemenet
        ,"slider" = hlp_ui_slider(config_to_use, ns, data)
      )
      
    })
  )
    
  return(final)
  
}
