util_prepare_data <- function(df_rides, df_agg, route_names){
  
  df_agg_cleaned <- df_agg %>%
    dplyr::select(-weekend_d) %>%
    dplyr::distinct(dplyr::across())
  
  df_joined <- df_rides %>%
    dplyr::distinct(dplyr::across()) %>%
    dplyr::inner_join(df_agg_cleaned, by = "date") %>%
    dplyr::distinct(dplyr::across())
  
  df_routes <- route_names %>%
    dplyr::distinct(dplyr::across())
  
  widen_on <- df_routes$route
  
  df_long <- df_joined %>%
    tidyr::pivot_longer(cols = dplyr::all_of(widen_on), names_to = "route", values_to = "rides_inbound") %>%
    dplyr::left_join(df_routes, by = "route") %>%
    dplyr::distinct(dplyr::across())
  
  return(df_long)
}