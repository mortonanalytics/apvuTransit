util_prep_sentiment <- function(df_rides, df_agg, df_sentiment, route_names){
  
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
  
  df_sentiment_cleaned <- df_sentiment %>% 
    select(-total_customers) %>% 
    mutate(date = (as.Date(date,"%m/%d/%Y"))) %>% 
    tidyr::pivot_longer(cols = c('la','ie','av','vc','rv','sb','oc'), names_to = "route")
  
  df_long <- df_joined %>%
    mutate(date = (as.Date(date,"%Y-%m-%d"))) %>% 
    tidyr::pivot_longer(cols = dplyr::all_of(widen_on), names_to = "route", values_to = "rides_inbound") %>%
    dplyr::left_join(df_routes, by = "route") %>%
    dplyr::left_join(df_sentiment_cleaned, by=c("date" = "date","route"="route")) %>% 
    dplyr::distinct(dplyr::across())
  

  
  return(df_long)
}