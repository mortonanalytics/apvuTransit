mod_traffic_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        mod_traffic_uc_ui(ns("uc"), 3)
      )
      ,mainPanel(
        fluidRow(
          column(
            12
            ,h3("Map of Routes")
            ,p("Click or Tap the line to see model output")
            ,leafletOutput(ns("traffic"), width = "100%", height = "700px")
            )
          )
        )
      )
    )
}

mod_traffic_srv <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      predictions <- mod_traffic_uc_srv("uc", 3)
      
      initial_predictions <- reactive({
        
        row_to_use <- df_rides %>% 
          select(-date, -rides_inbound, -county) %>%
          summarise(across(.fns = ~ mean(.x, na.rm = TRUE)))
        
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
        
        new_data <- shps@data %>%
          inner_join(df_crswlk, by = c("Route" = "route")) %>%
          inner_join(predicted_cases, by = "county") %>%
          distinct(.keep_all = TRUE) %>%
          filter(complete.cases(.)) 
        
        new_shape <- shps
        new_shape@data <- new_data
        return(new_shape)
      })
      
      output$traffic <- renderLeaflet({
        req(initial_predictions())
        
        leaflet(initial_predictions()) %>%
          addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap = TRUE)
          ) %>% 
          addPolylines(
            color = ~pal(rides_inbound)
            ,opacity = 1
            ,popup = ~Route
            ,highlightOptions = highlightOptions(weight = 8,
                                                 bringToFront = TRUE)
          ) %>%
         app_legend()
      })
      
      observeEvent(predictions$pred(),{
        
        if (nrow(predictions$pred()) > 0 ) {
          
          new_data <- shps@data %>%
            inner_join(df_crswlk, by = c("Route" = "route")) %>%
            inner_join(predictions$pred(), by = "county") %>%
            distinct(.keep_all = TRUE) %>%
            filter(complete.cases(.))
    
          new_shape <- shps
          new_shape@data <- new_data
          
          proxy <- leaflet::leafletProxy("traffic", data = new_shape)
          
          proxy %>%
            leaflet::clearShapes() %>%
            leaflet::removeControl('legend ') %>%
            leaflet::addPolylines(
              color = ~pal(rides_inbound)
              ,opacity = 1
              ,popup = ~Route
              ,highlightOptions = highlightOptions(weight = 8,
                                                   bringToFront = TRUE)
            ) 
        }
        
        
        
      })
      
    }
  )
}
