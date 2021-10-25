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
      
      output$traffic <- renderLeaflet({
        
        leaflet(shps) %>%
          addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap = TRUE)
          ) %>% 
          addPolylines(color = c("blue", "orange", "purple", "green", "red", "brown", "yellow")
                       ,opacity = 1
                       ,popup = ~Route
                       ,highlightOptions = highlightOptions(weight = 8,
                                                            bringToFront = TRUE)
                       )
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
          
          pal <- colorNumeric(
            palette = "viridis",
            domain = new_data$rides_inbound
          )
          
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
            ) %>%
            addLegend("topright", pal = pal, values = ~rides_inbound,
                      title = "Inbound Rides",
                      opacity = 1, layerId = "legend"
            )
        }
        
        
        
      })
      
    }
  )
}
