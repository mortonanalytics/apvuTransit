mod_traffic_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
       column(
         2
         ,dropdown(
           mod_traffic_uc_ui(ns("uc"), 3)
         )
       )
      ,column(
         8
        ,leafletOutput(ns("traffic"), width = "100%", height = "800px")
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
          addPolylines(color = c("blue", "orange", "purple", "green", "red", "brown", "yellow"))
      })
      
    }
  )
}
