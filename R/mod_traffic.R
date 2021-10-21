mod_traffic_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12
        ,leafletOutput(ns("traffic"), width = "100%")
      )
    )
  )
}

mod_traffic_srv <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
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
