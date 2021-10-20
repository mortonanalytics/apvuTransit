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
        leaflet() %>%
          addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap = TRUE)
          ) %>%
          leaflet::addPolylines(shps, lat = ~.[,2], lng = ~.[,1])
      })
      
    }
  )
}
