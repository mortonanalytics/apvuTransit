mod_traffic_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      tags$div(class="sidebar",
        sidebarPanel(
          mod_traffic_uc_ui(ns("uc"))
        )
      )
      ,mainPanel(
        fluidRow(
          column(
            12
            ,id = "map-div"
            ,tags$div(
              class = "card-av"
              ,tags$div(
                class="card-container"
                ,h3("Map of Routes")
                ,p("Click or Tap the line to see model output")
                ,hr()
                ,p("This is a tool that uses artificial intelligence algorithms to predict transit ridership 0-2 weeks in the future using inputs including advertising expenditures, weather, and the number of local COVID cases.")
              )
              ,h4(textOutput(ns("total_rides")))
              ,leafletOutput(ns("traffic"), width = "100%", height = "400px")
            )
            
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
      
      output$total_rides <- renderText({
        req(predictions$pred())
       
        temp <- sum(predictions$pred()$rides_inbound, na.rm = TRUE)
        
        final <- paste0("Total Rides: ", format(round(temp), big.mark = ",") )
       
        return(final)
      })
      
      first_render <- reactive({
        isolate(invalidateLater(1000, session))
        
        final <- Sys.time()
        message(final)
        
        return(final)
      })
      
      initial_predictions <- eventReactive(first_render(),{
        req(input[[paste0("uc-slider_", var_choices[1], collapse = "")]])
        
        var_names <- lapply(var_choices, function(i){
          if(grepl("log", i)){
            split_name <- unlist(strsplit(i, split = "_"))[1]
            var_name <- switch(
              as.character(input[["uc-lag_choice"]])
              , "1" = paste0(split_name,"_log", collapse = "")
              , "2" = paste0(split_name, "_7l_log", collapse = "")
              , "3" = paste0(split_name, "_14l_log", collapse = "")
            )
          } else {
            var_name <- switch(
              as.character(input[["uc-lag_choice"]])
              , "1" = i
              , "2" = paste0(i, "_l7", collapse = "")
              , "3" = paste0(i, "_l14", collapse = "")
            )
          }
          
          return(var_name)
        })
        
        
        row_to_use <- df_rides %>% 
          select(-date, -rides_inbound, -county) %>%
          summarise(across(.fns = ~ mean(.x, na.rm = TRUE)))
        
        for(i in 1:length(var_choices)){
          
          this <-  input[[paste0("uc-slider_", var_choices[i], collapse = "")]]
          
          if(var_choices[i] == "pctPosSent") {
            this <- this / 100
          } else if(grepl("log", var_choices[i])){
            this <- log(this)
          }
          row_to_use[[ var_names[[i]] ]] <- this
        }
        
        predicted_cases <- map_df(shps@data$Route, function(d){
          if(d %in% c("LAUS", "Ventura County")) return(data.frame(county = d, Route = d))
          temp <- row_to_use %>%
            mutate(county = crswlk[names(crswlk) == d]) %>%
            mutate(rides_inbound = predict(fit, .) ) %>%
            select(county, rides_inbound) %>%
            mutate(Route = d , diff = 0)
          
          return(temp)
        }) 
        
        new_data <- predicted_cases
        
        new_shape <- shps
        new_shape@data <- new_data
        return(new_shape)
      })
      
      output$traffic <- renderLeaflet({
        req(initial_predictions())
        
        new_data <- initial_predictions()
        
        leaflet(initial_predictions()) %>%
          addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap = TRUE)
          ) %>% 
          addPolylines(
            color = ~pal(diff)
            ,opacity = 0.8
            ,popup = ~content(Route, rides_inbound, diff)
            ,highlightOptions = highlightOptions(weight = 10,
                                                 bringToFront = TRUE)
          ) %>%
         app_legend()
      })
      
      observeEvent(predictions$pred(),{
        
        if (nrow(predictions$pred()) > 0 ) {
          
          base_preds <- initial_predictions()@data %>% select(-county)
          
          new_data <- predictions$pred() %>%
            left_join(base_preds, by = "Route") %>%
            mutate(diff = round(rides_inbound.x - rides_inbound.y)/ rides_inbound.y) %>%
            select(county, Route, rides_inbound.x, diff) %>%
            rename(rides_inbound = rides_inbound.x)
    
          new_shape <- shps
          new_shape@data <- new_data
          
          proxy <- leaflet::leafletProxy("traffic", data = new_shape)
          
          proxy %>%
            leaflet::clearShapes() %>%
            leaflet::removeControl('legend ') %>%
            leaflet::addPolylines(
              color = ~pal(diff)
              ,opacity = 0.8
              ,popup = ~content(Route, rides_inbound, diff)
              ,highlightOptions = highlightOptions(weight = 10,
                                                   bringToFront = TRUE)
            ) 
        }
        
        
        
      })
      
    }
  )
}
