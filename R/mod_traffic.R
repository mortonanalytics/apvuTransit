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
              ,tabsetPanel(
                tabPanel(
                  "Map"
                  ,h4(textOutput(ns("total_rides")))
                  ,leafletOutput(ns("traffic"), width = "100%")
                )
                ,tabPanel(
                  "Chart"
                  ,plotOutput(ns("traffic_intervals"), width = "100%")
                )
              )
              
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

        return(final)
      })

      initial_predictions <- eventReactive(first_render(),{
        req(input[[paste0("uc-slider_", var_choices[1], collapse = "")]])

        var_names <- var_choices

        row_to_use <- df_final %>%
          select(-rides_inbound, -county) %>%
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
        
        calc_choices <- config %>%
          filter(variable_use =='calc')
        
        for(i in 1:nrow(calc_choices)){
          calc_to_consider <- calc_choices[i, ]
          
          if(grepl("_sq", calc_to_consider$variable_name)){
            this <-  input[[paste0("uc-slider_", jsonlite::fromJSON(calc_to_consider$preset_parameters), collapse = "")]] ^ 2
          } else if(calc_to_consider$preset_method == "default-value") {
            this <- jsonlite::fromJSON(calc_to_consider$preset_parameters)
          }
          
          row_to_use[[ calc_to_consider$variable_name ]] <- this
        }
        
        predicted_cases <- map_df(shps@data$Route, function(d){
          if(d %in% c("LAUS")) return(data.frame(county = d, Route = d))
          
          county_name <- crswlk[names(crswlk) == d]
          
          predictions_kept  <- predict(models[[county_name]], row_to_use, predict.all = TRUE)
          
          temp <- row_to_use %>%
            mutate(
              rides_inbound = predictions_kept$aggregate 
              ,rides_low = t.test(predictions_kept$individual)$conf.int[1]
              ,rides_high = t.test(predictions_kept$individual)$conf.int[2]
            ) %>%
            mutate(county = county_name) 
          
          temp <- temp %>%
            select(county, rides_inbound, rides_low, rides_high) %>%
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
      
      output$traffic_intervals <- renderPlot({
        req(initial_predictions())
        
        df <- initial_predictions()@data %>%
          filter(complete.cases(.))
        
        ggplot(df, aes(color = county)) +
          geom_errorbarh(aes(xmin = rides_low, xmax = rides_high, y = county),height=.4,  size = 0.5) +
          geom_point(aes(x = rides_inbound, y = county),  size = 2) +
          geom_text(aes( x = rides_high + 15, y = county, label = paste("Avg Rides:", round(rides_inbound) ) ) ) +
          scale_x_continuous(limits = c(0,2000), labels = function(y){format(y, big.mark = ",")}) +
          xlab("Rides") +
          ylab("")+
          ggthemes::theme_economist_white()+
          theme(
            legend.title=element_blank()
            ,legend.position="none"
            ,legend.text = element_text(size = 12, face = "bold")
          )
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
        
        output$traffic_intervals <- renderPlot({
          req(predictions$pred())
          
          df <- predictions$pred() %>%
            filter(complete.cases(.))
          
          ggplot(df, aes(color = county)) +
            geom_errorbarh(aes(xmin = rides_low, xmax = rides_high, y = county),height=.4,  size = 0.5) +
            geom_point(aes(x = rides_inbound, y = county),  size = 2) +
            geom_text(aes( x = rides_high + 15, y = county, label = paste("Avg Estimate:", format(round(rides_inbound),big.mark = ",") ) ), hjust = "left", nudge_y = -0.1, size = 6 ) +
            scale_x_continuous(limits = c(0,2500), labels = function(y){format(y, big.mark = ",")}) +
            xlab("Rides") +
            ylab("") +
            labs(caption = "**Bars represent 95% Confidence Interval to the Avg Estimate") +
            ggthemes::theme_economist_white()+
            theme(
              legend.title=element_blank()
              ,legend.position="none"
              ,legend.text = element_text(size = 12, face = "bold")
            )
        })


      })
      
    }
  )
}
