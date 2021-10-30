mod_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(12,
          fluidRow(
            column(3,selectInput( ns( "var_choice" ), "Pick an Input", choices = var_choices))
            ,column(3, selectInput( ns("lag_choice"), "Forecast Period", choices = lag_choices))
            )
          ,tags$div(
            class = "card-av"
            ,tags$div(
              class="card-container"
              ,h3("Training Data")
              ,p("Smoothed lines represeent trend estimates per county")
              )
            ,plotOutput(ns("analysis"), width = "100%", height = "400px"))
    )
        )
      )
}

mod_analysis_srv <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$analysis <- renderPlot({
        if(grepl("_log", input$var_choice)){
          split_name <- unlist(strsplit(input$var_choice, split = "_"))[1]
          var_name <- switch(
            as.character(input[["lag_choice"]])
            , "1" = input$var_choice
            , "2" = paste0(split_name, "_7l_log", collapse = "")
            , "3" = paste0(split_name, "_14l_log", collapse = "")
          )
        } else {
          var_name <- switch(
            as.character(input[["lag_choice"]])
            , "1" = input$var_choice
            , "2" = paste0(input$var_choice, "_l7", collapse = "")
            , "3" = paste0(input$var_choice, "_l14", collapse = "")
          )
        }
        
        p <- ggplot(df_rides, aes_string(x = var_name, y = "rides_inbound", color = "county")) +
          geom_point() + 
          geom_smooth(method = "lm", aes(color = county),formula = 'y ~ x') + 
          ylab("Rides") +
          xlab(names(var_choices[var_choices == var_name]))+
          scale_y_continuous(labels = function(y){format(y, big.mark = ",")})
          ggthemes::theme_economist_white()+
          theme(
            legend.title=element_blank()
            ,legend.position="top"
            ,legend.text = element_text(size = 12, face = "bold")
            ) 
        
        if(grepl("log", var_name)) {
          log_scale_linear_format <- function(){
            function(x){
              format(exp(x), digits = 0, big.mark = ",")
            }
          }
          
         p <- p +
            scale_x_continuous(labels=log_scale_linear_format()) 
        }
          p
      })
      
    }
  )
}