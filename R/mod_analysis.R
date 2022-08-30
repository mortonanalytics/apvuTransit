mod_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(12,
          fluidRow(
            column(
              3
              ,selectInput( ns( "var_choice" ), "Pick an Input", choices = var_choices)
              ,selectInput( ns("output_var"), "Pick an Output", choices = c("Rides", "Sentiment") )
              )
            )
          ,tags$div(
            class = "card-av"
            ,tags$div(
              class="card-container"
              ,h3("Training Data")
              ,p("Smoothed lines represeent trend estimates per county")
              )
            ,plotOutput(ns("analysis"), width = "100%"))
    )
        )
      )
}

mod_analysis_srv <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$analysis <- renderPlot({
        
        var_name <- input$var_choice
        
        ## TODO: make sure the references to sentiment output_var and dataset_to_use are correct
        output_var <- ifelse( input$output_var == "Rides", "rides_inbound", "sentiment")
        
        dataset_to_use <- switch(
          input$output_var
          ,"Rides" = df_model
          ,"Sentiment" = df_sentiment
        )
        
        p <- ggplot(dataset_to_use , aes_string(x = var_name, y = output_var, color = "county")) +
          geom_point(size = 3) + 
          scale_color_viridis_d(alpha = 0.8) +
          geom_smooth(method = "lm", aes(color = county),formula = 'y ~ x') + 
          ylab( input$output_var ) +
          xlab(names(var_choices[var_choices == var_name]))+
          scale_y_continuous(limits = c(0,3500), labels = function(y){format(y, big.mark = ",")}) +
          ggthemes::theme_economist_white()+
          theme(
            legend.title=element_blank()
            ,legend.position="top"
            ,legend.text = element_text(size = 12, face = "bold")
            ) + 
          guides(colour = guide_legend(nrow = 2))
        
        if(grepl("clicks|reach|cases|impressions|eng_sum", var_name)) {
          log_scale_linear_format <- function(){
            function(x){
              format(x, digits = 0, big.mark = ",")
            }
          } 
          
         p <- p +
            scale_x_continuous(labels=log_scale_linear_format()) 
        } else if(grepl("spend|gasprice", var_name)){
          dollar_log_scale_linear_format <- function(){
            function(x){
              gsub(" ", "", paste("$", format(x, digits = 0, big.mark = ","), sep=""))
            }
          } 
          
          p <- p +
            scale_x_continuous(labels=dollar_log_scale_linear_format()) 
        }else if(grepl("TMAX", var_name)){
          temp_scale_linear_format <- function(){
            function(x){
              paste(x,intToUtf8(176), sep = "")
            }
          } 
          p <- p + 
            scale_x_continuous(labels=temp_scale_linear_format()) 
        } else if(grepl("pctPosSent|unem", var_name)){
          percent_scale_linear_format <- function(){
            function(x){
              paste(x,"%", sep = "")
            }
          } 
          p <- p + 
            scale_x_continuous(labels=percent_scale_linear_format()) 
        } else if(grepl("gas", var_name)){
          message(var_name)
          dollar_scale_linear_format <- function(){
            function(x){
              paste("$",x, sep = "")
            }
          } 
          p <- p + 
            scale_x_continuous(labels=dollar_scale_linear_format()) 
        }
          p
      })
      
    }
  )
}