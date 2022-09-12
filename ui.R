shinyUI(
    navbarPage(
        title = tagList("Rebuilding Ridership (Beta)",actionLink("sidebar_button","",icon = icon("bars")))
        ,collapsible = FALSE
        ,windowTitle = "AlphaVu | Ridership"
        ,tabPanel(
            "Predicting Ridership"
            ,includeCSS("www/style.css")
            ,useShinyjs()
            ,tags$head(tags$script(type="text/javascript", src = "append_logo.js"))
            ,mod_traffic_ui("traffic", "rides_inbound")
            )
        ,tabPanel(
          "Predicting Customer Sentiment"
          ## TODO: convert this to sentiment output variable name when ready
          ,mod_traffic_ui("sentiment", "value")
        )
        ,tabPanel(
            "Ridership Analysis"
            , mod_analysis_ui("analysis")
            )
        
    )
)
