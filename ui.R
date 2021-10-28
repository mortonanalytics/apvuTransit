shinyUI(
    navbarPage(
        title = tagList("Rebuilding Ridership (Beta)",actionLink("sidebar_button","",icon = icon("bars")))
        ,collapsible = FALSE
        ,windowTitle = "AlphaVu | Ridership"
        ,tabPanel(
            "Metrolink Rail Lines"
            ,includeCSS("www/style.css")
            ,useShinyjs()
            ,tags$head(tags$script(type="text/javascript", src = "append_logo.js"))
            ,mod_traffic_ui("traffic")
            )
        ,tabPanel(
            "Ridership Analysis"
            , mod_analysis_ui("analysis")
            )
        
    )
)
