shinyUI(
    navbarPage(
        #theme = bs_theme(version = 4, bootswatch = "lux")
        collapsible = TRUE
        ,title = tagList("Rebuild Ridership",actionLink("sidebar_button","",icon = icon("bars")))
        ,windowTitle = "AlphaVu | Ridership"
        ,tabPanel(
            "Metrolink Rail Lines"
            ,includeCSS("www/style.css")
            ,useShinyjs()
            ,mod_traffic_ui("traffic")
            
            )
        ,tabPanel(
            "Ridership Analysis"
            , mod_analysis_ui("analysis")
            )
        
    )
)
