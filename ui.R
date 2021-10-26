shinyUI(
    navbarPage(
        #theme = bs_theme(version = 4, bootswatch = "lux")
        collapsible = TRUE
        ,title = "Rebuild Ridership"
        ,windowTitle = "AlphaVu | Ridership"
        ,tabPanel(
            "Metrolink Rail Lines"
            ,mod_traffic_ui("traffic")
            ,tags$head(tags$link(href="style.css", rel="stylesheet"))
            )
        ,tabPanel(
            "Ridership Analysis"
            , mod_analysis_ui("analysis")
            )
        
    )
)
