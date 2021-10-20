shinyUI(
    navbarPage(
        theme = bs_theme(version = 4, bootswatch = "lux")
        ,collapsible = TRUE
        ,title = "Guess My Traffic"
        ,windowTitle = "AlphaVu | Ridership"
        ,tabPanel(
            "Traffic Lines"
            ,mod_traffic_ui("traffic")
            )
        ,tabPanel(
            "Ridership Analysis"
            )
    )
)
