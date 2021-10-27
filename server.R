shinyServer(function(input, output, session) {
  mod_traffic_srv("traffic")
  mod_analysis_srv("analysis")
  
  observeEvent(input$sidebar_button,{
    shinyjs::toggle(selector = ".tab-pane.active div:has(> [role='complementary'])")
    
    js_maintab <- paste0('$(".tab-pane.active div[role=',"'main'",']")')
    
    runjs(paste0('
          width_percent = parseFloat(',js_maintab,'.css("width")) / parseFloat(',js_maintab,'.parent().css("width"));
          if (width_percent == 1){
            ',js_maintab,'.css("width","");
          } else {
            ',js_maintab,'.css("width","100%");
          }
          '))
  })
})
