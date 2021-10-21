hlp_route_crosswalk <- function(route) {
  crswlk <- c("Antelope Valley" = "Los Angeles"
              ,"Inland Empire-Orange County" = "Orange"
              ,"Orange County" = "Orange"
              ,"Riverside" = "Riverside"
              ,"San Bernardino" = "San Bernardino"
              ,"Ventura County" = "Ventura"
              ,"91/Perris Valley" = "Riverside")
  
  final <- crswlk[route]
  
  return(final)
}


