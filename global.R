#### app runtime and UI ####
library(shiny)
library(bslib)
library(shinyjs)

#### map ####
library(rgdal)
library(leaflet)

#### chart ####
library(ggplot2)
library(ggthemes)
library(scales)

#### data processing ####
library(dplyr)
library(purrr)
library(glue)

#### colors ####
library(viridis)

#### map assets ####
shps <- readOGR("shp/2019SCRRALines.shp", layer = "2019SCRRALines", GDAL1_integer64_policy = TRUE)

#### data assets ###
df_rides <- read.csv("data/AV_Dashboard_Lags_Rounded_Logged.csv", stringsAsFactors = FALSE)

## repeated orange county gets duplicated when mapped to route
crswlk <- c("Antelope Valley" = "Los Angeles"
            ,"LAUS" = "LAUS"
            ,"Inland Empire-Orange County" = "Orange"
            ,"Orange County" = "Orange"
            ,"Riverside" = "Riverside"
            ,"San Bernardino" = "San Bernardino"
            ,"Ventura County" = "Ventura"
            ,"91/Perris Valley" = "Riverside")

df_crswlk <- data.frame(
  route = names(crswlk)
  ,county = crswlk
  ,row.names = 1:length(crswlk)
  ,stringsAsFactors = FALSE
)

df_final <- map_df(crswlk,function(d){
  final <- df_rides %>%
    filter(county == d) %>%
    mutate(route = names(d))
  return(final)
})

var_choices <- c(
  "Advertising Expenditure" = "spend_log"
  ,"Website Clicks" = "clicks_log"
  ,"Advertising Audience Size" = "reach_log"
  ,"Gas Price" = "gas_i"
  ,"Unemployment Rate" = "unem_i"
  ,"Precipitation" = "precip"
  ,"Daily High Temperature" = "tempMax"
  ,"Daily Low Temperature" = "tempMin"
  ,"% Positive Customer Satisfaction" = "pctPosSent"
  ,"COVID Cases" = "cases_log"
)

lag_choices <- c("None" = 1, "One Week" = 2, "Two Weeks" = 3)

#### color palette ####
color_domain <- c(0.5,-0.5)
pal <- colorNumeric(
  palette = "viridis",
  domain = color_domain,
  na.color = NA
)

#### Map legend ####
app_legend <-  function(map){
  final <- addLegend(
    map = map
    ,position = "topright"
    ,pal = pal
    ,values = color_domain
    ,title = "% Difference in Rides"
    ,opacity = 1
    ,labFormat = labelFormat(
      suffix = "%",
      transform = function(x) 100 * x
    )
    ,layerId = "legend"
  )
  
  return(final)
}

#### map pop up content ####
content <- function(d, p, c){
  
  rides <- format(round(p), big.mark = ",")
  
  diff <- paste(round(100 * c, digits = 1),"%", sep = "")
  
  html_text <- glue('
                    <b>{d}</b>
                    <p class="popup"> Rides: {rides} </p>
                    <p> Difference from the Average: {diff} </p>
                    ')
  
  return(html_text)
}

#### fit linear model ####
## (step regression didn't produce reasonable estimates - see intercept in model e.g.)
df_model <- df_rides %>%
  filter(complete.cases(.)) %>%
  select(-date)
options(set.seed = 12345, scipen = 999)

fit <- lm(rides_inbound ~ ., data = df_model)
