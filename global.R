#### app runtime and UI ####
library(shiny)
library(bslib)

#### map ####
library(rgdal)
library(leaflet)

#### chart ####
library(ggplot2)

#### data processing ####
library(dplyr)
library(purrr)

#### colors ####
library(viridis)

#### map assets ####
shps <- readOGR("shp/2019SCRRALines.shp", layer = "2019SCRRALines", GDAL1_integer64_policy = TRUE)

#### data assets ###
df_rides <- read.csv("data/AV_Dashboard_Lags_Rounded_Logged.csv", stringsAsFactors = FALSE)

## repeated orange county gets duplicated when mapped to route
crswlk <- c("Antelope Valley" = "Los Angeles"
            ,"Inland Empire-Orange County" = "Orange"
            ,"Orange County" = "Orange"
            ,"Riverside" = "Riverside"
            ,"San Bernardino" = "San Bernardino"
            #,"Ventura County" = "Ventura"
            ,"91/Perris Valley" = "Riverside")

df_crswlk <- data.frame(
  route = names(crswlk)
  ,county = crswlk
  ,row.names = 1:6
  ,stringsAsFactors = FALSE
)

df_final <- map_df(crswlk,function(d){
  final <- df_rides %>%
    filter(county == d) %>%
    mutate(route = names(d))
  return(final)
})

var_choices <- c(
  "Ad Spend" = "spend_log"
  ,"User Clicks" = "clicks_log"
  ,"Campaign Reach" = "reach_log"
  ,"Gas Price" = "gas_i"
  ,"Unemployment Rate" = "unem_i"
  ,"Precipitation" = "precip"
  ,"Daily High Temperature" = "tempMax"
  ,"Daily Low Temperature" = "tempMin"
  ,"Percent Pos Sent" = "pctPosSent"
  ,"Cases" = "cases_log"
)

lag_choices <- c("None" = 1, "One Week" = 2, "Two Weeks" = 3)

#### color palette ####
color_domain <- df_rides$rides_inbound[df_rides$rides_inbound < 1600]
pal <- colorNumeric(
  palette = "viridis",
  domain = color_domain
)

#### Map legend ####
app_legend <-  function(map){
  final <- addLegend(
    map = map
    ,position = "topright"
    ,pal = pal
    ,values = color_domain
    ,title = "Inbound Rides"
    ,opacity = 1
    ,layerId = "legend"
  )
  
  return(final)
}

#### fit linear model ####
## (step regression didn't produce reasonable estimates - see intercept in model e.g.)
df_model <- df_rides %>%
  filter(complete.cases(.)) %>%
  select(-date)
options(set.seed = 12345)

fit <- lm(rides_inbound ~ ., data = df_model)

model_coeffients <- coef(fit)
