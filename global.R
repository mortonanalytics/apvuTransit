#### app runtime and UI ####
library(shiny)
library(shinyWidgets)
library(bslib)

#### map ####
library(rgdal)
library(leaflet)

#### data processing ####
library(dplyr)
library(purrr)

#### map assets ####
shps <- readOGR("shp/2019SCRRALines.shp", layer = "2019SCRRALines", GDAL1_integer64_policy = TRUE)

#### data assets ###
df_rides <- read.csv("data/AV_Dashboard_Lags.csv", stringsAsFactors = FALSE)

## repeated orange county gets duplicated when mapped to route
crswlk <- c("Antelope Valley" = "Los Angeles"
            ,"Inland Empire-Orange County" = "Orange"
            ,"Orange County" = "Orange"
            ,"Riverside" = "Riverside"
            ,"San Bernardino" = "San Bernardino"
            #,"Ventura County" = "Ventura"
            ,"91/Perris Valley" = "Riverside")

df_final <- map_df(crswlk,function(d){
  final <- df_rides %>%
    filter(county == d) %>%
    mutate(route = names(d))
  return(final)
})

var_choices <- c(
  "Ad Spend" = "spend"
  ,"User Clicks" = "clicks"
  ,"Campaign Reach" = "reach"
  ,"Gas Price" = "gas_i"
  ,"Unemployment Rate" = "gas_i"
  ,"Precipitation" = "precip"
  ,"Daily High Temperature" = "tempMax"
  ,"Daily Low Temperature" = "tempMin"
  ,"Percent Pos Sent" = "pctPosSent"
  ,"Cases" = "cases"
)

lag_choices <- c("None" = 1, "One Week" = 2, "Two Weeks" = 3)

#### fit linear model ####
## (step regression didn't produce reasonable estimates - see intercept in model e.g.)
df_model <- df_rides %>%
  filter(complete.cases(.)) %>%
  select(-date)
options(set.seed = 12345)

fit <- lm(rides_inbound ~ ., data = df_model)

model_coeffients <- coef(fit)
