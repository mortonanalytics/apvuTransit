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
library(readr)
library(randomForest)

#### colors ####
library(viridis)

source("R/util_prepare_data.R")

#### config table ####
config <- read_csv("data/config.csv", col_types = "cccccc") %>%
  filter(variable_name != 'weekend_d')

ui_vars <- config %>%
  filter(variable_use =='ui')

var_choices <- ui_vars$variable_name
names(var_choices) <- ui_vars$variable_desc

#### map assets ####
shps <- readOGR("shp/2019SCRRALines.shp", layer = "2019SCRRALines", GDAL1_integer64_policy = TRUE)

#### data assets ###
df_rides <- read.csv("data/df_rides.csv", stringsAsFactors = FALSE)
df_agg <- read.csv("data/df_aggregates.csv", stringsAsFactors = FALSE)
route_names <- read.csv("data/df_route_names.csv", stringsAsFactors = FALSE)

df_to_use <- util_prepare_data(df_rides, df_agg, route_names)

## repeated orange county gets duplicated when mapped to route
crswlk <- c("Antelope Valley" = "Los Angeles"
            ,"Inland Empire-Orange County" = "Inland Empire"
            ,"Orange County" = "Orange"
            ,"Riverside" = "Riverside"
            ,"San Bernardino" = "San Bernardino"
            ,"Ventura County" = "Ventura"
            ,"91/Perris Valley" = "Riverside")

crswlk_abbrev <- c("Antelope Valley" = "av"
            ,"Inland Empire-Orange County" = "ie"
            ,"Orange County" = "oc"
            ,"Riverside" = "rv"
            ,"San Bernardino" = "sb"
            ,"Ventura County" = "vn"
            ,"91/Perris Valley" = "la")

df_crswlk <- data.frame(
  route_2 = names(crswlk)
  ,county_2 = crswlk
  ,abbrev = crswlk_abbrev
  ,row.names = 1:length(crswlk)
  ,stringsAsFactors = FALSE
)


df_final <- df_to_use %>%
  left_join( df_crswlk %>% select(-county_2), by = c("route" = "abbrev") ) %>%
  filter(weekend_d == 0, as.Date(date, "%Y-%m-%d") > "2021-01-01", rides_inbound > 0) %>%
  select(
    rides_inbound
    ,county
    ,any_of(config$variable_name)
    )

#### color palette ####
color_domain <- c(0.65,-0.65)
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
    ,opacity = 0.8
    ,labFormat = labelFormat(
      suffix = "%",
      prefix = " ", 
      transform = function(x)  100 * x
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
df_model <- df_final %>%
  filter(complete.cases(.)) 

options(set.seed = 12345, scipen = 99)

models <- df_model %>%
  split(df_model$county) %>% 
  map(. %>% select(-county) ) %>%
  map(~ randomForest(rides_inbound ~ ., data = . ) )
