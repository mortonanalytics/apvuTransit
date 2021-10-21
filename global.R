#### app runtime and UI ####
library(shiny)
library(shinyWidgets)
library(bslib)

#### map ####
library(rgdal)
library(leaflet)

#### map assets ####
shps <- readOGR("shp/2019SCRRALines.shp", layer = "2019SCRRALines", GDAL1_integer64_policy = TRUE)