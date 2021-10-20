#### app runtime and UI ####
library(shiny)
library(shinyWidgets)
library(bslib)

#### map ####
library(rgdal)
library(leaflet)

#### map assets ####
shps <- readOGR("shp/2019SCRRALines.shp")
