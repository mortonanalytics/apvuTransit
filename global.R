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
source('R/util_prep_sentiment.R')
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

## TODO: add sentiment dataset as a unique data set for now, combine with df_agg, see if we can combine on-time performance (OTP)
df_rides <- read.csv("data/df_rides.csv", stringsAsFactors = FALSE)
df_agg <- read.csv("data/df_aggregates.csv", stringsAsFactors = FALSE)
route_names <- read.csv("data/df_route_names.csv", stringsAsFactors = FALSE)
df_sentiment <- read.csv("data/avg_sentiment.csv", stringsAsFactors = FALSE)
df_to_use <- util_prepare_data(df_rides, df_agg, route_names)
df_sent_use <- util_prep_sentiment(df_rides,df_agg,df_sentiment,route_names)



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

df_final_sent <- df_sent_use %>% 
  left_join( df_crswlk %>% select(-county_2), by = c("route" = "abbrev") ) %>%
  filter(weekend_d == 0, as.Date(date, "%Y-%m-%d") > "2021-01-01", rides_inbound > 0) %>%
  select(
    value
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

#dummy needs to change to value if not using dummy vars
content <- function(d, p, c, output_var){
  printed_value <- switch(
    output_var
    ,"rides_inbound" = format(round(p), big.mark = ",")
    , "value" = format(round(p, -2), big.mark = ",")
  )
  
  printed_label <- switch(
    output_var
    ,"rides_inbound" = "Rides"
    , "value" = "Avg Sentiment"
  )
  
  diff <- paste(round(100 * c, digits = 1),"%", sep = "")
  
  html_text <- glue('
                    <b>{d}</b>
                    <p class="popup"> {printed_label}: {printed_value} </p>
                    <p> Difference from the Average: {diff} </p>
                    ')
  
  return(html_text)
}

#### fit rides_inbound model ####
## (step regression didn't produce reasonable estimates - see intercept in model e.g.)
df_model <- df_final %>%
  filter(complete.cases(.)) 

options(set.seed = 12345, scipen = 99)

models <- df_model %>%
  split(df_model$county) %>% 
  map(. %>% select(-county) ) %>%
  map(~ randomForest(rides_inbound ~ ., data = . ) )

## TODO: add model for sentiment

df_sent_model <- df_final_sent %>% 
  filter(complete.cases(.))

options(set.seed = 12345, scipen = 99)
#commented out for ease of use. If not using dummy, uncoment this
# sent_mod <- df_sent_model %>%
#   split(df_sent_model$county) %>% 
#   map(. %>% select(-county) ) %>%
#   map(~  randomForest(value ~ ., data = . ) ) 

## testing not needed, can be deleted if needed
## commenting out when using dummy
# new_mod <- df_sent_model %>% 
#   select(-county) %>% 
#   randomForest(value ~ ., data = .) 

#### Model Exploration ####
# make test df with filter, add preds

#I understand this is messy, and could have been done just on the df_sent_model.
# I was going in a different direction, and then pivoted to violin plots instead
# of bar charts.

riverside_df <- df_sent_model %>% filter(county == "Riverside")
riverside_df$predictions  <- predict(sent_mod[["Riverside"]], df_sent_model%>% filter(county=="Riverside")) 
riverside_df$diff <- riverside_df$predictions - riverside_df$value
riv_avg_diff <- mean(riverside_df$diff)

la_df <- df_sent_model %>% filter(county == "Los Angeles")
la_df$predictions <- predict(sent_mod[["Los Angeles"]], df_sent_model%>% filter(county=="Los Angeles"))
la_df$diff <- la_df$predictions - la_df$value
la_avg_diff <- mean(la_df$diff)

ie_df <- df_sent_model %>% filter(county == "Inland Empire")
ie_df$predictions <- predict(sent_mod[["Inland Empire"]], df_sent_model%>% filter(county=="Inland Empire"))
ie_df$diff <- ie_df$predictions - ie_df$value
ie_avg_diff <- mean(ie_df$diff)

orange_df <- df_sent_model %>% filter(county == "Orange")
orange_df$predictions <- predict(sent_mod[["Orange"]], df_sent_model%>% filter(county=="Orange"))
orange_df$diff <- orange_df$predictions - orange_df$value
orange_avg_diff <- mean(orange_df$diff)

san_df <- df_sent_model %>% filter(county == "San Bernardino")
san_df$predictions <- predict(sent_mod[["San Bernardino"]], df_sent_model%>% filter(county=="San Bernardino"))
san_df$diff <- san_df$predictions - san_df$value
san_avg_diff <- mean(san_df$diff)

preds_df<- bind_rows(san_df, orange_df, ie_df, la_df, riverside_df)
preds_df_diff<- mean(preds_df$diff) 


diff_plot <- ggplot(preds_df, aes(y=diff, x=county, color = county)) + 
                       geom_violin()+
                      geom_boxplot(width=0.1)+
                      ggtitle("Difference Between Actual and Predicted")
preds_plot <- ggplot(preds_df, aes(y=predictions, x=county, color = county)) + 
                     geom_violin()+
                     geom_boxplot(width=0.1)+
                     ggtitle("Predicted Sentiment Scores")
actual_plot <- ggplot(preds_df, aes(y=value, x=county, color = county), fill = county) + 
                      geom_violin()+ 
                      geom_boxplot(width=0.1)+
                      ggtitle("Actual Sentiment Scores")
preds_plot
diff_plot
actual_plot 

 #### New_Mod Exploration ####
new_mod_df <- df_sent_model
new_mod_df$predictions  <- predict(new_mod, new_mod_df) 
new_mod_df$diff <- new_mod_df$predictions - new_mod_df$value
new_mod_avg_diff <- mean(new_mod_df$diff)

new_mod_plot <- ggplot(new_mod_df, aes(y=value, x =county, color = county), fill = county)+
                geom_violin()+
                geom_boxplot(width = 0.1)+
                  ggtitle("New Actual Sentiment Score")
new_mod_diff_plot <-ggplot(new_mod_df, aes(y=diff, x =county, color = county), fill = county)+
  geom_violin()+
  geom_boxplot(width = 0.1)+
  ggtitle("New Difference between Actual and Predicted")

new_mod_pred_plot <-ggplot(new_mod_df, aes(y=predictions, x =county, color = county), fill = county)+
  geom_violin()+
  geom_boxplot(width = 0.1)+
  ggtitle("New Predicted Sentiment Score")

new_mod_plot
new_mod_diff_plot
new_mod_pred_plot

#### Dummy Data ####


df_sent_model$value <- rnorm(nrow(df_sent_model), mean = 0, sd = 1)
df_sent_model$value <- rescale(df_sent_model$value, to = c(-1, 1))
#use this model for splitting counties
sent_mod <- df_sent_model %>%
  split(df_sent_model$county) %>% 
  map(. %>% select(-county) ) %>%
  map(~  randomForest(value ~ ., data = . ) )

#use this model for non-split modeling
#dummy_mod <- dummy_df %>% 
#  select(-county, -value) %>% 
#  randomForest(dummy~ ., data = .)
  

#Use this section for split data
# dummy_riverside_df <- dummy_df %>% filter(county == "Riverside")
# dummy_riverside_df$predictions  <- predict(dummy_mod[["Riverside"]], dummy_df%>% filter(county=="Riverside"))
# dummy_riverside_df$diff <- dummy_riverside_df$predictions - dummy_riverside_df$value
# dummy_riv_avg_diff <- mean(dummy_riverside_df$diff)
# 
# dummy_la_df <- dummy_df %>% filter(county == "Los Angeles")
# dummy_la_df$predictions <- predict(dummy_mod[["Los Angeles"]],dummy_df%>% filter(county=="Los Angeles"))
# dummy_la_df$diff <- dummy_la_df$predictions - dummy_la_df$value
# dummy_la_avg_diff <- mean(la_df$diff)
# 
# dummy_ie_df <- dummy_df %>% filter(county == "Inland Empire")
# dummy_ie_df$predictions <- predict(dummy_mod[["Inland Empire"]], dummy_df%>% filter(county=="Inland Empire"))
# dummy_ie_df$diff <- dummy_ie_df$predictions - dummy_ie_df$value
# dummy_ie_avg_diff <- mean(dummy_ie_df$diff)
# 
# dummy_orange_df <- dummy_df %>% filter(county == "Orange")
# dummy_orange_df$predictions <- predict(dummy_mod[["Orange"]], dummy_df%>% filter(county=="Orange"))
# dummy_orange_df$diff <- dummy_orange_df$predictions - dummy_orange_df$value
# dummy_orange_avg_diff <- mean(dummy_orange_df$diff)
# 
# dummy_san_df <- dummy_df %>% filter(county == "San Bernardino")
# dummy_san_df$predictions <- predict(dummy_mod[["San Bernardino"]], dummy_df%>% filter(county=="San Bernardino"))
# dummy_san_df$diff <-dummy_san_df$predictions - dummy_san_df$value
# dummy_san_avg_diff <- mean(dummy_san_df$diff)
# 
# dummy_preds_df<- bind_rows(dummy_san_df, dummy_orange_df, dummy_ie_df, dummy_la_df,dummy_riverside_df)
# dummy_preds_df_diff<- mean(dummy_preds_df$diff)
# 
# 
# dummy_mod_plot <- ggplot(dummy_preds_df, aes(y=dummy, x =county, color = county), fill = county)+
#   geom_violin()+
#   geom_boxplot(width = 0.1)+
#   ggtitle("New Actual Sentiment Score")
# 
# dummy_mod_diff_plot <-ggplot(dummy_preds_df, aes(y=diff, x =county, color = county), fill = county)+
#   geom_violin()+
#   geom_boxplot(width = 0.1)+
#   ggtitle("New Difference between Actual and Predicted")
# 
# dummy_mod_pred_plot <-ggplot(dummy_preds_df, aes(y=predictions, x =county, color = county), fill = county)+
#   geom_violin()+
#   geom_boxplot(width = 0.1)+
#   ggtitle("New Predicted Sentiment Score")
# 
# 
# #use this section for non-split data
# # dummy_mod_df <- dummy_df
# # dummy_mod_df$predictions  <- predict(dummy_mod, dummy_mod_df) 
# # dummy_mod_df$diff <- dummy_mod_df$predictions - dummy_mod_df$dummy
# # dummy_mod_avg_diff <- mean(dummy_mod_df$diff)
# # 
# # dummy_mod_plot <- ggplot(dummy_mod_df, aes(y=dummy, x =county, color = county), fill = county)+
# #   geom_violin()+
# #   geom_boxplot(width = 0.1)+
# #   ggtitle("New Actual Sentiment Score")
# # 
# # dummy_mod_diff_plot <-ggplot(dummy_mod_df, aes(y=diff, x =county, color = county), fill = county)+
# #   geom_violin()+
# #   geom_boxplot(width = 0.1)+
# #   ggtitle("New Difference between Actual and Predicted")
# # 
# # dummy_mod_pred_plot <-ggplot(dummy_mod_df, aes(y=predictions, x =county, color = county), fill = county)+
# #   geom_violin()+
# #   geom_boxplot(width = 0.1)+
# #   ggtitle("New Predicted Sentiment Score")
# 
# dummy_mod_plot
# dummy_mod_diff_plot
# dummy_mod_pred_plot
