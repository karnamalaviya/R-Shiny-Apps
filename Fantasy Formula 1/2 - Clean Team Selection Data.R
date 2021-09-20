#########################################################
#########################################################
###                                                   ### 
### Purpose: Create F1 Summary Statistics Graphs      ### 
###                                                   ### 
### Last updated: 09/30/2018                          ###                  
### Audited: NO                                       ###                           
#########################################################
#########################################################

 # Set the working directory
setwd("C:/Users/Karna Malaviya/Desktop/f1/")

library(dplyr)
library(readxl)
library(readr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(BrattleExtras)
library(RColorBrewer)
library(plotly)
library(gridExtra)
library(magrittr)
library(xlsx)
library(viridis)
library(leaflet)

##################
# 1. Merge Data #
#################

choices <- read.csv("scrape/raw/2019_choices.csv", na = NA, stringsAsFactors = FALSE)
prices <- read.csv("scrape/raw/2019_prices.csv", na = NA, stringsAsFactors = FALSE)

# Clean Team Choices for merge
choices <- choices %>% 
  mutate(points = as.numeric(str_replace(points, " PTS", "")),
         name = recode(name, "A ROMEO" = "ALFA ROMEO", "R BULL" = "RED BULL", "R POINT" = "RACING POINT", "T ROSSO" = "TORO ROSSO"),
         player_name = team_name) %>% 
  select(-team_name) %>% 
  mutate(player_rank = rank(-season_points, ties.method = "min"), 
         player_rank = (((player_rank - 1)/6) + 1)) %>% 
  select(-season_points, -points)

# Clean Prices for Merge
prices <- prices %>% 
  select(type, name = team_name, team_name_abrev, price, points, owned_pct_global = share_owned) %>% 
  mutate(name = toupper(name), 
         price = as.numeric(gsub("\\$|m", "", price)), 
         points = as.numeric(gsub("Pts", "", points)), 
         owned_pct_global = as.numeric(gsub("%", "", owned_pct_global))/100
         )

# Merge Team Choices with Prices
df <- merge(choices, prices, by = c("type", "name"), all = TRUE)
df$team_name[df$name == "Aastha"] <- "i'm in me mum's car"

##################
# 2. Clean Data #
#################

# Create Team Name Variable
df <- df %>% 
  mutate(team_name = team_name_abrev) %>% 
  mutate(team_name = recode(team_name, "MER" = "Mercedes", "FER" = "Ferrari", "RED" = "Red Bull", "REN" = "Renault",
                            "HAA" = "Haas", "MCL" = "McLaren", "RAC" = "Racing Point", "TOR" = "Toro Rosso",
                            "ALF" = "Alfa Romeo", "WIL" = "Williams"))
  

# Create Number of Times Selected in a Race Variable
df <- df %>% 
  group_by(type, name) %>% 
  mutate(count = if_else(!is.na(player_name), 1, 0), 
         owned_league = sum(count), 
         owned_pct_league = owned_league / number_of_teams) %>% 
  ungroup() %>% 
  select(-count)

# Create Indicator for Whether Constructor or Not and Turbo Eligible
df <- df %>% 
  mutate(is_constructor = if_else(toupper(team_name) == toupper(name), 1, 0), 
         turbo_eligible = if_else(price < 19 & is_constructor == 0, 1, 0))

################################
# 3. Create Plotly Scatterplot #
################################

for_plot <- df %>% 
  group_by(type, team_name, name, is_constructor) %>% 
  summarize(points = min(points),
            owned_league = min(owned_league),
            owned_pct_league = min(owned_pct_league),
            owned_pct_global = min(owned_pct_global), 
            price = min(price), 
            avg_league_position = mean(player_rank)) %>% 
  mutate(points_per_dollar = (points / price)) %>% 
  ungroup() %>% 
  mutate(rank_ppd = rank(-points_per_dollar), 
         avg_league_position = if_else(is.na(avg_league_position), number_of_teams, round(avg_league_position, 2)))
  
#colors = colorRamp(viridis(30))

# Define Team Colors for Plot (Alphabetical Order)
team_colors <-  c("#BD0026", "#E41A1C", "#B15928", "#FF7F00", "#999999", "#F781BF", "#08306B", "#FFD92F", "#313695", "#74ADD1")

# Create Plot without Turbo Threshold Line
plot_ly(for_plot, 
         y = ~avg_league_position, 
         z = ~points) %>% 
    add_trace(x = ~price, 
              color = ~team_name, 
              colors = team_colors, 
              size = ~owned_pct_league, 
              type = 'scatter3d',
              marker = list(symbol = 'circle', 
                            sizemode = 'diameter'),
              sizes = c(10, 70), 
              hoverinfo = 'text',
              text = ~paste('</br> Name: ', name,
                            '</br> Points: ', points,
                            '</br> Price: ', price,
                            '</br> Number Owned: ', owned_league, 
                            '</br> Average League Position: ', avg_league_position)) %>%
    layout(scene = list(xaxis = list(title = 'Price ($ millions)', 
                                     range = c(min(for_plot$price),max(for_plot$price))),
                        yaxis = list(title = 'Average League Position of Teams with this Person',
                                     range = c(min(for_plot$avg_league_position),max(for_plot$avg_league_position))),
                        zaxis = list(title = 'Points Scored',
                                     range = c(min(for_plot$points),max(for_plot$points)))))

# Export Intermediate Fantasy Dataset to file and Shiny App
write.csv(for_plot, file = "analysis/intermediate/choices_data_2019.csv", row.names = FALSE)
write.csv(for_plot, file = "shiny apps/fantasy_2019/choices_data_2019.csv", row.names = FALSE)
