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
# 1. Clean Data #
#################

df <- read.csv("scrape/raw/2019_fantasy_results.csv", na = NA, stringsAsFactors = FALSE)

nicknames <- read.csv("scrape/raw/fantasy_name_map_2019.csv", na = NA, stringsAsFactors = FALSE)

season <- "2019"

# Clean Names for merge
df <- df %>% 
  mutate(name = str_replace(df$name, "Name: ", "")) %>% 
  select(name, team_name, points, race_name, race_number)

# Merge in Nicknames
df <- merge(df, nicknames, by = "name", all = TRUE) 
df$team_name[df$name == "Aastha"] <- "i'm in me mum's car"
df$team_name[df$name == "Karna"] <- "Karn on d Kob(e Bryan)"

################################
# 3. Create Analysis Variables #
################################

# Clean Race Position
df <- df %>% 
  group_by(race_number) %>% 
  mutate(position = rank(-points, na.last = TRUE, ties.method = "min")) %>% 
  rename(race_position = position) %>% 
  ungroup()

# Clean Race Number
df <- df %>% 
  mutate(max_race_number = max(race_number))
#         race_number = -(race_number - max_race_number - 1))

# Minimum and Maximum Points Variables
df <- df %>% 
  mutate(top_scores = rank(-points, na.last = TRUE), 
         lowest_scores = rank(points, na.last = TRUE))

# Create variables for running sum of points, points summary stats variables
df <- df %>% 
  group_by(name) %>% 
  arrange(race_number) %>% 
  mutate(season_points = cumsum(points), 
         total_points = sum(points, na.rm = TRUE), 
         avg_points = mean(points, na.rm = TRUE), 
         avg_position = mean(race_position), 
         min_points = min(points, na.rm = TRUE), 
         max_points = max(points, na.rm = TRUE)) %>% 
  ungroup()

# Create variable for season-long position race position, season position as of race, and reverse of race position 
df <- df %>%   
  group_by(race_number) %>% 
  mutate(reverse_race_position = rank(-race_position), 
         season_position_torace = rank(-season_points, ties.method = "min"),
         winner_points = max(points, na.rm = TRUE), 
         distance_to_first = winner_points - points, 
         winner_points_season = max(season_points, na.rm = TRUE), 
         distance_to_first_season = season_points - winner_points_season) %>% 
  ungroup() 

# Create and Merge Season Position Variable
season_positions <- df %>%
  filter(race_number == max_race_number) %>% 
  select(name, total_points) %>% 
  mutate(season_position = rank(-total_points, ties.method = "min", na.last = TRUE)) %>% 
  select(-total_points)

df <- merge(df, season_positions, by = c("name"), all = TRUE)


# Create Podium, Victories, Bottom Three, Last Variables  
df <- df %>% 
  group_by(name) %>% 
  mutate(podium = if_else(race_position<=3, 1, 0), 
         season_podiums = sum(podium), 
         victory = if_else(race_position == 1, 1, 0),
         season_victories = sum(victory),
         fourth = if_else(race_position == 4, 1, 0), 
         season_fourths = sum(fourth),
         bottom_three = if_else(reverse_race_position <= 3, 1, 0), 
         season_bottom_threes = sum(bottom_three),
         last = if_else(reverse_race_position == 1, 1, 0),
         season_lasts = sum(last),
         avg_distance_to_first = mean(distance_to_first, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(race_number, race_position, race_name)

# Clean Names, Nicknames, Race Numbers for Plots
df <- df %>% 
  mutate(clean_position = if_else(race_position>=10, paste0(as.character(race_position), " ", team_name), 
                         paste0("0", as.character(race_position), " ", team_name)), 
         clean_nickname = if_else(race_position>=10, paste0(as.character(race_position), " ", nickname), 
                         paste0("0", as.character(race_position), " ", nickname)), 
         clean_season_position = if_else(season_position>=10, paste0(as.character(season_position), " ", nickname), 
                                         paste0("0", as.character(season_position), " ", nickname)), 
         clean_race_number = if_else(race_number >= 10, paste0(as.character(race_number), " ", race_name), 
                                     paste0("0", paste0(as.character(race_number), " ", race_name))),
         clean_race_number_app = if_else(race_number >= 10, paste0(as.character(race_number), " ", race_name), 
                                     paste0("0", as.character(race_number), " ", race_name)), 
         latest_race = if_else(race_number == max_race_number, 1, 0)) 



# Export Intermediate Fantasy Dataset
write.csv(df, file = "analysis/intermediate/fantasy_data_2019.csv", row.names = FALSE)