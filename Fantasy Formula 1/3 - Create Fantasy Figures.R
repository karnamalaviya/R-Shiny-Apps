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


########################
# 1. Import Clean Data #
########################

# Actual Data
df_output <- read.csv("analysis/intermediate/fantasy_data_2019.csv", na = NA, stringsAsFactors = FALSE)

#############################
# 2. Plots for Latest Race  #
#############################

for (i in 1: race_number) {

# Create DF for Latest Race
race_plot <- df_output %>%
  select(race_position, name, points, race_name, race_number, nickname, team_name,
         season_points, season_position_torace) %>% 
  filter(race_number == i) %>% 
  mutate(clean_position = if_else(race_position>=10, paste0(as.character(race_position), " ", team_name), 
                          paste0("0", as.character(race_position), " ", team_name)), 
         clean_nickname = if_else(race_position>=10, paste0(as.character(race_position), " ", nickname), 
                                  paste0("0", as.character(race_position), " ", nickname)))

graph_name <- race_plot$race_name[race_plot$race_position == 1]
graph_title <- paste0(graph_name, " ", "Results: Who Run Da World?? Balds")
graph_name <- paste0(graph_name, " ", "Results")
graph_save <- paste0("analysis/output/race results/2019/", graph_name, ".jpeg")


# Plot Results
race_results <- ggplot(race_plot, aes(x = reorder(clean_position, -race_position), y = points, fill = clean_nickname)) +
                    geom_bar(stat = "identity") + 
                    labs(fill = "Legal Name") + labs(x = "Team Name") + labs(y = "Points") + 
                    ggtitle(graph_title) + coord_flip() +
                    geom_text(aes(label = points), hjust=-0.2)

ggsave(graph_save, plot = race_results, width =  15.5, height = 5.5, dpi = 120)   

}


################################
# 4. Plot for Season Standings #
################################

season_plots <- df_output %>% 
  select(race_position, name, points, season_points, race_name, race_number, total_points, 
         season_position_torace, season_position, nickname, distance_to_first_season) %>% 
  mutate(clean_season_position = if_else(season_position>=10, paste0(as.character(season_position), " ", nickname), 
                                  paste0("0", as.character(season_position), " ", nickname)), 
         clean_race_number = if_else(race_number >= 10, paste0(as.character(race_number), " ", race_name), 
                                     paste0("0",paste0(as.character(race_number), " ", race_name)))) %>% 
  arrange(race_number, race_position)
         

# Points away from first
plotly_season_results_distance <- plot_ly(season_plots, x = ~clean_race_number, y = ~distance_to_first_season, 
                                 color = ~clean_season_position, colors = "Set1", 
                                 type = 'scatter', mode = 'lines',
                                 line = list(shape = 'spline', 'smoothing' = 0.4, width = 3.75),
                                 hoverinfo = 'text',
                                 text = ~paste('</br> Name: ', nickname,
                                               '</br> Race: ', race_name,
                                               '</br> Race Position: ', race_position,
                                               '</br> Race Points: ', points, 
                                               '</br> Season Position: ', season_position_torace,
                                               '</br> Season Points: ', season_points, 
                                               '</br> Points Behind First: ', distance_to_first_season))%>% 
  layout(xaxis = list(title = "<b> Race </b>", tickangle = -55), 
         yaxis = list(title = "<b> Points Away from First Place Overall </b>"), 
         title = "<b> Who Run Da World?? BALDS Season Results </b>", 
         font = list(family = "Arial"))


############################
# 5. Season Summary Stats  #
############################

# Highest and Lowest Race Point Totals
summary_high_low_scores <- df_output %>% 
  select(name, points, race_name, starts_with("top_scores"), starts_with("lowest_scores")) %>% 
  arrange(top_scores)

write.xlsx(summary_high_low_scores, file = "analysis/output/fantasy_stats_2019.xlsx", sheetName = "High Low", 
           append = TRUE)

# General Summary Stats
summary_stats <- df_output %>% 
  select(name, nickname, total_points, avg_points, min_points, 
         max_points, avg_position, season_victories, season_podiums, 
         season_fourths, season_lasts, season_bottom_threes, avg_distance_to_first) %>% 
  distinct() %>% 
  mutate(season_position = rank(-total_points)) %>% 
  arrange(season_position) %>% 
  select(season_position, everything())

write.xlsx(summary_stats, file = "analysis/output/fantasy_stats_2019.xlsx", sheetName = "Sum Stats", 
           append=TRUE)

