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
# 1. Append Data #
#################

df_2019 <- read.csv("analysis/intermediate/fantasy_data_2019.csv", na = NA, stringsAsFactors = FALSE)

df_2018 <- read.csv("analysis/intermediate/fantasy_data_2018.csv", na = NA, stringsAsFactors = FALSE)

df <- bind_rows(df_2018, df_2019)

# Export Intermediate Fantasy Dataset
write.csv(df, file = "analysis/intermediate/fantasy_data_forapp.csv", row.names = FALSE)
write.csv(df, file = "shiny apps/fantasy_2019/fantasy_data_forapp.csv", row.names = FALSE)
write.csv(df_2018, file = "shiny apps/fantasy_2019/fantasy_data_2018.csv", row.names = FALSE)
write.csv(df_2019, file = "shiny apps/fantasy_2019/fantasy_data_2019.csv", row.names = FALSE)

