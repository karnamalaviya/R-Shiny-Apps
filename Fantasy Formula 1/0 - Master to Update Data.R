#########################################################
#########################################################
###                                                   ### 
### Purpose: Create F1 Summary Statistics Graphs      ### 
###                                                   ### 
### Last updated: 09/30/2018                          ###                  
### Audited: NO                                       ###                           
#########################################################
#########################################################

# Define Globals
  # Number of Races Completed in the Season
  race_number <-  21
  # Number of Teams in League
  number_of_teams <- 17

 # Set the working directory
setwd("C:/Users/Karna Malaviya/Desktop/f1/")

# Clean Data
source("analysis/code/2019 Fantasy/1 - Clean Results Data.R")

# Prepare 3D Scatterplot Data
source("analysis/code/2019 Fantasy/2 - Clean Team Selection Data_v2.R")

# Create Figures
source("analysis/code/2019 Fantasy/3 - Create Fantasy Figures.R")

# Update Shiny App Folder
source("analysis/code/2019 Fantasy/4 - Prep Data for App.R")


# Re-Deploy Shiny App
setwd("C:/Users/Karna Malaviya/Desktop/f1/shiny apps/fantasy_2019")

source("App.R")
deployApp()