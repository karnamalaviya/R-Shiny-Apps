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
library(tidyverse)
library(RColorBrewer)
library(plotly)
library(gridExtra)
library(magrittr)
library(xlsx)
library(viridis)
library(shiny)
library(shinydashboard)
library(DT)
library(rsconnect)
library(leaflet)


###################
# 1. Import Data  #
###################


# 2018 Data
df_2018 <- read.csv("analysis/intermediate/fantasy_data_2018.csv", na = NA, stringsAsFactors = FALSE)

# 2019 Data
df_2019 <- read.csv("analysis/intermediate/fantasy_data_2019.csv", na = NA, stringsAsFactors = FALSE)

# Team Selection Data
team_selection <- read.csv("analysis/intermediate/choices_data_2019.csv", na = NA, stringsAsFactors = FALSE)
team_colors <-  c("#BD0026", "#E41A1C", "#B15928", "#FF7F00", "#999999", "#F781BF", "#08306B", "#FFD92F", "#313695", "#74ADD1")


#######################
# 2. Create Shiny App #
#######################


# Create List for Dropdown of Season's Races, with Detailed List
race_names_2018 <- df_2018 %>% 
  select(clean_race_number_app)  
race_names_2018 <- unique(race_names_2018$clean_race_number_app)

race_names_2019 <- df_2019 %>% 
  select(clean_race_number_app)  
race_names_2019 <- unique(race_names_2019$clean_race_number_app)


# User Interface
ui <- dashboardPage(skin = 'green', 
  dashboardHeader(title = "Hover Over Karna's Demise", titleWidth = 300),
  dashboardSidebar(
    # Sidebar Items
    sidebarMenu(
      menuItem("Race Results", tabName = "race_results_tab", icon = icon("chart-bar")),
      menuItem("Season Results", tabName = "season_results_tab", icon = icon("chart-line")),
      menuItem("Team Selections", tabName = "team_selections_tab", icon = icon("cookie-bite")),
      menuItem("Summary Statistics", tabName = "summary_stats_tab", icon = icon("flag-checkered"), 
               menuSubItem("Overview", tabName = "season_overview_tab", icon = icon("trophy")), 
               menuSubItem("Highest/Lowest Scores", tabName = "high_low_tab", icon = icon("exchange-alt")))), 
      radioButtons(inputId = "datatype", "See Results for 2019 (Current) or 2018 Season?", 
                choices = c("2019 Results" = "df_2019", "2018 Results" = "df_2018"),
                 selected = "df_2019")),
  
  dashboardBody(
    
  
    tabItems(
      # Race Results tab content
      
      tabItem(tabName = "race_results_tab",
              fluidRow(
                plotlyOutput("race_result", width = "1100px", height = "450px")), 
              fluidRow(
                box(title = "Select Race", width = 4, status = "primary", solidHeader = TRUE,
                    selectInput(inputId = "race", label = "Choose Which Race's Results to Display",
                                choices = race_names_2019, 
                                selected = race_names_2019[-1])), 
                infoBoxOutput("race_winnerBox", width = 4),
                box(title = "Data", width = 4, status = "primary", solidHeader = TRUE,
                    textOutput("data_description_race"))
                )),
      
      # Season Results tab content
      tabItem(tabName = "season_results_tab",
              fluidRow(
                plotlyOutput("season_result", width = "1100px", height = "550px")), 
              fluidRow(
                box(title = "Figure", width = 4, status = "primary", solidHeader = TRUE,
                    textOutput("season_results_text")),
                infoBoxOutput("season_winnerBox", width = 4), 
                box(title = "Data", width = 4, status = "primary", solidHeader = TRUE,
                    textOutput("data_description_season")) 
                )),
      
      # Season Overview Summary Statistics tab content
      tabItem(tabName = "season_overview_tab",
              fluidRow(
                box(title = "Season Overview", width = 12, status = "primary", 
                    dataTableOutput("overview_summary_stats")))),
      
      # Team Changes Tab Content 
      tabItem(tabName = "team_selections_tab", 
              fluidRow(
                plotlyOutput("team_selections", width = "1100px", height = "450px")),
              fluidRow(
                box(title = "Figure", width = 4, status = "primary", solidHeader = TRUE, 
                    textOutput("team_changes_text")), 
                valueBoxOutput("changes_correlationBox", width = 4),
                box(title = "Data", width = 4, status = "primary", solidHeader = TRUE,
                    textOutput("data_description_changes")))),
      
      # Season Overview Highest/Lowest Scores tab content
      tabItem(tabName = "high_low_tab",
              fluidRow(
                box(title = "Highest Scores", width = 6, status = "primary",
                dataTableOutput("high_summary_stats")),
                box(title = "Lowest Scores", width = 6, status = "danger",
                    dataTableOutput("low_summary_stats")))))))



# Server
server <- function(input, output, session) {

  ## Data Description
    
    # Description of Actual and Imputed Data  
    DataDescription <- reactive({
      ifelse(input$datatype == "df_2019", 
             "You are using the results data from the 2019 (current) F1 season. Switch the button in the sidebar to 2018 to view results from the 2018 season.",
             "You are viewing results for the 2018 F1 season. Switch the button in the sidebar to switch to 2019 to view results from the 2019 (current) season.")
    })
  
    # Data Description Box
    output$data_description_race <- renderText(DataDescription())
    output$data_description_season <- renderText(DataDescription())
    output$data_description_changes <- renderText(DataDescription())

    
  ## Race Result Tab
    
    # Select Relevant Race Result Dataframe
    dataInputRace <- reactive({
      get(input$datatype) %>% 
        filter(clean_race_number_app %in% input$race)
    })
    
    
    # Race Result Plotly Graph
    output$race_result <- renderPlotly( {
      plot_ly(dataInputRace(), x = ~points, y = ~clean_position,
              color = ~clean_nickname,
              type = 'bar', orientation = 'h',
              colors = colorRamp(viridis(14)) ,
              hoverinfo = 'text',
              text = ~paste('</br> Name: ', nickname,
                            '</br> Race Points ', points,
                            '</br> Race Position: ', race_position,
                            '</br> Season Position: ', season_position_torace)) %>%
        layout(xaxis = list(title = "Points", gridcolor = "lightgray", gridwidth = 0.5, 
                            autotick = FALSE, dtick = 20),
               yaxis = list(title = "Position", autorange = "reversed"),
               title =   paste0("<b> ", 
                                dataInputRace()$race_name[dataInputRace()$race_position == 1],
                                " ",
                                "Race Result </b>"),
               font = list(family = "Arial", color = "black"), 
               plot_bgcolor = 'transparent',
               paper_bgcolor = 'transparent')
        } )
    
    # Race Winner Info Box
    race_winner_name <- reactive({
      dataInputRace()$nickname[dataInputRace()$race_position == 1]
    })
    
    race_winner_points <- reactive({
      dataInputRace()$points[dataInputRace()$race_position == 1]
    })
    
    output$race_winnerBox <- renderInfoBox({
      infoBox("Race Winner", paste0(race_winner_name(), ": ", race_winner_points(), " Points")
              , icon = icon("medal"), color = "red", fill = TRUE)
    })
  
    
  ## Season Results
    # Select Relevant Dataframe for Season Results
    dataInputSeason <- reactive({
      get(input$datatype)
    })
    
    # Plot Season Results
    output$season_result <- renderPlotly( {
      plot_ly(dataInputSeason(), x = ~clean_race_number, y = ~distance_to_first_season, 
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
        layout(xaxis = list(title = "<b> Race </b>", tickangle = -55, gridcolor = "lightgray", gridwidth = 0.5), 
               yaxis = list(title = "<b> Points Away from First Place in Season Standings </b>", gridcolor = "lightgray", gridwidth = 0.5), 
               title = "<b> Points Away From First Place: Season Results </b>", 
               font = list(family = "Arial"),
               plot_bgcolor = 'transparent',
               paper_bgcolor = 'transparent')
    })
  
    # Season Results Figure Description
    output$season_results_text <- renderText("The figure shows how far each person is behind 1st place in the overall standings at 
                each point in the season. Values are <=0 because each point is equal to the season points
                by the person in first minus and the other person's points. The 0 line corresponds to the person in first place.")
    
    # Season Winner Info Box
    season_winner_name <- reactive({
      dataInputSeason()$nickname[dataInputSeason()$season_position_torace == 1 & 
                                   dataInputSeason()$latest_race == 1]
    })
    
    season_winner_points <- reactive({
      dataInputSeason()$season_points[dataInputSeason()$season_position_torace == 1 & 
                                   dataInputSeason()$race_number == dataInputSeason()$max_race_number]
    })
    
    output$season_winnerBox <- renderInfoBox({
      infoBox("Season Leader", paste0(season_winner_name(), ": ", season_winner_points(), " Points")
              , icon = icon("medal"), color = "red", fill = TRUE)
    })
    
  
  ## Summary Statistics  
    # Select Relevant Dataframe for Overview Summary Statistics
    dataInputSumOverview <- reactive({
      get(input$datatype) %>% 
      select(name, nickname, total_points, avg_points, min_points, 
             max_points, season_victories, season_podiums, 
             season_fourths, season_lasts, season_bottom_threes) %>% 
      distinct() %>% 
      mutate(season_position = rank(-total_points), 
             avg_points = round(avg_points, digits = 2)) %>% 
      arrange(season_position) %>% 
      select(season_position, everything()) %>% 
      rename(Position = season_position, Name = name, Nickname = nickname, Points = total_points, 
             Avg.Points = avg_points, Min.Points = min_points, Max.Points = max_points, 
             Wins = season_victories, Podiums = season_podiums, Fourths = season_fourths, 
             Lasts = season_lasts, Bottom.Threes = season_bottom_threes)   
    })
    
    # Table of Overview Summary Statistics
    output$overview_summary_stats <- renderDataTable(dataInputSumOverview(), 
                                            options = list(pageLength = 25, 
                                                           autoWidth = TRUE,
                                                           scrollX = TRUE), 
                                            rownames = FALSE)
  
  
    # Select Relevant Dataframe for Overview Summary Statistics
    dataInputHigh <- reactive({
      get(input$datatype) %>% 
        select(name, nickname, points, race_name, top_scores) %>%  
        arrange(top_scores) %>% 
        rename(Name = name, Nickname = nickname, Points = points, Race = race_name,
               Score.Rank = top_scores)
    })
    
    # Table of Overview Summary Statistics
    output$high_summary_stats <- renderDataTable(dataInputHigh(), 
                                             options = list(pageLength = 20, 
                                                            autoWidth = TRUE,
                                                            scrollX = TRUE), 
                                             rownames = FALSE) 
    
    # Select Relevant Dataframe for Overview Summary Statistics
    dataInputLow <- reactive({
      get(input$datatype) %>% 
        select(name, nickname, points, race_name, top_scores) %>%  
        arrange(-top_scores) %>% 
        rename(Name = name, Nickname = nickname, Points = points, Race = race_name,
               Score.Rank = top_scores)
    })
    
    # Table of Overview Summary Statistics
    output$low_summary_stats <- renderDataTable(dataInputLow(), 
                                                 options = list(pageLength = 20, 
                                                                autoWidth = TRUE,
                                                                scrollX = TRUE), 
                                                 rownames = FALSE) 
    
  
  ## Team Changes  
    # Select Relevant Dataframe for Team Changes
    dataInputChanges <- team_selection
  
    
    # Team Changes Plotly Graph
    output$team_selections <- renderPlotly( {
      plot_ly(team_selection, y = ~avg_league_position, z = ~points) %>% 
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
                                       range = c(min(team_selection$price),max(team_selection$price))),
                          yaxis = list(title = 'Average League Position of Teams with this Person',
                                       range = c(min(team_selection$avg_league_position),max(team_selection$avg_league_position))),
                          zaxis = list(title = 'Points Scored',
                                       range = c(min(team_selection$points),max(team_selection$points))), 
                          title =   paste0("<b> Value for Money & Popularity"),
                          font = list(family = "Arial", color = "black"), 
                          plot_bgcolor = 'transparent',
                          paper_bgcolor = 'transparent'))
      } )
    
    output$team_changes_text <- renderText("Scatterplot of a Driver or Constructor's current price, total points scored in the season, 
                                           and the average league position of the teams they have been selected on. 
                                           The size of a data point's sphere represents how many teams they are currently on. 
                                           Good luck figuring out how to use this helpfully..heh.")
    
    

    
    observe({
    if(input$datatype == "df_2019") {
    updateSelectInput(session = session, inputId = "race", choices = race_names_2019, selected = race_names_2019[-1])
    }
    else {
      updateSelectInput(session = session, inputId = "race", choices = race_names_2018, selected = race_names_2018[-1])  
    }
    })
  
}


shinyApp(ui = ui, server = server)
