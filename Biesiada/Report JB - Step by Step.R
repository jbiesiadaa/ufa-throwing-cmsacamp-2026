###Julia Biesiada
### Title: Data Wrangling - Teams Overviews

## -- 0. Data Loading-----------------------------------------------------------
library(tidyverse)
library(ggplot2)
ufa_throws <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/ufa_throws.csv")

# -- 1. Filter out all-star games once -----------------------------------------
ufa_clean <- ufa_throws |>
  filter(home_teamID != "allstars2",
         away_teamID != "allstars1")

# -- 2. Home goals scored / conceded per game, then summed ---------------------
home_score <- ufa_clean |>
  group_by(home_teamID, gameID) |>
  summarise(
    goals_scored   = max(home_team_score, na.rm = TRUE),
    goals_conceded = max(away_team_score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(home_teamID) |>
  summarise(
    total_goals_scored   = sum(goals_scored),
    total_goals_conceded = sum(goals_conceded),
    .groups = "drop"
  ) |>
  rename(team = home_teamID)

# -- 3. Away goals scored / conceded per game, then summed----------------------
away_score <- ufa_clean |>
  group_by(away_teamID, gameID) |>
  summarise(
    goals_scored   = max(away_team_score, na.rm = TRUE),
    goals_conceded = max(home_team_score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(away_teamID) |>
  summarise(
    total_goals_scored   = sum(goals_scored),
    total_goals_conceded = sum(goals_conceded),
    .groups = "drop"
  ) |>
  rename(team = away_teamID)

# -- 4. Combined goals (home + away) -------------------------------------------
combined_goals <- bind_rows(home_score, away_score) |>
  group_by(team) |>
  summarise(
    total_goals_scored   = sum(total_goals_scored),
    total_goals_conceded = sum(total_goals_conceded),
    .groups = "drop"
  )
# -- 5. Home/Away split stats --------------------------------------------------
split_stats <- ufa_clean |>
  mutate(
    team     = if_else(is_home_team == TRUE, home_teamID, away_teamID),
    location = if_else(is_home_team == TRUE, "home", "away"),
    win      = (is_home_team == TRUE  & home_team_win == 1) |
      (is_home_team == FALSE & home_team_win == 0)
  ) |>
  group_by(team, location) |>
  summarise(
    games     = n_distinct(gameID),
    wins      = n_distinct(gameID[win]),
    losses    = games - wins,
    throws    = sum(turnover == 0 & goal == 0, na.rm = TRUE),
    turnovers = sum(turnover == 1, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  pivot_wider(
    names_from  = location,
    values_from = c(games, wins, losses, throws, turnovers),
    names_glue  = "{location}_{.value}"          # e.g. home_games, away_wins
  )

# -- 6. Core team stats --------------------------------------------------------
team_stats_core <- ufa_clean |>
  mutate(
    team = if_else(is_home_team == TRUE, home_teamID, away_teamID),
    win  = (is_home_team == TRUE  & home_team_win == 1) |
      (is_home_team == FALSE & home_team_win == 0)
  ) |>
  group_by(team) |>
  summarise(
    total_games     = n_distinct(gameID),
    total_wins      = n_distinct(gameID[win]),
    total_losses    = total_games - total_wins,
    total_throws    = sum(turnover == 0 & goal == 0, na.rm = TRUE), #max posession_throw, possesionnumber 1
    total_goals     = sum(goal == 1,     na.rm = TRUE), #check this 
    total_turnovers = sum(turnover == 1, na.rm = TRUE),
    .groups = "drop"
  )

team_stats <- team_stats_core |>
  left_join(combined_goals, by = "team") |>
  left_join(split_stats,    by = "team") |>
  mutate(
    # Ratio: goals scored per throw attempt
    goal_ratio          = round(total_goals / (total_throws + total_goals + total_turnovers), 3),
    
    # Ratio: turnovers per total possession attempts
    turnover_ratio      = round(total_turnovers / (total_throws + total_goals + total_turnovers), 3),
    
    # Throws per goal: How many throws take to score a goal?
    throws_per_goal = round(total_throws / total_goals,1),
    
    # +/- box score: goals scored minus goals conceded across all games
    plus_minus          = total_goals_scored - total_goals_conceded,
    
    # Win percentage
    win_pct             = round(total_wins / total_games,3)
  ) |>
  # Tidy column order
  select(
    team,
    total_games, total_wins, total_losses, win_pct,
    total_throws,throws_per_goal, total_goals, total_turnovers,
    total_goals_scored, total_goals_conceded, plus_minus,
    goal_ratio, turnover_ratio,
    home_games, home_wins, home_losses, home_throws, home_turnovers,
    away_games, away_wins, away_losses, away_throws, away_turnovers
  )

team_stats|>
  mutate(throws_per_goal = total_throws / total_goals)|>
  select(throws_per_goal)

# 7.Adding Tactical Features ---------------------------------------------------

tactics_features <- ufa_clean|>
  mutate(team = if_else(is_home_team == TRUE, home_teamID, away_teamID)) |>
  group_by(team) |>
  summarise( 
    ## Offensive Features
    # Average throw distance on ALL throws
    avg_throw_distance = round(mean(throw_distance, na.rm = TRUE),2),
    # Average throw distance specifically on GOALS
    avg_goal_distance = round(mean(throw_distance[goal == 1], na.rm = TRUE),2),
    # Average throw distance on TURNOVERS 
    avg_turnover_distance = round(mean(throw_distance[turnover == 1], na.rm = TRUE),2),
    # Average throw angle
    avg_throw_angle = round(mean(abs(throw_angle), na.rm = TRUE),3),
    # How often they attempt long throws -> (I assume long throw is >20)
    long_throw_rate = round(sum(throw_distance >= 35, na.rm = TRUE) / n(),3),
    # Long Goal Rate 
    long_goal_rate= round(sum(throw_distance >= 35 & goal == 1,na.rm = TRUE) /
                          sum(goal == 1, na.rm = TRUE), 3),
    # Short Goal Rate 
    short_goal_rate= round(sum(throw_distance <= 10 & goal == 1,na.rm = TRUE) /
                           sum(goal == 1, na.rm = TRUE), 3),
    # Medium Goal Rate
    medium_goal_rate= round(sum(throw_distance > 10 & throw_distance < 35 & goal == 1,na.rm = TRUE) /
                            sum(goal == 1, na.rm = TRUE), 3), 
       
    ## Defensive Features
    # Short throw rate — teams that play it safe
    short_throw_rate = round(sum(throw_distance <= 10, na.rm = TRUE) / n(),3),
    #Medium throw rate 
    medium_throw_rate = round(sum(throw_distance > 10 & throw_distance < 35, na.rm = TRUE) / n(),3),
    #What is the long shot turnover rate?
    long_throw_turnover_rate = round(sum(throw_distance >= 35 & turnover == 1, na.rm = TRUE) /
                                       sum(throw_distance >= 35,            na.rm = TRUE), 3),
    # What is the medium shot turnover rate?
    medium_throw_turnover_rate = round(sum(throw_distance > 10 & throw_distance < 35 & turnover == 1, na.rm = TRUE) /
                                        sum(throw_distance > 10 & throw_distance < 35,na.rm = TRUE), 3),
    #What is the short shot turnover rate?
    short_throw_turnover_rate = round(sum(throw_distance <= 10 & turnover == 1, na.rm = TRUE) /
                                      sum(throw_distance  <= 10,            na.rm = TRUE), 3),
    
    ## Game Momentum Features (TBD - Add with time stamp and also with the num_possession -> sequence)
    # Goals scored per quarter - who starts strong vs finishes strong?
    goals_q1 = sum(goal == 1 & game_quarter == 1, na.rm = TRUE),
    goals_q2 = sum(goal == 1 & game_quarter == 2, na.rm = TRUE),
    goals_q3 = sum(goal == 1 & game_quarter == 3, na.rm = TRUE),
    goals_q4 = sum(goal == 1 & game_quarter == 4, na.rm = TRUE),
    goals_q5 = sum(goal == 1 & game_quarter == 5, na.rm = TRUE),
    # Turnovers per quarter - when do they lose concentration?
    turnovers_q1 = sum(turnover == 1 & game_quarter == 1, na.rm = TRUE),
    turnovers_q2 = sum(turnover == 1 & game_quarter == 2, na.rm = TRUE),
    turnovers_q3 = sum(turnover == 1 & game_quarter == 3, na.rm = TRUE),
    turnovers_q4 = sum(turnover == 1 & game_quarter == 4, na.rm = TRUE),
    turnover_q5 = sum(turnover == 1 & game_quarter == 5, na.rm = TRUE),
    # Power quarter — which quarter do they score most?
    power_quarter = case_when(
      goals_q1 == pmax(goals_q1, goals_q2, goals_q3, goals_q4) ~ "Q1",
      goals_q2 == pmax(goals_q1, goals_q2, goals_q3, goals_q4) ~ "Q2",
      goals_q3 == pmax(goals_q1, goals_q2, goals_q3, goals_q4) ~ "Q3",
      goals_q4 == pmax(goals_q1, goals_q2, goals_q3, goals_q4) ~ "Q4"
    ),
    
    # Turnover quarter - which quarter do they turnover the most?
    turnovers_quarter = case_when(
      turnovers_q1 == pmax(turnovers_q1, turnovers_q2, turnovers_q3, turnovers_q4) ~ "Q1",
      turnovers_q2 == pmax(turnovers_q1, turnovers_q2, turnovers_q3, turnovers_q4) ~ "Q2",
      turnovers_q3 == pmax(turnovers_q1, turnovers_q2, turnovers_q3, turnovers_q4) ~ "Q3",
      turnovers_q4 == pmax(turnovers_q1, turnovers_q2, turnovers_q3, turnovers_q4) ~ "Q4"
    ))

# 8.Combining Team Stats and Tactical Features ---------------------------------

full_team_stats <- team_stats |>
  left_join(tactics_features, by = "team")

# 9. Visualization for Teams Overview ------------------------------------------

## Creatw a win pct
#a) Teams Overview for Total Losses and Total Wins (make it better)

full_team_stats |>
  mutate(team = fct_reorder(team, total_wins, .desc = TRUE))|>
  pivot_longer(cols = c(total_wins, total_losses),
               names_to = "outcome",
               values_to = "count") |>
  ggplot(aes(x = team, y = count, fill = outcome)) +
  geom_col(position = "stack") +
  scale_y_continuous(breaks = seq(0,55, by = 5))+
  scale_fill_manual(values = c("total_wins" = "lightgreen", "total_losses" = "salmon"),
                    labels = c("total_wins" = "Total Wins", "total_losses" = "Total Losses"))+
  labs(title = " Teams Overview",y = "Total Games", x = "Team", fill = "Outcome")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1, hjust = 1))

#b) 
# Scatter Plot - team patterns 
#left top - elite (score the most and has the least turnovers)
#right top - risky (score a lot but also struggle with turnovers)
#left bottom - safe (can't score but do not make turnovers)
#riht bottom - struggling (hard to score and make a lot of turnovers)

#1. Loading Library 
library(ggimage)

#2. Recreate logo paths
team_logos <- tibble(
  team = c(
    "alleycats", "aviators",  "breeze",      "cannons",
    "cascades",  "empire",    "flyers",      "glory",
    "growlers",  "havoc",     "hustle",      "legion",
    "mechanix",  "nitro",     "outlaws",     "phoenix",
    "radicals",  "royal",     "rush",        "shred",
    "sol",       "spiders",   "summit",      "thunderbirds",
    "union",     "windchill"
  ),
  logo_url = paste0(getwd(), "/logos/", team, ".png")
)

#3. Creating a dataframe for the ggplot with a new plot_data
plot_data <- full_team_stats |>
  left_join(team_logos, by = "team")

#4.1 Creating a Median for a Goal Ratio
med_goal     <- median(plot_data$goal_ratio, na.rm = TRUE)

#4.2 Creating a Median for a Turnover Ratio
med_turnover <- median(plot_data$turnover_ratio, na.rm = TRUE)


#5. Scatterplot 
ggplot(plot_data, aes(x = turnover_ratio, y = goal_ratio)) +
  
  # Quadrant shading
  annotate("rect",
           xmin = -Inf,         xmax = med_turnover,
           ymin = med_goal,     ymax = Inf,
           fill = "darkgreen",  alpha = 0.05) +
  annotate("rect",
           xmin = med_turnover, xmax = Inf,
           ymin = med_goal,     ymax = Inf,
           fill = "orange",     alpha = 0.05) +
  annotate("rect",
           xmin = -Inf,         xmax = med_turnover,
           ymin = -Inf,         ymax = med_goal,
           fill = "steelblue",  alpha = 0.05) +
  annotate("rect",
           xmin = med_turnover, xmax = Inf,
           ymin = -Inf,         ymax = med_goal,
           fill = "firebrick",  alpha = 0.05) +
  
  #Median lines 
  geom_vline(xintercept = med_turnover,
             linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_hline(yintercept = med_goal,
             linetype = "dashed", color = "gray40", linewidth = 0.6) +
  
  # Team logos
  geom_image(aes(image = logo_url), size = 0.07, asp = 1.5) +
  
  # Quadrant labels
  annotate("text",
           x = min(plot_data$turnover_ratio),
           y = max(plot_data$goal_ratio),
           label = "ELITE",      hjust = 0, vjust = 1,
           color = "darkgreen",  fontface = "bold", size = 4) +
  annotate("text",
           x = max(plot_data$turnover_ratio),
           y = max(plot_data$goal_ratio),
           label = "RISK_TAKING",    hjust = 1, vjust = 1,
           color = "orange",     fontface = "bold", size = 4) +
  annotate("text",
           x = min(plot_data$turnover_ratio),
           y = min(plot_data$goal_ratio),
           label = "PASSIVE",    hjust = 0, vjust = 0,
           color = "steelblue",  fontface = "bold", size = 4) +
  annotate("text",
           x = max(plot_data$turnover_ratio),
           y = min(plot_data$goal_ratio),
           label = "STRUGGLING", hjust = 1, vjust = 0,
           color = "firebrick",  fontface = "bold", size = 4) +
  
  #Labels & theme
  labs(
    title    = "UFA Team Efficiency",
    subtitle = "Dashed lines = league median",
    x        = " Turnover Ratio",
    y        = "Goal Ratio",
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),  
    plot.subtitle = element_text(face = "italic", size = 10, color = "gray40",hjust = 0.5),  
    panel.grid    = element_line(color = "gray90"))


#c) Throw type based on the teams stack bar
# idea of pointing out the teams we are choosing for presentation
full_team_stats|>
  select(team, win_pct, short_throw_rate, medium_throw_rate, long_throw_rate) |>
  pivot_longer(-c(team, win_pct), names_to = "throw_type", values_to = "rate") |>
  mutate(throw_type = recode(throw_type,
                             "short_throw_rate"  = "Short (=<10)",
                             "medium_throw_rate" = "Medium (10-35)",
                             "long_throw_rate"   = "Long (>=35)"
  )) |>
  ggplot(aes(x = reorder(team, win_pct),   # reorder teams by win_pct
             y = rate,                  
             fill = throw_type)) +      
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(rate, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            size = 2.5, color = "white", fontface = "bold") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()+
  labs(
    title    = "Throw Profile by Team",
    subtitle = "Distribution of short, medium and long throws",
    x        = "Team",
    y        = "Proportion of Throws",
    fill     = "Throw Type"
  ) +
  theme_minimal()+
  theme(                                  
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
    axis.text.x   = element_text(face = "italic", size = 10),
    axis.text.y   = element_text(face = "italic", size = 10),
    axis.title.x  = element_text(face = "bold", size = 12),
    axis.title.y  = element_text(face = "bold", size = 12),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
  )

#d) Throw Rate by Goal

full_team_stats|>
  select(team, win_pct, short_goal_rate, medium_goal_rate, long_goal_rate) |>
  pivot_longer(-c(team, win_pct), names_to = "throw_type", values_to = "rate") |>
  mutate(throw_type = recode(throw_type,
                             "short_throw_rate"  = "Short (=<10)",
                             "medium_throw_rate" = "Medium (10-35)",
                             "long_throw_rate"   = "Long (>=35)"
  )) |>
  ggplot(aes(x = reorder(team, win_pct),   # reorder teams by win_pct
             y = rate,                  
             fill = throw_type)) +      
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(rate, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            size = 2.5, color = "white", fontface = "bold") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()+
  labs(
    title    = "Throw Profile by Team",
    subtitle = "Distribution of short, medium and long throws",
    x        = "Team",
    y        = "Proportion of Throws",
    fill     = "Throw Type"
  ) +
  theme_minimal()+
  theme(                                  
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
    axis.text.x   = element_text(face = "italic", size = 10),
    axis.text.y   = element_text(face = "italic", size = 10),
    axis.title.x  = element_text(face = "bold", size = 12),
    axis.title.y  = element_text(face = "bold", size = 12),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
  )

# e) Turnover rate by the throw -> scale percent
full_team_stats|>
  select(team, win_pct, short_throw_turnover_rate, medium_throw_turnover_rate, long_throw_turnover_rate) |>
  pivot_longer(-c(team, win_pct), names_to = "throw_type", values_to = "rate") |>
  mutate(throw_type = recode(throw_type,
                             "short_throw_rate"  = "Short (=<10)",
                             "medium_throw_rate" = "Medium (10-35)",
                             "long_throw_rate"   = "Long (>=35)"
  )) |>
  ggplot(aes(x = reorder(team, win_pct),   # reorder teams by win_pct
             y = rate,                  
             fill = throw_type)) +      
  geom_col(position = "fill") +
  geom_text(aes(label = scales::percent(rate, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            size = 2.5, color = "white", fontface = "bold") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()+
  labs(
    title    = "Throw Profile by Team",
    subtitle = "Distribution of short, medium and long throws",
    x        = "Team",
    y        = "Proportion of Throws",
    fill     = "Throw Type"
  ) +
  theme_minimal()+
  theme(                                  
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
    axis.text.x   = element_text(face = "italic", size = 10),
    axis.text.y   = element_text(face = "italic", size = 10),
    axis.title.x  = element_text(face = "bold", size = 12),
    axis.title.y  = element_text(face = "bold", size = 12),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
  )

#f) Goal difference (column +-) - like in the pdf - ranking (lolipop)
plot_data |>
  ggplot(aes(x = reorder(team, plus_minus), y = plus_minus))+
  geom_segment(aes(xend = team, y = 0, yend = plus_minus, color = plus_minus > 0),linewidth = 1.5)+
  geom_image(aes(image = logo_url)) + # logos instead of points
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "salmon")) +
  scale_y_continuous(breaks = seq(-500, 275, by = 50))+
  coord_flip()+
  guides(color = "none") + 
  labs(
    title = "Goal Differential (+/-) by Team",
    subtitle = "2021 - 2024 Season",
    x = "Team", y = "+/-"
  ) +
  theme_minimal()+
  theme_bw()+
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 10, hjust = 0.5),
    axis.text.x = element_text(face = "italic", size = 10),
    axis.text.y = element_text(face = "italic", size = 10),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12))
    
#g) Home vs Away Win%  -> column to see where teams have better performance

#h) Correaltion map

# 10. Roadmap for the future exploration ---------------------------------------

#To Be Done 
# - developing more metrics -> ofenssive, defenesive, game_momentum (time stamp)
# - creating cool visuals

#a) Player Level Analysis
# Who is the best duo ? (thrower - reciver)
# Who is the clutch player  for Q4 (with left time or in Q5)
# Player Impact Score - How much does a player's throw help their team? -> creating a impact_score = goals*2 - turnover*-1
# Who is the MVP of the UFA and what impact for the team they have 

#b) Season Trends
# - Team improvement/decline over seasons
# - Player development trajectories 
# - Did tactical changes show up in numbers? ( by changing long,medium, short throws)

#c) Game Analysis - Advanced
# - Score progression (how did the lead change?)
# - Momentum shifts (which quarter flipped the game?)
# - Comeback games vs dominant wins
# - Close games (decided by 1-2 goals) vs blowouts

#d) Clustering
# - Team playstyle clusters (aggressive/defensive/balanced)
# -  Player role clusters (hybrid/handler/cutter)


# 11. Exploring for me based on the lectures  ----------------------------------

# a) Doing more with group_by() and summarize() -> Lecture 2
# b) Better looking tables with gt(), rename() -> customize tables -> Lecture 2
# c) Visualizing 1D categorical data -> geom_bar() -> Lecture 3 
# d) Visualizing 2D categorical data -> geom_col(), stack bar -> Lecture 3
# e) Train with pivot_wider() and pivot_longer() -> Lecture 3
# f) Heatmaps (geom_tile) -> Lecture 3
# g) Mosaic Plot -> Lecture 3
# h) Facets -> Many Plots -> Lecture 3
# i) Boxplots -> Lecture 4
# j) Histograms , Density, Beeswarm, ECDF plot (1D and 2D), ridgeline plot -> Lecture 4
# k) scatterplot + regression -> Lecture 4
# l) creating a denisty heatmap of throws, creating a hexagonal_heatmaps -> Lecture 5
# m) k-means clustering -> Lecture 6
# n) dendogram trees -> Lecture 7
# o) soft clustering -> Lecture 8

