###Julia Biesiada
### Title: Data Wrangling - Teams Overviews

## -- 0. Data Loading-----------------------------------------------------------
library(tidyverse)
library(ggplot2)
ufa_throws <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/ufa_throws.csv")

# -- 1. Filter out all-star games once -----------------------------------------
ufa_clean <- ufa_throws |>
  filter(home_teamID != "allstars1",
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
    long_throw_rate = round(sum(throw_distance > 20, na.rm = TRUE) / n(),3),
    # Long Goal Rate 
    long_goal_rate        = round(sum(throw_distance > 20 & goal == 1,na.rm = TRUE) /
                                    sum(goal == 1,                      na.rm = TRUE), 3),
    
          
    ## Defensive Features
    # Short throw rate — teams that play it safe
    short_throw_rate = round(sum(throw_distance < 10, na.rm = TRUE) / n(),3),
    #Medium throw rate 
    medium_throw_rate = round(sum(throw_distance > 10 & throw_distance < 20, na.rm = TRUE) / n(),3),
    #What is the long shot turnover rate?
    long_throw_turnover_rate = round(sum(throw_distance > 20 & turnover == 1, na.rm = TRUE) /
                                       sum(throw_distance > 20,            na.rm = TRUE), 3),
    # What is the medium shot turnover rate?
    medium_throw_turnover_rate = round(sum(throw_distance > 10 & throw_distance < 20 & turnover == 1, na.rm = TRUE) /
                                        sum(throw_distance > 10 & throw_distance < 20,na.rm = TRUE), 3),
    #What is the short shot turnover rate?
    short_throw_turnover_rate = round(sum(throw_distance < 10 & turnover == 1, na.rm = TRUE) /
                                      sum(throw_distance > 10,            na.rm = TRUE), 3),
    
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
ggplot(full_team_stats, aes(
  x = turnover_ratio,
  y = goal_ratio,
  size = win_pct,
  label = team))+
  geom_point(alpha = 0.8)+
  geom_text(vjust = -1, size = 3) +
  coord_fixed()+
  theme_minimal()


#c) Heat map for the Quarters -> teams and goals by quater 

#d) Heat map for thr Turnovers -> teams and goals by quarter (maybe cutting by teams who played > n games)

 
#e) Throw Profile per team by creating a bar stack of the short,medium, long throw -> tune the range of the distance


#f) Goal difference (column +-) - like in the pdf - ranking (lolipop)


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

