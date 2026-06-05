### Ultimate Frisbee (Throws)
### Julia Biesiada
### 06/05/2026

## Data Loading
library(tidyverse)

ufa_throws <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/ufa_throws.csv")

## Data structure
dim(ufa_throws) #to check a dimension
class(ufa_throws) # check a class 
colnames(ufa_throws)  # check the column names

head(ufa_throws) #check the head of 6 

summary(ufa_throws$thrower_x) # check what is the min (-26.670) and max (26.660) 
summary(ufa_throws$thrower_y) # min = 0, max = 100
summary(ufa_throws$receiver_x) # min = -26.670, max = 26.660
summary(ufa_throws$receiver_y) # min = 0, max = 120
summary(ufa_throws$turnover) # 1 and 0 (where 1 represents turnover and 0 represents not)

summary(ufa_throws$possession_num)#represents the sequence of the play untill the goal is scored, min = 0, max = 7

summary(ufa_throws$game_quarter) # 1 - 5 quaters (4 is the basic and 5th is the overtime)
summary(ufa_throws$is_home_team) # TRUE (home team is in offense) Or False (away team is in offense) -> it depends on the situation if the team is 
summary(ufa_throws$home_team_score) # min = 0, median = 9 , max = 34
summary(ufa_throws$away_team_score) # min = 0 , median = 9, max = 36
distinct(ufa_throws, home_teamID) # 27 unique teams
distinct(ufa_throws, away_teamID) # 27 unique teams

summary(ufa_throws$times) # in seconds -> why do we have a negative and postive values for reaming game - because of the overtime  -> 
# min = - 297.0s = 4.95 min, max 2880.0s = 48 min 

#home_team_wins -> 0 is not winning, 1 is winning based on the home_team_score and away_team_score

summary(ufa_throws$score_diff) # score difference between home and away -> min = -24.0, max = 21
summary(ufa_throws$goal) # whether the throw result in goal (it reached the end zone and was catches by reciver) -> 0 is no goal, 1 is a goal
summary(ufa_throws$throw_distance) # throw distance in yards -> min = 0, median = 13.79, max = 101.21 yards
summary(ufa_throws$x_diff) # diff thrower and reciver coord -> min = -52.850, max = 53.30
# x_diff > 0  = throw moved left
# x_diff = 0  = throw stayed straight sideways
# x_diff < 0  = throw moved right

summary(ufa_throws$y_diff) #min = - 83.230, max = 99.07
# y_diff > 0  = forward throw, closer to the target end zone
# y_diff = 0  = sideways throw, same field depth
# y_diff < 0  = backward throw, away from the target end zone

summary(ufa_throws$throw_angle) # in radians -> pi number = 3.14115 means backward, 0 means forward, + means to the right, - means to the left of the thrower

## Summary -> Comments on what I found

# for x = -26.67 to 26.660   field width -> 53.33 yard wide (yards)
# y = 0 to 100          main playing area (yards)
# y = 100 to 120        target end zone 20 (yards)

## Interpretation:

# when y increases = offense moves toward the end zone
# y = 0       = far from target end zone
# y = 100     = goal line
#y = 100-120 = target end zone

# x = 0      middle of field
# x > 0      left side when facing the target end zone
# x < 0      right side when facing the target end zone

## checking why we have a - negative time -> because of the overtime -> 5th quater
ufa_throws|>
  select(game_quarter,times)|>
  filter(game_quarter == 5, times < 0)

## Possesion Number and Possesion Throw -> looking at the specific game and team (windchill)

summary(ufa_throws$possession_num) #ranges from 0 - 7
summary(ufa_throws$possession_throw) #ranges from 0 - 63

ufa_throws|>
  select(thrower,receiver,possession_num,possession_throw,home_team_score,away_team_score,away_teamID,turnover, goal,gameID)|>
  filter(gameID == "2021-07-10-MIN-IND", away_teamID == "windchill", goal == 0, turnover == 1)|>
  arrange(desc(possession_throw))

# possesion_throw -> it adds up when we have the same possession by team and is not resulting in goal or turnover
# about the turnover we saw that it can be to NA (which I assume it's out) and to someone (interception)

# Possesion Number -> represents the sequence of the play until the goal is scored
# example possesion_num = 1,team1 -> turnover team 1,possession_num 1, team2 -> turnover team 2, possession_num= 2, team 1
# it restarts when we have a goal 

ufa_throws|>
  select(thrower,receiver,possession_num,possession_throw,home_team_score,away_team_score,turnover, goal,times,gameID,away_teamID)|>
  filter(gameID == "2021-07-10-MIN-IND", away_teamID == "windchill" )|>
  arrange(desc(times), desc(possession_num)) # it not depends on the time reamining, it not depends on the specific thrower to the same person

ufa_throws|>
  select(throw_angle, thrower,receiver,possession_num,possession_throw,times,is_home_team, turnover, goal,game_quarter, gameID, away_team_score,home_team_score,away_teamID, home_teamID)|>
  filter(gameID == "2021-07-10-MIN-IND", throw_angle == 0 |throw_angle == 3.14115)|> #goal == 1 | turnover == 1)|> #thrower == "crae" - > checking if it depends on the thrower and reciver
  arrange(desc(times))|>
  print(n = 30)
  #arrange (desc(possession_num))
