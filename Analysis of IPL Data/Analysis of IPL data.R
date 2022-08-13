############## Final Assessment - Module 8 (Q. No. - 11) ########################

# Q. No. 11 - NOTE: You need to perform this using R Studio

# Using the IPL data given below write R code to derive the solution for following questions: 
  
# Question 1: Considering that a match score of greater than 300 is treated as "High Scoring Match" 
# and match score less than or equal to 300 is treated as "Low scoring match", find the solution for: 
  
# a) Which two season have equal percentage of High Scoring Matches

# b) Which season have 43% matches as High Scoring Matches.

# Question 2: Find the top 10 batsman based on number of centuries.

# Question 3: Find the top 10 batsman based on batting average (which is total runs by total matches played).

# Question 4: What is the percentage of matches won by team winning the toss?

############### Installing packages ###################

library('readxl')
library('dplyr')
library('ggplot2')
library('e1071')

# To set the working directory to a specific folder

setwd("C:/Users/Aman Garg/Desktop/Data Science Classes/Assignments/Assignment - Module 8/Final Assessment")

getwd()

################# Importing files ############################

# Lets import the data sets

balls = read_excel("Ball-by-Ball.xlsx")

match = read_excel("Match.xlsx")

player = read_excel("Player.xlsx")

season = read_excel("Season.xlsx")

team = read_excel("Team.xlsx")

# We do not need Extra_Type column so we will delete it.
balls1 = balls[-13]

# We need to replace null values in Extra_Runs column with 0.
balls1[is.na(balls1)] = 0
balls1_new = merge(balls1, player, by.x = 'Striker_Id', by.y = 'Player_Id', all.x = TRUE)

################ a) Two seasons having equal percentage of High Scoring Matches ##################

# Lets find the scores of matches having total > 300 first
scores_per_match = balls1_new %>% group_by(Match_Id,Season_Id) %>% 
  summarize(Total_Runs = sum(sum(Batsman_Scored) + sum(Extra_Runs))) %>% filter(Total_Runs > 300)

# Now lets find the no. of matches having total runs > 300
no._of_hsm  = scores_per_match %>% group_by(Season_Id) %>% count(Season_Id)

# Lets rename the column 
colnames(no._of_hsm)[2] <- 'No._of_High_Scoring_Matches'

# Now lets find the no. of matches per season
matches_per_season = match %>% group_by(Season_Id) %>% count(Season_Id)

# Lets rename the column
colnames(matches_per_season)[2] <- 'No._of_Matches_per_Season'

# Lets create a merged data set
a = merge(no._of_hsm, matches_per_season, by.x = 'Season_Id', by.y = 'Season_Id', all.x = TRUE)

# Now lets find percentage of matches having total runs > 300 season wise
per_of_hsm = a %>% group_by(Season_Id,No._of_High_Scoring_Matches,No._of_Matches_per_Season) %>%
  summarize(Percentage = ((No._of_High_Scoring_Matches/No._of_Matches_per_Season)*100))

# Lets find the seasons having same percentage of high scoring matches
b = per_of_hsm %>% group_by(Season_Id,No._of_High_Scoring_Matches,No._of_Matches_per_Season) %>%
  count(Percentage)

# Since there is not any value of count = 2, therefore no season have same percentage 
# of high scoring matches.

###################### b) Season have 43% matches as High Scoring Matches ###################

c = per_of_hsm %>% group_by(Season_Id,No._of_High_Scoring_Matches,No._of_Matches_per_Season) %>%
  filter(Percentage >= 43)

# There are total of 7 seasons out of 9 which have more than 43% matches as "High Scoring Matches".


###################### Top 10 players based on no. of centuries score #######################

# Lets first find out the centuries scored by players match wise 
centuries_scored = balls1_new %>% group_by(Match_Id,Striker_Id,Player_Name) %>% 
  summarize(Runs_by_Batsman = sum(Batsman_Scored)) %>%
  filter(Runs_by_Batsman >= 100)

# Now lets find out the total no. of centuries scored by individual players
centuries_by_individual_player = centuries %>% group_by(Striker_Id,Player_Name) %>% 
  count(Striker_Id) %>% arrange(-n)

# Lets rename the column
colnames(centuries_by_individual_player)[3] <- 'No._of_Centuries'

# Since last 18 players scored 1 century each so they all will take the 10th place.

################## Top 10 batsman based on batting average ###########################

# Lets find the total no. of runs scored by individual players
runs_per_batsman = balls1_new %>% group_by(Striker_Id, Player_Name) %>% 
  summarize(Runs_by_Batsman = sum(Batsman_Scored))
  
# Lets find the no. of matches played by each player
e = balls %>% group_by(Match_Id) %>% count(Striker_Id)

f= e %>% group_by(Striker_Id) %>% count(Striker_Id)

# Lets rename the column
colnames(f)[2] <- 'No._of_matches_played_by_each_player'

g = merge(runs_per_batsman, f, by.x = 'Striker_Id', by.y = 'Striker_Id', all.x = TRUE)

# Top 10 batsman based on the average
average = g %>% group_by(Striker_Id,Player_Name,Runs_by_Batsman,No._of_matches_played_by_each_player) %>% 
  summarize(Average = (Runs_by_Batsman/No._of_matches_played_by_each_player)) %>%
  arrange(-Average) %>% head(10)

######################## Percentage of matches won by team winning the toss ###################

# Lets find the no. of matches where the team who won the toss also won the match
h = match %>% select(Match_Id,Toss_Winner_Id, Match_Winner_Id) %>% filter(Toss_Winner_Id == Match_Winner_Id)
i = h %>% count(Match_Id)
j = i %>% summarize(Total_no._such_matches = sum(n))

# Lets find the no. of matches where there was a definite result (i.e. excluding "NO RESULT" matches)
k = match %>% filter(IS_Result == 1) %>% count(IS_Result) 

# Now the percentage of matches where the team who won the toss also won the match
l = (j[1,1]/k[1,2])*100
l

# Therefore the percentage of such matches is 50.70.

########################## THE END ################################