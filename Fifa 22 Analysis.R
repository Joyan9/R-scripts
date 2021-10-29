library(tidyverse) #contains dplyr and ggplot
library(e1071)
setwd(choose.dir()) # setting the working directory, choose.dir allows to select the folder
players <- read.csv('players_22.csv')
teams <- read.csv('teams_fifa22.csv')
str(players)
isl_players <- subset(players, players$league_name == 'Indian Super League')
View(isl_players)
dimnames(isl_players)
isl_players <- subset(isl_players, select = -c(sofifa_id,player_url,short_name,league_name,
                                               club_jersey_number,club_loaned_from,club_joined,
                                               club_contract_valid_until,nation_jersey_number))

ggplot(data= isl_players,aes(age)) + 
  geom_histogram(breaks = seq(0,60, by=2), colour = 'black')+
  labs(title = 'Histogram of Player Age', 
       x = 'Age', y = 'Number of Players')
mean(players$age) # average age 25
mean(players$potential) # 72
mean(players$overall) # 65

#top 10 players with highest overall
isl_players %>% 
  select(long_name, overall) %>% 
  arrange(desc(overall)) %>% 
  slice(1:10)

#top 10 players with highest potential
isl_players %>% 
  select(long_name, age, overall, potential) %>% 
  arrange(desc(potential)) %>% 
  slice(1:10)



#top 10 youngest players with highest overalls and potentials
isl_players %>%
  select(long_name,age, overall, potential) %>% 
  arrange((age)) %>% 
  slice(1:10)



#plotting club wise count of players above avg rating 
print(mean(isl_players$overall))
players_above_mean <- isl_players %>% 
                        filter(overall >= 65) %>% 
                        group_by(club_name) %>% 
                        tally() %>% 
                        arrange((desc(n)))
ggplot(players_above_mean, aes(x = factor(club_name), y = n)) +
  geom_col(color = 'black', fill = 'blue')+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = 'Teams with Players Above Average Overall (65)', 
       x = 'Teams', y = 'Number of Players')

#plotting club wise count of players above avg potential
mean(isl_players$potential) # 62
players_potential_above_mean <- isl_players %>%
  filter(potential >= 62) %>%
  group_by(club_name) %>%
  tally() %>%
  arrange((desc(n)))
ggplot(players_potential_above_mean, aes(x = factor(club_name), y = n))+
  geom_col(color = 'black', fill = 'pink') +
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = 'Teams with Players Above Average Potential (62)',
       x = 'Teams', y = 'Number of Players')


#making a ISL dream 11
# 1 GK, 1 LCB + 1 RCB, 1 LB + 1 RB, 1 CDM, 2 CM, 1 RW, 1 LW, 1 ST 
unique(isl_players$club_position)

#finding max overall on all positions 
values <- isl_players %>%
  group_by(club_position) %>%
  summarise(max = max(overall))

#Best GK
gk <- isl_players %>% select(long_name, overall, club_position)%>% filter(overall >= 64,
                                                                          club_position == "GK") 

lcb <- isl_players %>% select(long_name, overall, club_position)%>% filter(overall == 71,
                                                                           club_position == 'LCB')
lb <- isl_players %>% select(long_name, overall, club_position)%>% filter(overall == 60,
                                                                           club_position == 'LB')
rb <- isl_players %>% select(long_name, overall, club_position)%>% filter(overall == 61,
                                                                                 club_position == 'RB')

rcb <- isl_players %>% select(long_name, overall, club_position)%>% filter(overall == 66,
                                                                                  club_position == 'RCB') %>% slice(1)
# using slice since there were 2 max rated RCB
cdm <- isl_players %>% select(long_name, overall, club_position)%>% filter(overall == 70,
                                                                           club_position == 'CDM') 
cam <- isl_players %>% select(long_name, overall, club_position)%>% filter(overall >= 69,
                                                                          club_position == 'CAM')%>%slice(1:2)
st <- isl_players %>% select(long_name, overall, club_position)%>% filter(overall == 71,
                                                                          club_position == 'ST') 
rs <- isl_players %>% select(long_name, overall, club_position)%>% filter(overall == 67,
                                                                          club_position == 'RS') 
ls <- isl_players %>% select(long_name, overall, club_position)%>% filter(overall == 72,
                                                                          club_position == 'LS') 

dream_11 <- data.frame()
dream_11 <- rbind(gk,lcb,rcb,lb,rb,cdm,cam,st,rs,ls)
View(dream_11)
