rm(list=ls())

library('tidyverse')
library('dplyr')
library(readxl)
library("ggplot2")
library(brms)

Salary_2020<-read_excel("/Users/AustinTheBoss/Documents/COSC R Files/Final Project Code/MLB Player Salaries 2020_to_2023.xlsx",
                   sheet = "2020")
Salary_2021<-read_excel("/Users/AustinTheBoss/Documents/COSC R Files/Final Project Code/MLB Player Salaries 2020_to_2023.xlsx",
                        sheet = "2021")
Salary_2022<-read_excel("/Users/AustinTheBoss/Documents/COSC R Files/Final Project Code/MLB Player Salaries 2020_to_2023.xlsx",
                        sheet = "2022")
Salary_2023<-read_excel("/Users/AustinTheBoss/Documents/COSC R Files/Final Project Code/MLB Player Salaries 2020_to_2023.xlsx",
                        sheet = "2023")
Injury_2020<-read_excel("/Users/AustinTheBoss/Documents/COSC R Files/Final Project Code/MLB Injuries 2020_to_2023.xlsx",
                   sheet = "2020")
Injury_2021<-read_excel("/Users/AustinTheBoss/Documents/COSC R Files/Final Project Code/MLB Injuries 2020_to_2023.xlsx",
                        sheet = "2021")
Injury_2022<-read_excel("/Users/AustinTheBoss/Documents/COSC R Files/Final Project Code/MLB Injuries 2020_to_2023.xlsx",
                        sheet = "2022")
Injury_2023<-read_excel("/Users/AustinTheBoss/Documents/COSC R Files/Final Project Code/MLB Injuries 2020_to_2023.xlsx",
                        sheet = "2023")

PitcherStats<-read_excel("/Users/AustinTheBoss/Documents/COSC R Files/Final Project Code/mlb-player-stats-P 2020_2023.xlsx")
BatterStats<-read_excel("/Users/AustinTheBoss/Documents/COSC R Files/Final Project Code/mlb-player-stats-Batters 2020_2023.xlsx")

#First & Last Day of Regular season each year
Seasons_Start <- as.Date(c("2020-07-23", "2021-4-01", "2022-4-07", "2023-3-30"))
Seasons_End <- as.Date(c("2020-09-27", "2021-10-03", "2022-10-02", "2023-10-01"))
Season_Length <- as.numeric(difftime(Seasons_End, Seasons_Start, units = "days"))


#Anyone who did not return in the regular season will have their return date set to the last day of the season
Injury_2020$`Return Date` <- replace(Injury_2020$`Return Date`, is.na(Injury_2020$`Return Date`), Seasons_End[1])
Injury_2021$`Return Date` <- replace(Injury_2021$`Return Date`, is.na(Injury_2021$`Return Date`), Seasons_End[2])
Injury_2022$`Return Date` <- replace(Injury_2022$`Return Date`, is.na(Injury_2022$`Return Date`), Seasons_End[3])
Injury_2023$`Return Date` <- replace(Injury_2023$`Return Date`, is.na(Injury_2023$`Return Date`), Seasons_End[4])

Injury_2020 <- Injury_2020 %>% rename("Player" = "Name")
Injury_2021 <- Injury_2021 %>% rename("Player" = "Name")
Injury_2022 <- Injury_2022 %>% rename("Player" = "Name")
Injury_2023 <- Injury_2023 %>% rename("Player" = "Name")
Injury_2020$Injured <- 1
Injury_2021$Injured <- 1
Injury_2022$Injured <- 1
Injury_2023$Injured <- 1

Data_2020 <- merge(Salary_2020, Injury_2020, by='Player', all.x = TRUE)
Data_2021 <- merge(Salary_2021, Injury_2021, by='Player', all.x = TRUE)
Data_2022 <- merge(Salary_2022, Injury_2022, by='Player', all.x = TRUE)
Data_2023 <- merge(Salary_2023, Injury_2023, by='Player', all.x = TRUE)

Data <- rbind(Data_2020,Data_2021,Data_2022,Data_2023)
Data <- Data %>% rename("Playoffs" = "Playoff Team")
Data <- Data %>% rename("Injury" = "Injury / Surgery")
Data <- Data %>% rename("Service_Time" = "Service Time")
Data <- Data %>% rename("Return_Date" = "Return Date")
Data <- Data %>% rename("IL_Retro_Date" = "IL Retro Date")
Data$Duration <- as.numeric(difftime(Data$Return_Date, Data$IL_Retro_Date, units = "days"))
Data$Injured <-replace(Data$Injured, is.na(Data$Injured), 0)
#have to remove the NAs
Data$Salary <- as.numeric(Data$Salary)
Data<-(Data[complete.cases(Data$Salary), ])

Data = subset(Data, select = -c(Team.y,Year.y, `Injury / Surgery Date`) )
Data <- Data %>% rename("Year" = "Year.x")
Data <- Data %>% rename("Team" = "Team.x")


rm(Salary_2020, Salary_2021, Salary_2022, Salary_2023, Injury_2020, Injury_2021, Injury_2022, Injury_2023, Data_2020,Data_2021,Data_2022,Data_2023)

#fill in playoff team data
Playoffs_2020 <- c("TBR","TOR","CLE","NYY","MIN","HOU","OAK","CHW","LAD","MIL","SDP", "STL","CHC","MIA","ATL","CIN")
Playoffs_2021 <- c("BOS","TBR","NYY","CHW","HOU","LAD","STL","SFG","ATL","MIL")
Playoffs_2022 <- c("TBR","CLE","SEA","TOR","NYY","HOU","PHI","STL","SDP","NYM","LAD","ATL")
Playoffs_2023 <- c("TOR","MIN","TEX","TBR","HOU","BAL","PHI","MIA","MIL","ARI","LAD","ATL")

check_playoffs <- function(year, team) {
  switch(
    as.character(year),
    "2020" = team %in% Playoffs_2020,
    "2021" = team %in% Playoffs_2021,
    "2022" = team %in% Playoffs_2022,
    "2023" = team %in% Playoffs_2023,
    NA
  )
}

# Apply the function to each row of data
Data$Playoffs <- mapply(check_playoffs, Data$Year, Data$Team)

# Convert logical values to 1 or 0
Data$Playoffs <- as.integer(Data$Playoffs)
summary(Data)


Data <- Data %>%
  group_by(Player) %>%
  fill(Pos, .direction = "downup")

#Remove players without position value
Data<-(Data[complete.cases(Data$Pos), ])


Data <- Data %>%
  arrange(Player) %>%
  mutate(Age = first(Age) + (Year - first(Year)))

#Distinguish between position players and pitchers
Data <- Data %>%
  mutate(Position = ifelse(grepl("SP", Pos) | grepl("RP", Pos), "Pitcher", "Position"))

Data$Pitcher <- ifelse(Data$Position == "Pitcher", 1, 0)

#Group Covid's together
Data <- Data %>%
  mutate(Injury_Category = ifelse(grepl("^COVID-19", Injury), "COVID", Injury))

#Upper vs Lower Body Injuries, only used body parts with more than 5 occurences
Data <- Data %>%
  mutate(TypeInj = ifelse(
                      #Upper Body Parts
                           grepl("Arm", Injury_Category, ignore.case = TRUE) |
                           grepl("Abdominal", Injury_Category, ignore.case = TRUE)|
                           grepl("Back", Injury_Category, ignore.case = TRUE)|
                           grepl("Bicep", Injury_Category, ignore.case = TRUE)|
                           grepl("Elbow", Injury_Category, ignore.case = TRUE) |
                           grepl("Finger", Injury_Category, ignore.case = TRUE) |
                           grepl("Neck", Injury_Category, ignore.case = TRUE) |
                           grepl("Wrist", Injury_Category, ignore.case = TRUE)|
                           grepl("Tricep", Injury_Category, ignore.case = TRUE)|
                           grepl("Tommy", Injury_Category, ignore.case = TRUE)|
                           grepl("Thumb", Injury_Category, ignore.case = TRUE)|
                           grepl("Thoracic", Injury_Category, ignore.case = TRUE)|
                           grepl("Shoulder", Injury_Category, ignore.case = TRUE)|
                           grepl("Oblique", Injury_Category, ignore.case = TRUE)|
                           grepl("Lat", Injury_Category, ignore.case = TRUE)|
                           grepl("Intercostal", Injury_Category, ignore.case = TRUE)|
                           grepl("Hand", Injury_Category, ignore.case = TRUE)|
                           grepl("Pec", Injury_Category, ignore.case = TRUE)|
                           grepl("Concussion", Injury_Category, ignore.case = TRUE), "Upper",
                       #Lower Body Parts  
                           ifelse(grepl("Toe", Injury_Category, ignore.case = TRUE) |
                                  grepl("Ankle", Injury_Category, ignore.case = TRUE)  |
                                  grepl("Foot", Injury_Category, ignore.case = TRUE) |
                                  grepl("Achilles", Injury_Category, ignore.case = TRUE) |
                                  grepl("Calf", Injury_Category, ignore.case = TRUE) |
                                  grepl("Knee", Injury_Category, ignore.case = TRUE) |
                                  grepl("Hip", Injury_Category, ignore.case = TRUE) |
                                  grepl("Plantar", Injury_Category, ignore.case = TRUE) |
                                  grepl("Groin", Injury_Category, ignore.case = TRUE) |
                                  grepl("Hamstring", Injury_Category, ignore.case = TRUE) |
                                  grepl("Quad", Injury_Category, ignore.case = TRUE) |
                                  grepl("Heel", Injury_Category, ignore.case = TRUE), 
                                  "Lower", Injury_Category)))


Data$SeasonGames <- ifelse(Data$Year == 2020, 60, 162)
Data$SeasonDays <- ifelse(Data$Year == 2020, Season_Length[1],ifelse(Data$Year == 2021, Season_Length[2], ifelse(Data$Year == 2022, Season_Length[3], Season_Length[4])))
Data$GameRatio <- Data$SeasonDays/Data$SeasonGames
Data$GamesMissed <- Data$Duration/Data$GameRatio
Data$PayPerGame <- Data$Salary/Data$SeasonGames
Data$MoneyLost <- Data$PayPerGame * Data$GamesMissed

# Separate by injured and uninjured
Injured_Players <- Data %>%
  filter(Duration >0, Duration <= 300, Injured == 1)

Healthy_Players <- Data %>%
  filter(Injured == 0)

summary(Injured_Players)
summary(Healthy_Players)

#Get average money lost by players per year, 
MoneyLost_ByYear<-Injured_Players%>%
  group_by(Year,Team)%>%
  summarise(sum(MoneyLost))

TeamMoney<-MoneyLost_ByYear%>%
  group_by(Year)%>%
  summarise(mean(`sum(MoneyLost)`))
TeamMoney <- TeamMoney %>% rename("MoneyLost" = "mean(`sum(MoneyLost)`)")
TeamMoney$MoneyLost <- round(TeamMoney$MoneyLost)


#Classify Injuries

Injured_Players$SevereInjury <- ifelse(Injured_Players$Duration >= mean(Injured_Players$Duration), 1, 0)

Upper_Body <- Injured_Players %>%
  filter(TypeInj == "Upper")

Lower_Body <- Injured_Players %>%
  filter(TypeInj == "Lower")

UpperLower_Inj <-Injured_Players %>%
  filter(TypeInj == "Lower" | TypeInj == "Upper")


Team_ByYear<-Injured_Players%>%
  group_by(Team,Year)%>%
  summarise(Total_Players=n())

Injured_counts <- Data %>%
  group_by(Year, Team) %>%
  summarise(Injured = sum(Injured),
            MadePlayoffs = max(Playoffs))

avg_injured <- mean(Injured_counts$Injured)

# Create a new column indicating whether the team had more than the average number of injured players
Injured_counts <- Injured_counts %>%
  mutate(MoreThanAvg = Injured > avg_injured)

Injured_counts$MoreThanAvg <- ifelse(Injured_counts$MoreThanAvg == FALSE, 0, 1)



#Injuries separated by position, using for bar chart
Injury_ByPos<-Data%>%
  group_by(Pitcher,Injured)%>%
  summarise(Total_Players=n())

Injury_ByPos$Status <- ifelse(Injury_ByPos$Pitcher == 0 & Injury_ByPos$Injured == 0 , "PosPlayer_NoInj", ifelse(Injury_ByPos$Pitcher == 0 & Injury_ByPos$Injured == 1 , "PosPlayer_Inj", ifelse(Injury_ByPos$Pitcher == 1 & Injury_ByPos$Injured == 0 , "Pitcher_NoInj", ifelse(Injury_ByPos$Pitcher == 1 & Injury_ByPos$Injured == 1 , "Pitcher_Inj","NA"))))


#Divide position players and pitchers into 2 dataframes
Pitchers <- Data %>%
  filter(Pitcher == 1)

Batters <- Data %>%
  filter(Pitcher == 0)
  
#Clean up data
Pitchers <- merge(Pitchers, PitcherStats, by= c("Player", "Year"), all.x = TRUE)
Pitchers <- Pitchers %>%
  distinct(Player, IL_Retro_Date, .keep_all = TRUE)

Batters <- merge(Batters, BatterStats, by= c("Player", "Year"), all.x = TRUE)
Batters <- Batters %>%
  distinct(Player, IL_Retro_Date, .keep_all = TRUE)

Pitchers <- subset(Pitchers, select = -c(Team.y,Age.y) )
Pitchers <- Pitchers %>% rename("Team" = "Team.x","Age" = "Age.x")

Batters <- subset(Batters, select = -c(Team.y,Age.y,Pos.y) )
Batters <- Batters %>% rename("Team" = "Team.x","Age" = "Age.x","Pos" = "Pos.x")




#Injury by Location Table
Injury_Location<-Injured_Players%>%
  group_by(Injury_Category)%>%
  summarise(Total_Players=n())
Injury_Location<-filter(Injury_Location, Total_Players > 1)
summary(Injury_Location)

Severe_UpLow<-Injured_Players%>%
  group_by(TypeInj)%>%
  summarise(Total_Players=n())
Severe_UpLow<-filter(Severe_UpLow, Total_Players > 15)


#Injury types not including "Undisclosed" and with > 5 occurrences

Injury_Summary<-Data%>%
  group_by(Injured)%>%
  summarise(Total_Players=n())

Injury_Summary$Status <- ifelse(Injury_Summary$Injured == 0, "Healthy", "Injured")

Severe<-Injured_Players%>%
  group_by(SevereInjury)%>%
  summarise(Total_Players=n())

SevereType<-filter(Injury_Location, Total_Players > mean(Total_Players))


Data$Veteran <- ifelse(Data$Service_Time > mean(Data$Service_Time),1,0)
Data$HighPaid <- ifelse(Data$Salary > mean(Data$Salary),1,0)

Data$Veteran <- ifelse(Data$Service_Time > mean(Data$Service_Time),1,0)
Data$HighPaid <- ifelse(Data$Salary > mean(Data$Salary),1,0)



#HYPOTHESIS TESTING

#Null: The average duration of injury time for upper body injuries is greater than
#The average duration of injuries for lower body injuries 

#H0: P(Mu_Up) > P(Mu_Lo)
#HA: P(Mu_Up) <= P(Mu_Lo)

#P = proportion of players who have suffered an injury
#Mu_Up = Average injury duration for upper body injuries
#Mu_Lo = Average injury duration for lower body injuries


t.test(Lower_Body$Duration, Upper_Body$Duration)



#Null: the average duration of pitcher injuries is greater than the 
#average duration of non-pitcher injuries.

#H0: P(Pi) > P(NPi)
#HA: P(Pi) <= P(NPi)

#P = proportion of players who have suffered an injury
#Pi = Average injury duration for pitcher injuries
#NPi = Average injury duration for non-pitcher injuries

t.test(Batters$Duration, Pitchers$Duration)


##Testing the null hypothesis that Injury occurrence
##does differ by Service_Time 
t.test(Data$Service_Time~Data$Injured)







#APPLIED PROBABILITY

#Probability a player is injured given they have been in the MLB longer than the average player. 

InjuredVet <- table(Data$Injured, Data$Veteran)

# Calculate the conditional probability table P(Injured | Vet)
conditional_IV <-
  prop.table(InjuredVet, margin = 1)

print(conditional_IV)



#Probability a player is injured given they are a pitcher

InjuredPosition <- table(Data$Injured, Data$Pitcher)

# Calculate the conditional probability table P(Injured | Pitcher)
conditional_IP <-
  prop.table(InjuredPosition, margin = 1)

print(conditional_IP)



#Probability that a player is paid more than MLB average given they were 
#injured that season

InjurySalary <- table(Data$Injured, Data$HighPaid)

# Calculate the conditional probability table P(Salary > Average | Injured)
conditional_IS <-
  prop.table(InjurySalary, margin = 1)

print(conditional_IS)



#Probability a team made the playoffs given they had more than the average number
# of injured players

InjuredPlayoffs <- table(Injured_counts$MoreThanAvg, Injured_counts$MadePlayoffs)

# Calculate the conditional probability table P(Playoffs | Injured > Average)
conditional_InjPlayoffs <-
  prop.table(InjuredPlayoffs, margin = 1)

print(conditional_InjPlayoffs)




#LINEAR REGRESSION 


#Measures of dispersion for age, check the variability for everyone's age in the first year
Age <- Data %>%
  filter(Year == 2020)

summary(Age)

boxplot(Age$Age,
        las=2,xlab="Age",ylab="MLB Player Ages",main="Variation in Player Age (2020)",col = "blue")


#All Player Data set
model1<-lm(Injured~Age+Service_Time+Pitcher, data=Data)
summary(model1)

model2<-lm(Injured~Age+Service_Time+Pitcher+Salary, data=Data)
summary(model2)

#Is there a disproportionate effect of service time on durability given age
model3<-lm(Injured~Age+Service_Time+Pitcher+Salary+(Age*Service_Time), data=Data)
summary(model3)


#Batter Data set
BatterModel1<-lm(Injured~Age+Service_Time+Games+PlateAppearances, data=Batters)
summary(BatterModel1)

BatterModel2<-lm(Injured~Age+Service_Time+Games+PlateAppearances+StolenBases, data=Batters)
summary(BatterModel2)

BatterModel3<-lm(Injured~Age+Service_Time+Games+PlateAppearances+StolenBases+Playoffs, data=Batters)
summary(BatterModel3)


#Pitcher Data set
PitcherModel1<-lm(Injured~Age+Service_Time+Games+GamesStarted, data=Pitchers)
summary(PitcherModel1)

PitcherModel2<-lm(Injured~Age+Service_Time+Games+GamesStarted+ InningsPitched, data=Pitchers)
summary(PitcherModel2)

PitcherModel3<-lm(Injured~Age+Service_Time+Games+GamesStarted+InningsPitched+Playoffs, data=Pitchers)
summary(PitcherModel3)




PlayersByDuration <- Injured_Players%>%
  group_by(Duration)%>%
  summarise(Total_Players=n())





#VISUALS

DurationPlot <- ggplot(PlayersByDuration, aes(x = Duration, y = Total_Players)) +
  geom_point()
DurationPlot


ggplot(Severe, aes(x = SevereInjury, y = Total_Players, fill = SevereInjury)) +
  xlab("Injury with Duration Longer than Average") + ylab("Total Players")+
  geom_col()

#Shows Teams injury occurrences over the years
ggplot(Team_ByYear, aes(x = Total_Players, y = Team)) +
    xlab("Total Injured Players") + ylab("MLB Team")+
  geom_point(size = 1, aes(colour = Year))


MoneyPlot<-ggplot(TeamMoney,
                aes(x = Year,y = MoneyLost, fill = Year),) + 
  geom_col(fill = "red", colour = "black")+
  xlab("Season") + ylab("Average Money Lost by Teams Due to Injury")+
  geom_text(aes(label = MoneyLost),vjust = 1.5, nudge_y = 50, colour = "white")
MoneyPlot


InjPlot<-ggplot(Injury_Summary,
             aes(x = Status,y = Total_Players, fill = Status),) + 
  geom_bar(stat = "identity")+ 
  xlab("Player Health Status") + ylab("Total Players")+
  scale_fill_manual(values=c("green", 
                             "red")) +
  geom_text(aes(label = signif(Total_Players)), nudge_y = 50)
InjPlot


#Plot of Severe Injury bar plot, by type of injury
SeverePlot<-ggplot(Severe_UpLow,
                aes(x = TypeInj,y = Total_Players, fill = TypeInj),) + 
  xlab("Type of Injury") + ylab("Total Players")+
  geom_bar(stat = "identity")+
  geom_text(aes(label = signif(Total_Players)), nudge_y = 50)
SeverePlot


#Injury Status by Position
InjPosPlot<-ggplot(Injury_ByPos,
                aes(x = Status,y = Total_Players, fill = Status),) + 
  geom_bar(stat = "identity")+ 
  xlab("Injury Status Given Position") + ylab("Total Players")+
  scale_fill_manual(values=c("red", 
                             "green",
                             "red", 
                             "green")) +
  geom_text(aes(label = signif(Total_Players)), nudge_y = 50)
InjPosPlot


