library(tidyverse)
library (readxl)
library(gridExtra)
library(ggrepel)

getwd()

#READING AND CLEANING DATA

#1 WC Events, Date, Time, Location
WC_Events_uncleaned <- read_csv("data/world_cups.csv",col_names= TRUE)

head(WC_Events_uncleaned)


#remove white space between column name

names(WC_Events_uncleaned) <- gsub(" ", "",colnames(WC_Events_uncleaned))


#pivot data to longer

WC_Events <- WC_Events_uncleaned %>%  pivot_longer(c(Winner, `Runners-Up`, Third, Fourth), names_to = "Standing", values_to = "Country")


#2 WC Matches

#The wC_Matches had not included results for WC 2022 joining the data with another one to retrieve the newest 2022 data

WC_Matches2022 <- read_csv("data/2022_world_cup_matches.csv", col_names = TRUE)
WC_MatchesB42022 <- read_csv("data/B42022_world_cup_matches.csv", col_names = TRUE)

head(WC_Matches2022)



#Using the temp_data to join and retrieve the readable data
temp_data <- read_csv("data/Fifa_world_cup_matches.csv", col_names = TRUE)


#Cleaning temp data to join converting data into lower case
temp_data$team1 <- str_to_title(tolower(temp_data$team1))
temp_data$team2 <- str_to_title(tolower(temp_data$team2))


#Joining and removing non-essential columns
                                            
WC_Matches2022 <- WC_Matches2022 %>% left_join(temp_data, by =c("Home Team"= "team1", "Away Team" = "team2")) %>% select(1:8, 12:13)

#Now Let's see what column this data have so we can rename column and do some cleaning

head(WC_Matches2022)


#renaming column and relocate column and changing column type for union - here we can join Home goals with numberofgoalteam 1 and Away Goals with numberofgoalteam2
WC_Matches2022 <- WC_Matches2022 %>% rename('Home Goals' = 'number of goals team1', 'Away Goals' = 'number of goals team2') %>% relocate('Home Goals', .after = 'Home Team' ) 
WC_Matches2022 <- relocate(WC_Matches2022, 'Away Goals', .after = 'Away Team')
WC_Matches2022$`Home Goals` <- as.double(WC_Matches2022$`Home Goals`)
                               
#Now that the data is sharing the same format we can union
WC_Matches <- union_all(WC_MatchesB42022, WC_Matches2022)

#removing space between column
names(WC_Matches) <- gsub(" ", "",colnames(WC_Matches))


#let's take a look at the clean version
head(WC_Matches)

#3 International Matches - same logic

International_Matches <- read_csv("data/International_Matches.csv", col_names = TRUE)


names(International_Matches) <- gsub(" ", "",colnames(International_Matches))

head(International_Matches)

#4 World cup Squads and Group

WC_Groups <- read_csv("data/2022_world_cup_groups.csv", col_names = TRUE)

names(WC_Groups) <- gsub(" ", "",colnames(WC_Groups))

head(WC_Groups)

                                  
#Moving on to the analysis steps, some of the insights we looking to answer

#1. Host Country Performance

#Select the host country matches only
Host_Country_Matches <- filter(WC_Matches, HostTeam == TRUE)

head(Host_Country_Matches)


#We use this data to find the furthest round that the host country make to

Host_Country_Best_Stage <- Host_Country_Matches %>% group_by(Year) %>% top_n(1,Date)

head(Host_Country_Best_Stage)

Host_Country_Best_Stage$Stage <- fct_recode(Host_Country_Best_Stage$Stage, "Final Round" ="Final", "Group Stage" = "Final round", "Group Stage" = "Second group stage", "Group Stage"="Group stage", "Quarter-final Round" = "Quarter-finals", "Round of 16" = "Round of 16", "Third-Fourth Round"= "Third place")


#We create a new column in that indicate whether the host country could make to the top 4 that year
WC_Events_Host_Standing<- WC_Events %>% 
  group_by(Year) %>%
  mutate(
    HostFinalStanding = if_else(
      HostCountry %in% head(Country, 4), 
      Standing, 
      "Out Top 4")) %>% 
  filter(HostCountry== Country | HostFinalStanding =="Out Top 4") %>% select(-Country, -Standing) %>% distinct()

View(WC_Events_Host_Standing)

Host_Country_Data <- WC_Events_Host_Standing %>% left_join(Host_Country_Best_Stage, by = "Year") 


Host_Country_Data <- Host_Country_Data %>% mutate(HostFinalStandingDetailed = case_when(HostFinalStanding== "Out Top 4" ~ (case_match(as.character(Stage),"Quarter-finals" ~ "Top 8", "Round of 16" ~ "Top 16", "Final round"~ "Top 4", .default = "Group Stage")), .default = HostFinalStanding))
                             

Host_Country_Data <- Host_Country_Data %>% mutate(HostFinalStandingNumber = case_match(HostFinalStandingDetailed, "Winner"~ 6,"Runners-Up" ~ 5, "Third" ~ 4, "Fourth" ~ 3,"Top 8" ~ 2, "Top 16"~1, .default=0))


ggplot(data = Host_Country_Data,mapping = aes( x = Year, y = HostFinalStandingNumber)) + geom_line(color = "red", size = 1, alpha = 0.5) + geom_text(aes(label = str_c(HostCountry,"\n", HostFinalStandingDetailed)), color = "black", vjust =0, size = 3) + theme(legend.position = "none") + ggtitle("Host Country Performance Over Time")  + scale_x_continuous(limits = c(1930, 2022), breaks = seq(1930,2022,4)) + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), legend.position = "none")


Host_Data_Summary <- fct_count(Host_Country_Data$HostFinalStandingDetailed) %>% rename(Standing = "f", Times = "n")

Host_Data_Summary$Standing <- fct_relevel (Host_Data_Summary$Standing,"Winner", "Runners-Up", "Third", "Fourth", "Top 8","Top 16", "Group Stage")


ggplot(data = Host_Data_Summary, aes(x = Standing, y = Times)) + 
  geom_bar(stat = "identity", aes(fill = Times), color = "black") + 
  scale_fill_gradient(low = "grey", high = "red") + 
  ggtitle("Host Country Performance Summary") +
  xlab("") +
  geom_text(aes(label = Times), position = position_stack(vjust = 0.8), fontface = "bold") +
  guides(fill = "none")



Summarised_HostPerformance <- WC_Matches %>%
  filter(HostTeam==TRUE) %>%
  group_by(Year) %>%
  summarise(
    Average_Goal_Scored = mean(HomeGoals),
    Average_Goal_Conceded = mean(AwayGoals))



Summarised_QatarPerformance <- filter(Summarised_HostPerformance, Year == 2022 )
Qatar_Average_Goal_Scored <- Summarised_QatarPerformance$Average_Goal_Scored
Qatar_Average_Goal_Conceded <- Summarised_QatarPerformance$Average_Goal_Conceded


# Chart with only Average Goal Scored
ggplot(data = Summarised_HostPerformance, aes(x = Year, y = Average_Goal_Scored)) + 
  geom_line(color = "red", size = 1, alpha = 0.5) +
  ggtitle("Host Country Average Goal Scored Per Match") +
  theme(axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 10)) +
  scale_x_continuous(limits = c(1930, 2022), breaks = seq(1930, 2022, 4)) +
  scale_y_continuous(breaks = seq(0, max(Summarised_HostPerformance$Average_Goal_Scored), 0.5)) +
  ylab("Average Goals") +
  xlab("Year") +
  geom_hline(aes(yintercept = Qatar_Average_Goal_Scored, linetype = "Average Goals Scored"), 
             color = "red", linetype = "dashed") +
  scale_linetype_manual(values=c("dashed"), guide = "none") +
  geom_label(aes(x = max(Year), y = Qatar_Average_Goal_Scored, label = "Qatar"), 
             color = "black", size = 5, vjust = 1, hjust = -0.1) +
  geom_text(aes(label = round(Average_Goal_Scored, 1)), nudge_y = 0.1, hjust = -0.2, size = 3.5) +
  theme(legend.position = "none")

# Chart with only Average Goal Conceded
ggplot(data = Summarised_HostPerformance, aes(x = Year, y = Average_Goal_Conceded)) + 
  geom_line(color = "blue", size = 1, alpha = 0.5) +
  ggtitle("Host Country Average Goal Conceded Per Match") +
  theme(axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 10)) +
  scale_x_continuous(limits = c(1930, 2022), breaks = seq(1930, 2022, 4)) +
  scale_y_continuous(breaks = seq(0, max(Summarised_HostPerformance$Average_Goal_Conceded), 0.5)) +
  ylab("Average Goals Conceded") +
  xlab("Year") +
  geom_hline(aes(yintercept = Qatar_Average_Goal_Conceded, linetype = "Average Goals Conceded"), 
             color = "blue", linetype = "dashed") +
  scale_linetype_manual(values=c("dashed"), guide = "none") +
  geom_label(aes(x = max(Year), y = Qatar_Average_Goal_Conceded, label = "Qatar"), 
             color = "black", size = 5, vjust = 0.1, hjust = -0.1) +
  geom_text(aes(label = round(Average_Goal_Conceded, 1)), nudge_y = -0.1, hjust = -0.2, size = 3.5) +
  theme(legend.position = "none")


 #2 Based on recent form and historical dominance which countries underperformed and over performed

#Historical Dominance:


WC_dominance_summary <- WC_Events %>%
  group_by(HostCountry, Standing) %>%
  count() %>%
  pivot_wider(names_from = Standing, values_from = n, values_fill = 0) %>%
  rename(Winner = "Winner", `Runners-Up` = "Runners-Up", Third = "Third", Fourth = "Fourth")


WC_dominance_summary <- WC_dominance_summary %>% mutate(Times_in_Top4 = rowSums(across(Winner:Fourth)))


WC_dominance_summary %>%
  pivot_longer(cols = -c(HostCountry,Times_in_Top4), names_to = "Place", values_to = "Sum") %>%
  mutate(Place = factor(Place, levels = c("Winner", "Runners-Up", "Third", "Fourth"))) %>%
  mutate(Country = fct_reorder(HostCountry, Sum, sum)) %>% 
  ggplot(aes(x = Country, y = Sum, fill = Place)) +
  geom_col(position = "stack", color = "black") +
  geom_text(aes(label = ifelse(Sum > 0, Sum, "")), position = position_stack(vjust = 0.5), size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0")) +
  labs(x = "", y = "Performances", fill = "") +
  ggtitle("Top Performers in World Cup") +
  theme_minimal() + 
  scale_y_continuous(limits = c(0, 12), breaks = seq(0,12,2))


WC_Matches %>% filter(Year == 2022) %>% select (HomeTeam)

International_Matches_Transformed <- 
  International_Matches %>% pivot_longer(cols = c(HomeTeam, AwayTeam), names_to = "Home_Away", values_to = "Country") %>%
  mutate(Goal_Scored = ifelse (Home_Away == "HomeTeam", HomeGoals, AwayGoals)) %>%
  filter( Country %in% (WC_Groups$Team))



#filter all the matches in world cup


International_Matches_Transformed_latest5 <- International_Matches_Transformed %>%
  arrange(Country, Date) %>%  # sort by Country and Date
  group_by(Country) %>%
  slice_tail(n = 5)  # keep only the last 5 matches for each Country



International_Matches_Transformed_latest5 <- International_Matches_Transformed_latest5 %>%
    mutate(Result = case_when(
    Home_Away == "HomeTeam" & HomeGoals > AwayGoals ~ "Win",
    Home_Away == "HomeTeam" & HomeGoals < AwayGoals ~ "Loss",
    Home_Away == "AwayTeam" & HomeGoals > AwayGoals ~ "Loss",
    Home_Away == "AwayTeam" & HomeGoals < AwayGoals ~ "Win",
    TRUE ~ "Draw"
  ))


International_Matches_Form <- International_Matches_Transformed_latest5 %>% select(Country, Result) %>% 
  mutate(Result_numeric = case_match(Result, "Win" ~ 3,  "Draw" ~ 1,  "Loss"  ~ 0)) %>%
  group_by(Country) %>%
  summarise(Total_point = sum(Result_numeric)) %>%
  mutate(Form_B4_WC = case_when(Total_point>10 ~ "A", Total_point <5 ~ "C", .default = "B"))%>%
  arrange(desc(Total_point))

International_Matches_Form

# create a ggplot object

ggplot(data = International_Matches_Form, aes(x = Total_point, y = reorder(Country,Total_point), color = Form_B4_WC)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("green", "blue", "red"), name = "Form_B4_WC") +
  labs(x = "Total Points", y = "Country", title = "Team Performance Summary Based on last 5 matches") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 15, by = 2))


WC2022_Performance <-WC_Groups %>% left_join(International_Matches_Form, by = c("Team" = "Country")) %>% 
  left_join(WC_dominance_summary, by = c("Team" = "HostCountry")) %>%
  replace_na(list(Times_in_Top4 = 0)) %>%
  arrange(Team) %>% 
  mutate(Final_Standing_numeric = case_match(Final_Standing,"Winner" ~ 1, "Runners-Up"~2,"Third Place" ~ 3, "Fourth Place" ~ 4, "Top 8" ~ 5, "Top 16" ~ 6, .default = 7))



WC2022_Performance$Final_Standing <- factor(WC2022_Performance$Final_Standing, levels = c("Group Stage", "Top 16", "Top 8", "Fourth Place", "Third Place", "Runners-Up", "Winner"))


ggplot(data = WC2022_Performance, aes(x = Final_Standing, y = Times_in_Top4, label = Team)) +
  geom_point(size =2 , color = "black") +
  ggrepel::geom_text_repel(aes(color = Form_B4_WC), size = 4, show.legend = TRUE, max.overlaps = Inf, nudge_x = 0.2, nudge_y = 0.2) +
  scale_color_manual(values = c("green", "blue", "red"), name = "Form_B4_WC") +
  labs(x = "Final Standing", y = "Times in Top 4", title = "Team Performance Summary Based on last 5 matches") +
  theme_bw() +
  scale_x_discrete(limits = c("Group Stage", "Top 16", "Top 8", "Fourth Place", "Third Place", "Runners-Up", "Winner"))



#Argentina 2022 road to glory compare to France 2018


WC_Matches_Transformed <- 
  WC_Matches %>% pivot_longer(cols = c(HomeTeam, AwayTeam), names_to = "Home_Away", values_to = "Country") %>%
  mutate(Goal_Scored = ifelse (Home_Away == "HomeTeam", HomeGoals, AwayGoals)) %>%
  filter((Country == "France" & Year == 2018) | (Country =="Argentina" & Year == 2022))

WC_Matches_Transformed <- WC_Matches_Transformed %>%
  mutate(Result = case_when(
    Home_Away == "HomeTeam" & HomeGoals > AwayGoals ~ "Win",
    Home_Away == "HomeTeam" & HomeGoals < AwayGoals ~ "Loss",
    Home_Away == "AwayTeam" & HomeGoals > AwayGoals ~ "Loss",
    Home_Away == "AwayTeam" & HomeGoals < AwayGoals ~ "Win",
    TRUE ~ "Draw")) %>%
  mutate(Result_numeric_streak = case_match(Result, "Win" ~ 1,  "Draw" ~ 0,  "Loss"  ~ -1)) %>%
  arrange(Country,Date) %>%
  group_by(Country) %>%
  mutate(match_index = row_number())


# plot Year vs. Result_Numeric with Country as the grouping variable
ggplot(WC_Matches_Transformed, aes(x = match_index, y = Result_numeric_streak, color = Country)) +
  geom_line(size = 1) + geom_point(size =2) +
  geom_text(aes(label = paste(Result, ifelse(is.na(WinCondition),"",WinCondition), sep = "\n")), 
            hjust = -0.1, size = 3, show.legend = FALSE, color = "black")  +
  labs(title = "Road to glory Argentina 2022 vs 2018 France",
       x = "Match Index",
       y = "Result",
       color = "Country") +
  scale_color_manual(values = c("#75AADB", "darkblue")) +
  facet_wrap(~Country, ncol = 2) +
  theme(axis.text.y = element_blank())

View(WC_Matches_Transformed)

Total_Goal_Scored <- WC_Matches_Transformed %>%
  group_by(Country) %>%
  summarize(Total_Goals_Scored = sum(ifelse(Home_Away == "HomeTeam", HomeGoals, AwayGoals)),
            Total_Goals_Conceded = sum(ifelse(Home_Away == "HomeTeam", AwayGoals, HomeGoals))) %>%
  ggplot(aes(x = Country, y = Total_Goals_Scored, fill = Country)) +
  geom_col(position = "dodge") +
  geom_text(aes(label=Total_Goals_Scored), position=position_dodge(width=0.9), vjust=-0.5) +
  facet_wrap(~ .) +
  labs(title = "Total Goals Scored",
       x = NULL, y = NULL) +
  guides(fill=FALSE) +
  scale_fill_manual(values = c("#75AADB", "darkblue")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

Total_Goal_Conceded <- WC_Matches_Transformed %>%
  group_by(Country) %>%
  summarize(Total_Goals_Scored = sum(ifelse(Home_Away == "HomeTeam", HomeGoals, AwayGoals)),
            Total_Goals_Conceded = sum(ifelse(Home_Away == "HomeTeam", AwayGoals, HomeGoals))) %>%
  ggplot(aes(x = Country, y = Total_Goals_Conceded, fill = Country)) +
  geom_col(position = "dodge") +
  geom_text(aes(label=Total_Goals_Conceded), position=position_dodge(width=0.9), vjust=-0.5) +
  facet_wrap(~ .) +
  labs(title = "Total Goals Conceded",
       x = NULL, y = NULL) +
  guides(fill=FALSE) +
  scale_fill_manual(values = c("#75AADB", "darkblue")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))


grid.arrange(Total_Goal_Scored + scale_y_continuous(limits = c(0, 20), expand = c(0, 0)), 
             Total_Goal_Conceded + scale_y_continuous(limits = c(0, 20), expand = c(0, 0)), 
             ncol = 2, widths = c(4, 4), top = "Argentina vs France World Cup Stats")

#rmarkdown::render("R_Script.R", "github_document")

