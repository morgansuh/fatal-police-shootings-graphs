library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)



fatal_police_shootings_data <- read_csv("PAM2070/fatal-police-shootings-data.csv")

fatal_police_shootings_data$year <- format(fatal_police_shootings_data$date, format="%Y")

#Chart 1 - Shows total shootings by race each year

#Filter out data that has NA in race & 2021 data and find total shootings based on race for each year
race_police_shootings_data <- fatal_police_shootings_data %>% 
  filter(!is.na(race)&year != "2021"&year != "2020"&race != "O") %>% 
  group_by(race,year) %>% 
  summarise(n=n()) 

#Data frame of total populations of each race from 2015-2020 (all values from U.S. Census Bureau ACS)

race_police_shootings_data$year_race_pop <- 
          c("17083925", "17347260","18001344","18195260","18430564",
          "39600891",	"39719644","40132047" ,"40310242","40599428",
          "59933510","60764560" ,"62152594","62917648","63616505",
          "2069664","2125860","2145239","2180266","2236807",
          "197560927","197515618","197308581","197064732","196832007")

#Column of shootings per 100k population by race
race_police_shootings_data <- race_police_shootings_data %>%
  mutate(per_100k_pop=(n*100000)/as.numeric(year_race_pop)) 


#Line graph showing the number of people shot based on race
gg<- ggplot(race_police_shootings_data,aes(x= year, y = per_100k_pop, group = race, colour = race)) + 
  labs(x = "Year", y = "Shootings per 100k Population of Race") +
  ggtitle("Total Shooting Victims by Race from 2015-2019 per 100k") +
  geom_line()
gg + scale_color_manual(name="Race", 
                        labels = c("Asian", 
                                   "Black", 
                                   "Hispanic", 
                                   "Native American",
                                   "White"), 
                        values = c("blue", 
                                   "red", 
                                   "green", 
                                   "orange",
                                   "purple"))


#Graph 2- Stacked area chart showing the percentages of unarmed/armed shootings

#Filter out data that has NA in armed column
armed_police_shootings_data <- fatal_police_shootings_data %>% 
  filter(year != "2021") %>% 
  filter(!is.na(armed)) 


#Adding column to categorize everything into 3 categories: armed with gun, armed with other type of object, unarmed or unknown whether armed
armed_police_shootings_data$category <- ifelse(armed_police_shootings_data$armed=="gun"|armed_police_shootings_data$armed=="gun and vehicle"|
                                                 armed_police_shootings_data$armed=="gun and knife"|armed_police_shootings_data$armed=="gun and car"|
                                                 armed_police_shootings_data$armed=="gun and machete"|armed_police_shootings_data$armed=="gun and sword"|
                                                 armed_police_shootings_data$armed=="guns and explosives"|armed_police_shootings_data$armed=="hatchet and gun"|
                                                 armed_police_shootings_data$armed=="machete and gun"|armed_police_shootings_data$armed=="vehicle and gun", "gun",
                                               ifelse(armed_police_shootings_data$armed=="unarmed"|armed_police_shootings_data$armed=="toy weapon", "unarmed",
                                                      ifelse(armed_police_shootings_data$armed=="undetermined"|armed_police_shootings_data$armed=="unknown weapon","unknown","other object")))

#Table of total shootings by year and category
armed_police_shootings_data <- armed_police_shootings_data%>% 
  group_by(category, year) %>% 
  summarise(n=n())


#Create data frame of total number of shootings each year
yearly_pop <- data.frame(year = integer(),total= integer())
for(i in 1: nrow(armed_police_shootings_data)){
  year = as.numeric(armed_police_shootings_data$year[i])
  #if year is already in df, add to total of that year
  if(year %in% yearly_pop$year){
    #add to total
    col_number <- which(yearly_pop$year == year,arr.ind=TRUE)
    yearly_pop$total[col_number] = yearly_pop$total[col_number] + armed_police_shootings_data$n[i]
  }
  #else add new year to data frame
  else{
    #adding new year
    diff_year <- data.frame(year,armed_police_shootings_data$n[i])      
    names(diff_year) <- c("year", "total")  
    yearly_pop<- rbind(yearly_pop,diff_year)
  }
}

#Function to return total number of shootings in given year
total_shoot_year<- function (year) {
  index <- which(yearly_pop$year == year,arr.ind=TRUE)
  return (yearly_pop$total[index])
}

#Add column of the percentage (out of total shootings that year what percent of people had gun/other obj/ unknown)
armed_police_shootings_data <- armed_police_shootings_data %>%
  #Use function total_shoot_year to get total shooting that year to calculate percentage
  mutate(percentage = (n/total_shoot_year(year))*100)

#Stacked area graph that shows the percentage of shooting victims armed by each category of weapon for each year
armed_police_shootings_data$category <- factor(armed_police_shootings_data$category , levels=c("gun", "other object", "unknown", "unarmed") )
ggplot(armed_police_shootings_data, aes(x=year, y=percentage, group = category, fill = category)) + 
  labs(x = "Year", y = "Percentage", fill = "Category of Weapon") +
  ggtitle("Relative Share of Shooting Victims Armed, by Category of Weapon")+
  geom_area()

