install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")
install.packages("ggplot2")
install.packages("scales")
install.packages("readr")
library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)
library(scales)
library(readr)
X202201_divvy_tripdata <- read_csv("Desktop/bike_data/202201-divvy-tripdata.csv")
X202202_divvy_tripdata <- read_csv("Desktop/bike_data/202202-divvy-tripdata.csv")
X202203_divvy_tripdata <- read_csv("Desktop/bike_data/202203-divvy-tripdata.csv")
X202204_divvy_tripdata <- read_csv("Desktop/bike_data/202204-divvy-tripdata.csv")
X202205_divvy_tripdata <- read_csv("Desktop/bike_data/202205-divvy-tripdata.csv")
X202206_divvy_tripdata <- read_csv("Desktop/bike_data/202206-divvy-tripdata.csv")
X202207_divvy_tripdata <- read_csv("Desktop/bike_data/202207-divvy-tripdata.csv")
X202208_divvy_tripdata <- read_csv("Desktop/bike_data/202208-divvy-tripdata.csv")
X202209_divvy_publictripdata <- read_csv("Desktop/bike_data/202209-divvy-publictripdata.csv")
X202210_divvy_tripdata <- read_csv("Desktop/bike_data/202210-divvy-tripdata.csv")
X202211_divvy_tripdata <- read_csv("Desktop/bike_data/202211-divvy-tripdata.csv")
X202212_divvy_tripdata <- read_csv("Desktop/bike_data/202212-divvy-tripdata.csv")

colnames(X202201_divvy_tripdata)
colnames(X202202_divvy_tripdata)
colnames(X202203_divvy_tripdata)
colnames(X202204_divvy_tripdata)
colnames(X202205_divvy_tripdata)
str(X202201_divvy_tripdata)
str(X202202_divvy_tripdata)
str(X202203_divvy_tripdata)
str(X202204_divvy_tripdata)
str(X202205_divvy_tripdata)
str(X202206_divvy_tripdata)
str(X202207_divvy_tripdata)
str(X202208_divvy_tripdata)
str(X202209_divvy_publictripdata)
str(X202210_divvy_tripdata)
str(X202211_divvy_tripdata)
str(X202212_divvy_tripdata)
bike_rides <- rbind (X202201_divvy_tripdata,X202202_divvy_tripdata,X202203_divvy_tripdata,X202204_divvy_tripdata,X202205_divvy_tripdata,X202206_divvy_tripdata,X202207_divvy_tripdata,X202208_divvy_tripdata,X202209_divvy_publictripdata,X202210_divvy_tripdata,X202211_divvy_tripdata,X202212_divvy_tripdata)
head(bike_rides)
str(bike_rides)
summary(bike_rides)
nrow(bike_rides)
colnames(bike_rides)
install.packages("dplyr")
library(dplyr)

bike_rides <- bike_rides %>%
  rename("user"="member_casual")

bike_rides$date <- as.Date(bike_rides$started_at)
bike_rides$month <- format(as.Date(bike_rides$date),"%m")
months_text <- month.name
print(months_text)

month_name= date.toString("MMMM")

  
bike_rides$day <- format(as.Date(bike_rides$date),"%d")
bike_rides$year <- format(as.Date(bike_rides$date), "%y")
bike_rides$hour <- format(bike_rides$started_at, format="%H")
bike_rides$weekday <-format(as.Date(bike_rides$date), "%A")

bike_rides$ride_length <- difftime(bike_rides$ended_at,bike_rides$started_at)
is.factor(bike_rides$ride_length)
bike_rides$ride_length <- as.numeric(as.character(bike_rides$ride_length))
is.numeric(bike_rides$ride_length)

summary(bike_rides)
str(bike_rides)
head(bike_rides)


bike_rides <- na.omit(bike_rides)
bike_rides <- distinct(bike_rides)
bike_rides <- bike_rides[!bike_rides$ride_length <=0,]
bike_rides <- bike_rides %>%
  select(-c(start_lat,start_lng,end_lat,end_lng))

bike_rides <- bike_rides %>%
  select(-c(start_station_id,end_station_id))

summary(bike_rides)
View(bike_rides)


bike_rides %>%
  group_by(user) %>%
  summarize(average_ride_length=mean(ride_length),median_ride_length=median(ride_length),
            min_ride_length=min(ride_length),max_ride_length=max(ride_length))

bike_rides %>%
  group_by(user,weekday) %>%
  summarize(average_ride_length=mean(ride_length),median_ride_length=median(ride_length))

bike_rides %>%
  group_by(user,month) %>%
  summarize(number_of_rides=n()) %>%
  arrange(user, month)

bike_rides <- bike_rides %>%
  mutate(month=factor(month,level=c("jun","jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May")))
  


print 
bike_rides %>% 
  group_by(user, weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(user, weekday)

bike_rides %>% 
  group_by(user, weekday) %>% 
  summarise(average_duration = mean(ride_length)) %>% 
  arrange(user, weekday)

bike_rides %>% 
  group_by(user, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(user, month)

bike_rides%>% 
  group_by(user)%>% summarise(n=n())%>%
  mutate(percent = n*100/sum(n))
ggplot(data = bike_rides,mapping= aes(x= user)) +geom_bar() + labs(title="user")+
scale_y_continuous(labels=comma)



  bike_rides %>% 
    group_by(user, weekday) %>% 
    summarise(number_of_rides=n(),
              average_duration = mean(ride_length)) %>% 
    arrange(user, weekday)  %>% 
    ggplot(aes(x = weekday, y = average_duration, fill = user)) +
    geom_col(position = "dodge")+labs(title="Average Duration by User and Weekday",subtitle="Cyclistic 2022 Data",x="WeekDay",y="Average Duration")
   scale_y_continuous(labels=comma)
   
  bike_rides%>% 
    group_by(rideable_type) %>% 
    summarise(n=n())%>% 
    mutate(percent = n*100/sum(n))
    ggplot(data = bike_rides,mapping= aes(x= rideable_type,fill=rideable_type)) +
    geom_bar() + labs(title="Bike Types used")
    scale_fill_brewer(palette="set2")
  
  
  bike_rides$weekday<- factor(bike_rides$weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  weektable<- bike_rides%>% group_by(weekday)%>% summarise(n=n())%>% mutate(percent = n*100/sum(n))
  ggplot(data = bike_rides,mapping= aes(x= weekday)) +geom_bar() + labs(title="Usage per day")+
scale_y_continuous(labels=comma)
  
  ggplot(data = bike_rides,mapping= aes(x= weekday, fill = user))+
    geom_bar() +facet_wrap(~user)+theme(axis.text.x = element_text(angle = 60, hjust =1))
    scale_y_continuous(labels = function(x) format(x,nsmall=2))  
  
  member_type<-bike_rides%>% 
    group_by(user,rideable_type) %>% 
    summarise(n=n())%>%  
    mutate(percent = n*100/sum(n))
    ggplot(data = as.data.frame(member_type),mapping= aes(x= user, y=n, fill =rideable_type)) +
      geom_bar(stat = 'identity') + labs(title="Choice of Bike by Riders") 
    scale_y_continuous(labels=comma)
    
    write.csv(bike_rides, file = "bike_rides.csv")
    
    bike_tableau <- bike_rides
    fwrite(bike_tableau,"bike_rides.csv")
    
  write.csv(bike_rides, file ="bike_rides_as_csv")
  
library(data.table) 
write(bike_rides, file="bike_rides.csv")

