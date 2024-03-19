getwd()

installed.packages("tidyverse") # Data cleaning
installed.packages("lubridate") # Date manipulation
installed.packages("data.table") # Data exporting

library(tidyverse)
library(lubridate)
library(data.table)

## Twelve-month data files were merged to create a single data frame and then the monthly data were dropped from the environment to tidy up the environment.
data_jan23<-read.csv("Jan2023-divvy-tripdata.csv",header=TRUE)
data_feb23<-read.csv("Feb2023-divvy-tripdata.csv",header=TRUE)
data_mar23<-read.csv("Mar2023-divvy-tripdata.csv",header=TRUE)
data_apr23<-read.csv("Apr2023-divvy-tripdata.csv",header=TRUE)
data_may23<-read.csv("May2023-divvy-tripdata.csv",header=TRUE)
data_june23<-read.csv("Jun2023-divvy-tripdata.csv",header=TRUE)
data_jul23<-read.csv("Jul2023-divvy-tripdata.csv",header=TRUE)
data_aug23<-read.csv("Aug2023-divvy-tripdata.csv",header=TRUE)
data_sept23<-read.csv("Sep2023-divvy-tripdata.csv",header=TRUE)
data_oct23<-read.csv("Oct2023-divvy-tripdata.csv",header=TRUE)
data_nov23<-read.csv("Nov2023-divvy-tripdata.csv",header=TRUE)
data_dec23<-read.csv("Dec2023-divvy-tripdata.csv",header=TRUE)

## Merge all files into single
annual_data <-rbind(data_jan23,data_feb23,data_mar23,data_apr23,data_may23,data_june23,data_jul23,data_aug23,data_sept23,data_oct23,data_nov23,data_dec23)

## Create a copy of merged file
annual_data2 <- annual_data

## Summary of data
head(annual_data2)
glimpse(annual_data2)
colnames(annual_data2)

## The merged and copied data frame was checked for duplicates and null values.
duplicate_rows<- annual_data2 %>%
    filter(duplicated(annual_data2))
print(duplicate_rows)

annual_data2<-distinct(annual_data2)

 null_rows<-annual_data2[apply(is.na(annual_data2),1,any),]
 print(null_rows)
 
 cleaned_annual_data<- annual_data2 %>%
     rename(member_type=member_casual,bike_type=rideable_type )
 colnames(cleaned_annual_data)
 head(cleaned_annual_data)
 
 ## Ride length in seconds and minutes were created as new variable.
 cleaned_annual_data$ride_length_seconds<- ymd_hms(cleaned_annual_data$ended_at)- ymd_hms(cleaned_annual_data$started_at)
 cleaned_annual_data$ride_length_minutes<-as.numeric(cleaned_annual_data$ride_length_seconds, units="mins")
 
 cleaned_annual_data$start_year<-format(ymd_hms(cleaned_annual_data$started_at), format="%y-%m-%d")
 cleaned_annual_data$start_month<-lubridate::month(cleaned_annual_data$start_year, label = TRUE, abbr = FALSE)
 
 cleaned_annual_data$end_year<-format(ymd_hms(cleaned_annual_data$ended_at), format="%y-%m-%d")
 cleaned_annual_data$end_month<-lubridate::month(cleaned_annual_data$end_year, label=TRUE, abbr=FALSE)
 
 cleaned_annual_data$start_day_of_week<-lubridate::wday(cleaned_annual_data$start_year, label=TRUE, abbr=FALSE)
 
 cleaned_annual_data<-cleaned_annual_data [,!(names(cleaned_annual_data) %in% c("ride_id","start_station_id","end_station_id","start_lat,start_lng","end_lat","end_lng"))]
 
 ## Write cleaned annual data into CSV
 write.csv(cleaned_annual_data, file="~/cleaned_annual_data_23.csv")
 View(cleaned_annual_data)
 
 ## Annual Rides
 nrow(cleaned_annual_data)
 print(nrow)
 
 ## Count ride by members
 cleaned_annual_data %>% count(member_type)
 
 ## Count Number of bikes per month
 cleaned_annual_data %>% 
   group_by(start_month)%>%
   count(rideable_type)
 
 ## Total rides by bike type
 cleaned_annual_data %>% 
   group_by(bike_type)%>%
   count(bike_type)
 
 ## Number of rides by day of week
 cleaned_annual_data%>%
   group_by(member_type)%>%
   count(start_day_of_week)
 
 ## Average trip duration by member
 cleaned_annual_data%>% 
   group_by(member_type)%>%
   drop_na() %>%
   summarise(mean_ride_length=mean(ride_length_minutes))
 
 ## Load ggplot
 install.packages("tidyverse")
 installed.packages("ggplot")
 library(ggplot2)

 
 ## As this study contains more than 5 million observation, the plots give scientific notations. To overcome this scenario follow this.
 options(scipen=999)
 
 cleaned_annual_data%>%
   group_by(member_type)%>%
   ggplot(aes(x=rideable_type, fill=member_type, colors=member_type))+
   geom_bar()+
   theme_bw()+
   labs(title = "Bike Type vs. Member_type",x="Bicycle_type", y="Number of trips")
 
 ## Plot ride by month
 cleaned_annual_data%>% 
   group_by(member_type)%>%
   ggplot(aes(x=start_month, fill=member_type, colors=member_type))+
   geom_bar()+
   theme_bw()+
   theme(axis.text.x=element_text(angle=25))+
   labs(title = "Number of Trips by Month",x="Start_month", y="Number of trips")
 
 ## Plot number of rides by day
 cleaned_annual_data%>%
   group_by(member_type)%>%
   ggplot(aes(x=start_day_of_week, fill=member_type,color=member_type))+
   geom_bar()
   
## Polpular start station for annual = member
 cleaned_annual_data%>%
   group_by(member_type,start_station_name)%>%
   summarise(number_of_ride=n())%>%
   filter(start_station_name!="", member_type=="member")%>%
   arrange(-number_of_ride)%>%
   head(5)%>%
   select(-member_type)
 
 ## Polpular start station for annual = casual
 cleaned_annual_data%>%
   group_by(member_type,start_station_name)%>%
   summarise(number_of_ride=n())%>%
   filter(start_station_name!="", member_type=="casual")%>%
   arrange(-number_of_ride)%>%
   head(5)%>%
   select(-member_type)
 


 
 
 
