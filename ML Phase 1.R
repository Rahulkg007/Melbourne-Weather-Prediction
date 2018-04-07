# http://www.bom.gov.au/climate/dwo/index.shtml
# https://en.wikipedia.org/wiki/Points_of_the_compass
# https://bmtc.moodle.com.au/mod/book/view.php?id=5580&chapterid=4117


rm(list = ls())
graphics.off()

# 1. Packages  ----
library(knitr)
library(mlr)
library(tidyverse)
library(GGally)
library(cowplot)
require(corrplot)
library(reshape2)
theme_set(theme_minimal())
theme_weather <- theme_minimal() +
  theme(plot.subtitle = element_text(color = '#333333', face = "italic"),
        plot.caption = element_text(color = '#666666', face = "italic", size = 9))

# -------------------------------
# Data Preprocessing and cleaning
# -------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
weather <- read.csv('weatherAUS 2.csv')
weather$Date = as.Date(weather$Date,'%Y-%m-%d')

# Filter data for Melbourne
Melbourne = weather[weather$Location %in% c('Melbourne','MelbourneAirport'),]

# Add Month and Year-Month
Melbourne$Month = strftime(Melbourne$Date,"%b")
Melbourne$MonthYear = strftime(Melbourne$Date,"%b-%Y")

# Missing Year Data
missing_year = subset(Melbourne ,is.na(Melbourne$RainToday) | is.na(Melbourne$RainTomorrow))
missing_year$Year = strftime(missing_year$Date,"%Y")
ggplot(missing_year,aes(Year)) + 
  geom_bar(fill='#969696') +
  ggtitle('Melbourne Weather Missing Data by Year') +
  labs(subtitle = '2015 has most number of Missing Data',
       caption="Source - Commonwealth of Australia , Bureau of Meteorology") +
  theme_weather

# Missing Month Data 
missing_month = subset(Melbourne ,is.na(RainToday) | is.na(RainTomorrow)) 

missing_month$Month = factor(missing_month$Month,
                             levels = c('Jan','Feb','Mar','Apr','May','Jun',
                                        'Jul','Aug','Sep','Oct','Nov','Dec'), 
                             ordered = TRUE)

ggplot(missing_month,aes(Month)) + 
  geom_bar(fill='#969696') +
  ggtitle('Melbourne Weather Missing Data by Month') +
  labs(subtitle = 'Apr has highest missing values, Dec has least',
       caption="Source - Commonwealth of Australia , Bureau of Meteorology") +
  theme_weather
# Since the month data is distributed well, we can remove them from data set

# Missing data percentage
missing_percentage = count(missing_month)/count(Melbourne) * 100
missing_percentage

# Remove data where Today and Tomorrow rain information is missing
# 14.4% data is missing

# Remove Missing Data
Melbourne = Melbourne %>% 
  drop_na(RainToday) %>%
  drop_na(RainTomorrow)

# Showing 
str(Melbourne)
summarizeColumns(Melbourne) %>% knitr::kable( caption = 'Feature Summary before Data Preprocessing')


# Change cardinal direction to degrees
direction = read.csv('direction.csv')
Melbourne$WindGustDir = factor(Melbourne$WindGustDir, 
                               levels = direction$CardinalDirection, 
                               labels = direction$DegreeDirectionMean)

Melbourne$WindDir9am = factor(Melbourne$WindDir9am, 
                               levels = direction$CardinalDirection, 
                               labels = direction$DegreeDirectionMean)

Melbourne$WindDir3pm = factor(Melbourne$WindDir3pm, 
                               levels = direction$CardinalDirection, 
                               labels = direction$DegreeDirectionMean)


# ----------------------------------------------
# Temperature frop from 9AM to 3 PM for the same day 
temperature_drop= Melbourne[Melbourne$Temp9am < Melbourne$Temp3pm, ]
prop.table(table(temperature_drop$RainToday)) * 100
# Even if temperature drops by 3 PM, chances of having rain is just 22% 
temperature_drop= Melbourne[Melbourne$Temp9am > Melbourne$Temp3pm, ]
prop.table(table(temperature_drop$RainToday)) * 100
# ----------------------------------------------


# ----------------------------------------------
# Relationship between today and tomorrow's rain
prop.table(table(Melbourne$RainToday, Melbourne$RainTomorrow,dnn=c('Rain Today','Rain Tomorrow'))) * 100
# 9% times if it rains today then it rained tomorrow 
# 14% times it rains today and didn't rain tomorrow
# If it didn't rain today then chances are it won't rain tomorrow

# TODO we can also do chi square test of independence
# ----------------------------------------------


# --------------
# Visualizations
# --------------

# ------------------
# Monthly Average Rainfall from 2009-2016
total_yearmonth_rainfall = Melbourne %>%
  select(MonthYear,Rainfall) %>%
  group_by(MonthYear) %>%
  summarise(monthTotal = sum(Rainfall, na.rm = TRUE))

total_yearmonth_rainfall$month = substr(total_yearmonth_rainfall$MonthYear, 1, 3)

monthly_rainfall =  total_yearmonth_rainfall %>%
  group_by(month) %>%
  summarise(avgRainfall = mean(monthTotal))

monthly_rainfall$month = factor(monthly_rainfall$month,
                                levels = c('Jan','Feb','Mar','Apr',
                                           'May','Jun','Jul','Aug',
                                           'Sep','Oct','Nov','Dec'),
                                ordered = TRUE)

ggplot(monthly_rainfall,aes(month,avgRainfall)) +
  geom_bar(stat = 'identity',fill="#006699") +
  ggtitle('Monthly Average Rainfall, 2009-2016') +
  xlab('Months') +
  ylab('Rainfall(MM)') +
  labs(subtitle = 'Rainfall is increasing from Jan to Dec with little variation') +
  theme(plot.subtitle = element_text(color = '#333333',face = "italic"))

# Except Nov, the variation isn't too much between Consecutive years. The possibility of getting rain 
# in Feb is almost same as Mar or Apr, hence we connot conclude the rain by month. 
# Melbourne doesn't have a fix rainy season hence average rainfall is distributed

# ------------------
# Correlation Matrix
correlation_matrix = cor(Melbourne[,c(3:7,9,12:21)],use='na.or.complete')
corrplot(correlation_matrix, order = 'AOE', type = "upper")
# Very few factors has high correlation


# visualize continuous variables
continouous_cols = c('MinTemp','MaxTemp','Rainfall','Evaporation','Sunshine',
                     'WindGustSpeed','WindSpeed9am','WindSpeed3pm','Humidity9am',
                     'Humidity3pm','Pressure9am','Pressure3pm','Temp9am','Temp3pm')
continouous_variables = melt(Melbourne[,continouous_cols])

ggplot(continouous_variables) + 
  stat_bin(aes(value), color='#EEEEEE', size=.1, fill="#006699") + 
  facet_wrap(~variable,scales="free") +
  ggtitle('Melbourne Weather - Feature Distribution') +
  xlab('') +
  ylab('') +
  labs(subtitle = 'Temperature and Pressure is normally distributed',
       caption="Source - Commonwealth of Australia , Bureau of Meteorology") +
  theme_weather

# visualize categorical variables
head(Melbourne)

wind_direction = Melbourne 
wind_direction$WindGustDir = factor(wind_direction$WindGustDir, 
                       levels = rev(direction$CardinalDirection), 
                       ordered = TRUE)

wind_direction %>%
  filter(!is.na(WindGustDir)) %>%
  ggplot(aes(WindGustDir)) + 
  geom_bar(fill="#006699") + 
  coord_flip() +
  ggtitle('Melbourne Direction of the Strongest Wind Gust in 24 Hours') +
  ylab('Count') +
  xlab('16 compass points') +
  labs(subtitle = 'Direction is ordered from 0 degrees North in clockwise order',
       caption="Source - Commonwealth of Australia , Bureau of Meteorology") +
  theme_weather




continouous_cols = c('Cloud9am','Cloud3pm')
x= melt(Melbourne[,c('Cloud9am','Cloud3pm')])
x = melt(Melbourne, )
  

  melt(Melbourne[,c('Cloud9am','Cloud3pm')]) %>%
  ggplot(aes(value)) + 
  geom_bar() + 
  facet_wrap(~variable,scales="free") +
  coord_flip()




# ---------------------------------------------------------
# Relationship Between Rain and Minimum Maximum Temperature
# Convert wide to long
Temperature = Melbourne %>% 
  select(MinTemp,MaxTemp,RainToday) %>%
  drop_na(RainToday) %>%
  melt(id.vars = 'RainToday')

ggplot(Temperature,aes(variable,value)) + 
  geom_boxplot(fill=c('#9ecae1','#3182bd','#9ecae1','#3182bd')) + 
  facet_grid(~RainToday) +
  theme_gray() +
  ggtitle('Relationship Between Rain and Minimum Maximum Temperature') +
  xlab('Months') +
  ylab('Temperature(Celsius)') +
  labs(subtitle = 'Difference between High and low temperature is less on Rainy day') +
  theme(plot.subtitle = element_text(color = '#333333',face = "italic"))

# ------------------
# Monthly Average Rainfall from 2009-2016

head(Melbourne)

month_raintoday = Melbourne %>%
  select(Month,RainToday) %>%
  group_by(Month) %>%
  summarise(monthTotal = sum(Rainfall, na.rm = TRUE))

ggplot(month_raintoday,aes(x=Month)) + geom_bar(aes(fill=RainToday),position = "dodge")

total_yearmonth_rainfall$month = substr(total_yearmonth_rainfall$MonthYear, 1, 3)

monthly_rainfall =  total_yearmonth_rainfall %>%
  group_by(month) %>%
  summarise(avgRainfall = mean(monthTotal))

month_raintoday$Month = factor(month_raintoday$Month,
                                levels = c('Jan','Feb','Mar','Apr',
                                           'May','Jun','Jul','Aug',
                                           'Sep','Oct','Nov','Dec'),
                                ordered = TRUE)

ggplot(monthly_rainfall,aes(month,avgRainfall)) +
  geom_bar(stat = 'identity',fill="#006699") +
  ggtitle('Monthly Average Rainfall, 2009-2016') +
  xlab('Months') +
  ylab('Rainfall(MM)') +
  labs(subtitle = 'Rainfall is increasing from Jan to Dec with little variation') +
  theme(plot.subtitle = element_text(color = '#333333',face = "italic"))

# Except Nov, the variation isn't too much between Consecutive years. The possibility of getting rain 
# in Feb is almost same as Mar or Apr, hence we connot conclude the rain by month. 
# Melbourne doesn't have a fix rainy season hence average rainfall is distributed





#################
# Delta change
#################

x = Melbourne
x$deltaCloud = x$Cloud9am - x$Cloud3pm
ggplot(x,aes(deltaCloud, fill=RainToday)) + geom_bar(position = 'dodge') 





