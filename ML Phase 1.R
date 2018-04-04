# http://www.bom.gov.au/climate/dwo/index.shtml

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

# Remove data where Tomorrow rain information is missing
table(Melbourne$RainTomorrow,useNA = 'ifany')
Melbourne = Melbourne %>% drop_na(RainTomorrow)

# Showing 
str(Melbourne)
summarizeColumns(Melbourne) %>% knitr::kable( caption = 'Feature Summary before Data Preprocessing')

# ----------------------------------------------
# Temperature frop from 9AM to 3 PM for the same day 
temperature_drop= Melbourne[Melbourne$Temp9am < Melbourne$Temp3pm, ]
prop.table(table(temperature_drop$RainToday)) * 100
# Even if temperature drops by 3 PM, chances of having rain is just 22% 
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
