# http://www.bom.gov.au/climate/dwo/index.shtml

rm(list = ls())
graphics.off()
# 1. Packages  ----
library(knitr)
library(mlr)
library(tidyverse)
library(GGally)
library(cowplot)
theme_set(theme_minimal())

# -------------------------------
# Data Preprocessing and cleaning
# -------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
weather <- read.csv('weatherAUS 2.csv')

# Initial Filters
Melbourne = weather[weather$Location %in% c('Melbourne','MelbourneAirport'),]

table(Melbourne$RainTomorrow,useNA = 'ifany')
Melbourne = Melbourne %>% drop_na(RainTomorrow)

str(Melbourne)
summarizeColumns(Melbourne) %>% knitr::kable( caption = 'Feature Summary before Data Preprocessing')

# --------------
# Visualizations
# --------------

ggplot(Melbourne,aes(x=0,y=RISK_MM)) + geom_boxplot()
ggplot(Melbourne,aes(RISK_MM)) + geom_histogram()

x = Melbourne[Melbourne$Temp9am > Melbourne$Temp3pm, ]
prop.table(table(x$RainToday))

y = Melbourne[Melbourne$Temp9am < Melbourne$Temp3pm, ]
prop.table(table(y$RainToday))

prop.table(table(Melbourne$RainTomorrow)) * 100
