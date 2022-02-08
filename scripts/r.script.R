#### Preamble ####
# Purpose: How to reproduce the paper Covid 19 cases in toronto
# Author: Swarnadeep Chattopadhyay
# Data: 3 January 2021
# Contact: swarnadeep.chattopadhyay@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!
# - Change these to yours
# Any other information needed?

### Workplace Setup ###
library(tidyverse)
library(palmerpenguins)

library(opendatatoronto)
library(dplyr)

# get package
package <- show_package("64b54586-6180-4485-83eb-81e8fae3b8fe")
package

# get all resources for this package
resources <- list_package_resources("64b54586-6180-4485-83eb-81e8fae3b8fe")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
#covid_data <- filter(datastore_resources, row_number()==1) %>% get_resource()
#covid_data
covid_data <- read.csv("C:/Users/deeps/Downloads/COVID19 cases.csv")

#In order to reproduce this project, download the csv file from open data portal and name the file as 'COVID19 cases.csv'
#and change the name deeps to whatever is your name in laptop

### This next part consists of all the codes required to produce the Visuals

#This will create a table with all the information found in the data set

library(tidyverse)
library(ggplot2)
library(ggpubr)

#removing the spaces in column headings
names(covid_data) <- str_replace_all(names(covid_data), c(" " = "." , "," = "" ))
dim(covid_data) #Matrix: Number of rows and columns in our data set
names(covid_data) #Column names in the data set 
sapply(covid_data, function(x) length(unique(x))) #Unique rows for each column
head(covid_data[c(3,4,5,7)], 5) #selected columns from the data set

#This is to create the Top 10 Toronto Neighbourhoods case count
nbr_ct <- data.frame(sort(table(covid_data$Neighbourhood.Name), decreasing = TRUE))
knitr::kable(head(nbr_ct, 10), "pipe", col.name =c("Neighbourhood Name", "Number of Cases"), align = c("l", "c"))

#Create a copy of original data to add columns for Year and Year & Month
c19_cpy <- covid_data
c19_cpy$Yr <- format(as.Date(covid_data$Reported.Date), "%Y")
c19_cpy$Yr_Mth <- format(as.Date(covid_data$Reported.Date), "%Y-%m")

#This will show a visual of the Case Count by year
ggplot(c19_cpy, aes(Yr, ..count..))+
  geom_bar(aes(fill = Yr), alpha = 0.5)+
  geom_text(aes(label = after_stat(count)), vjust = 0, stat = "count")+
  labs(x = "Year", y = "Number of Cases", title = "Fig1: Annual Case Count")

#This will show Case Count by Months
Yr_20 <- c19_cpy %>% 
  filter(Yr == "2020") %>% 
  ggplot(aes(Yr_Mth, ..count..))+
  geom_bar(fill = "red", width = 0.5, alpha = 0.5)+
  scale_x_discrete(labels = c("2020-01" = "Jan", "2020-02" = "Feb","2020-03" = "Mar",
                              "2020-04" ="Apr", "2020-05" = "May","2020-06" = "Jun",
                              "2020-07" = "Jul","2020-08" = "Aug","2020-09" = "Sep",
                              "2020-10" = "Oct","2020-11" = "Nov","2020-12" = "Dec"))  
labs(x = "Months", y = "Number of Cases", title = "2020 Monthly Numbers")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

Yr_21 <- c19_cpy %>% 
  filter(Yr == "2021") %>% 
  ggplot(aes(Yr_Mth, ..count..))+
  geom_bar(fill = "blue", width = 0.5, alpha = 0.5)+
  scale_x_discrete(labels = c("2021-01" = "Jan", "2021-02" = "Feb","2021-03" = "Mar",
                              "2021-04" = "Apr","2021-05" = "May","2021-06" = "Jun",
                              "2021-07" = "Jul","2021-08" = "Aug","2021-09" = "Sep",
                              "2021-10" = "Oct","2021-11" = "Nov","2021-12" = "Dec"))
labs(x = "Months", y = "Number of Cases", title = "2021 Monthly Numbers")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

Yr_22 <- c19_cpy %>% 
  filter(Yr == "2022") %>% 
  ggplot(aes(Yr_Mth, ..count..))+
  geom_bar(fill = "green", width = 0.5, alpha = 0.5)+
  labs(x = "Months", y = "Number of Cases", title = "2022 Monthly Numbers")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

combined_plots <- ggarrange(Yr_20, Yr_21,labels = c("2020", "2021"), ncol = 2)
annotate_figure(combined_plots, top = text_grob("Fig2: Monthly numbers by Year", color = "brown", face = "bold", size = 10))

#This is for the Total case count by age groups
ggplot(covid_data, aes(Age.Group, ..count..))+
  geom_bar(aes(fill = Age.Group), alpha = 0.5)+
  labs(x = "Age Groups", y = "Number of Cases", title = "Fig3: ICU Admissions by Age groups")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#And finally, leading factors for covid infections
ggplot(covid_data, aes(Source.of.Infection, ..count..))+
  geom_bar(aes(fill = Source.of.Infection), alpha = 0.5)+
  labs(x = "Sources", y = "Number of Cases", title = "Fig4: Factors leading to cases")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  facet_wrap(~Classification)

#This allows you to create all the data requried to reproduce this exact paper