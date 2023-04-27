#Dataset Titanic https://www.kaggle.com/c/titanic/data
#Dataset RMS Lusitania Complete Passanger Manifest

#loading packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

#Create URL for dataset
#Alessio
url_Titanic <- "C:/Users/aless/DASB/rstudio-titanic/train.csv"
url_RMS_Lusitania <- "C:/Users/aless/DASB/rstudio-titanic/LusitaniaManifest.csv"

#Chris
url_Titanic <- 
url_RMS_Lusitania <- 
  
#Ramon
url_Titanic <- 
url_RMS_Lusitania <- 

#Driton
url_Titanic <- 
url_RMS_Lusitania <- 
  

#load Dataset
titanic <- read.csv(url_Titanic)
lusitania <- read.csv(url_RMS_Lusitania)

#Testtt

#Show first rows of dataset Titanic to check
head(titanic)

#Show first rows of dataset Lusitania to check
head(lusitania)

#summary of dataset Titanic
summary(titanic)

#summary of dataset Lusitania
summary(lusitania)

#Start Data-Cleaning:
#What is the sturcture of the dataset for both datasets

names(titanic)
names(lusitania)


#Change columne 0 from Lusitania to be the same as titanic "PersonalId"
#Changes have been made directly inside the csv

#Change Name in Titanic to Family name and first name
titanic <- titanic %>%
  mutate(`Family_name` = str_trim(str_split(Name, ",")[[1]][1]),
         `first_name` = str_trim(str_split(Name, ",")[[1]][2]))
write.csv(titanic, file = "C:/Users/aless/DASB/rstudio-titanic/train.csv", row.names = FALSE)

#Delete the column Name in Titanic because its unnecessary
titanic <- select(titanic, -Name)
write.csv(titanic, file = "C:/Users/aless/DASB/rstudio-titanic/train.csv", row.names = FALSE)


