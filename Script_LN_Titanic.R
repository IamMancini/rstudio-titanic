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





#rename "Family name" to "Family_name" in Lusitania
lusitania <- lusitania %>% rename(Family_name = `Family.name`)
write.csv(lusitania, "C:/Users/aless/DASB/rstudio-titanic/LusitaniaManifest.csv", row.names = FALSE)


#rename Personal name to first_name in Lusitania
lusitania <- rename(lusitania, first_name = Personal.name)
write.csv(lusitania, "C:/Users/aless/DASB/rstudio-titanic/LusitaniaManifest.csv", row.names = FALSE)


#remove Title from Lusitania
lusitania <- lusitania %>% 
  select(-Title)
write.csv(lusitania, "C:/Users/aless/DASB/rstudio-titanic/LusitaniaManifest.csv", row.names = FALSE)


#Change Column for "Sex" from Lusitania
lusitania <- lusitania %>%
  select(PassengerId, Family_name, first_name, Sex, everything())


#Change sequence
titanic <- titanic %>%
  select(PassengerId, Name, Sex, Survived, everything())

write.csv(titanic, "C:/Users/aless/DASB/rstudio-titanic/train.csv", row.names = FALSE)

#Change the data from Titanic in Column "survived" to [yes = survived] and [no = not survived]
titanic$Survived <- ifelse(titanic$Survived == 0, "no", "yes")
write.csv(titanic, file = "C:/Users/aless/DASB/rstudio-titanic/train.csv", row.names = FALSE)

#Change the name of the column "Fate" to "Survived" in Lusitania
lusitania <- lusitania %>%
  rename(Survived = Fate)
write.csv(lusitania, "C:/Users/aless/DASB/rstudio-titanic/LusitaniaManifest.csv", row.names = FALSE)


#Change the data from Lusitania in Column "Survival" to [yes =îf survived] and [no if not survived]
lusitania$Survived <- ifelse(lusitania$Survived == "Lost", "no", "yes")

show(lusitania)
#save changes from lusitania
write.csv(lusitania, "C:/Users/aless/DASB/rstudio-titanic/LusitaniaManifest.csv", row.names = FALSE)

#-------------

#Change Name in Titanic to Family name and first name


#Delete the column Name in Titanic because its unnecessary

