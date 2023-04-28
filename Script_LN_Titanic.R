#Dataset Titanic https://www.kaggle.com/c/titanic/data
#Dataset RMS Lusitania Complete Passanger Manifest

#loading packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

#Create URL for dataset
url_Titanic <- "https://raw.githubusercontent.com/IamMancini/rstudio-titanic/main/train.csv"
url_RMS_Lusitania <- "https://raw.githubusercontent.com/IamMancini/rstudio-titanic/main/LusitaniaManifest.csv"


#Data-Cleaning
url_Titanic <- "https://raw.githubusercontent.com/IamMancini/rstudio-titanic/main/train.csv"
url_RMS_Lusitania <- "https://raw.githubusercontent.com/IamMancini/rstudio-titanic/main/LusitaniaManifest.csv"



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

#-------------test


#Seperate the name in Titanic
titanic <- titanic %>%
  separate(Name, into = c("Family_name", "first_name"), sep = ", ") 

write.csv(titanic, file = "C:/Users/aless/DASB/rstudio-titanic/train.csv", row.names = FALSE)


#Write the data in the column "Sex" lower case
lusitania$Sex <- tolower(lusitania$Sex)

write.csv(lusitania, "C:/Users/aless/DASB/rstudio-titanic/LusitaniaManifest.csv", row.names = FALSE)

#Write the first letter in Family_name capital and the rest in lower Cases
lusitania$Family_name <- sapply(lusitania$Family_name, function(x) {
  paste(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))), sep="")
})

write.csv(lusitania, "C:/Users/aless/DASB/rstudio-titanic/LusitaniaManifest.csv", row.names = FALSE)


#Change abbreviation in the column "Embarked"
titanic$Embarked[titanic$Embarked == "C"] <- "Cherbourg"
titanic$Embarked[titanic$Embarked == "Q"] <- "Queenstown"
titanic$Embarked[titanic$Embarked == "S"] <- "Southampton"
write.csv(titanic, file = "C:/Users/aless/DASB/rstudio-titanic/train.csv", row.names = FALSE)


#Delete the title of a person from first_name in Titanic
titanic$first_name <- gsub("\\w+\\.", "", titanic$first_name)
write.csv(titanic, file = "C:/Users/aless/DASB/rstudio-titanic/train.csv", row.names = FALSE)

#Change PassengerId from 0 to 1 in Lusitania
lusitania$PassengerId <- lusitania$PassengerId - min(lusitania$PassengerId) + 1
write.csv(lusitania, "C:/Users/aless/DASB/rstudio-titanic/LusitaniaManifest.csv", row.names = FALSE)


#Change the name of the column from Department.Class to Ticket_class in Dataset Lusitania
colnames(lusitania)[colnames(lusitania) == "Department.Class"] <- "Ticket_class"

write.csv(lusitania, "C:/Users/aless/DASB/rstudio-titanic/LusitaniaManifest.csv", row.names = FALSE)


#Change the name of the column from Pclass	to Ticket_class in Dataset Titanic
colnames(titanic)[colnames(titanic) == "Pclass"] <- "Ticket_class"
write.csv(titanic, file = "C:/Users/aless/DASB/rstudio-titanic/train.csv", row.names = FALSE)

#In the Lusitania Dataset the data is going to be renamed, so that they match the ticket class from titanic. Saloon is going to be 1 / Second is going to be 2 / Thrid is going to be 3 / The rest of the Ticket classes are going to be 4
lusitania$Ticket_class <- gsub("Saloon", "1", lusitania$Ticket_class)
lusitania$Ticket_class <- gsub("Second", "2", lusitania$Ticket_class)
lusitania$Ticket_class <- gsub("Third", "3", lusitania$Ticket_class)
lusitania$Ticket_class <- gsub("Victualling|Engineering|Deck|Band", "4", lusitania$Ticket_class)
write.csv(lusitania, "C:/Users/aless/DASB/rstudio-titanic/LusitaniaManifest.csv", row.names = FALSE)


