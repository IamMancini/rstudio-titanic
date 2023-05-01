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

#------------------------------------------------------------------------------------------------------

#Hypothesis 1 - Survival rate in relation to gender and class:


#1. create a subset for titanic and lusitania dataset and only inlclude the variables of interest

titanic_subset <- titanic[, c("Sex", "Survived", "Ticket_class")]
lusitania_subset <- lusitania[, c("Sex", "Survived", "Ticket_class")]



#2. Calculate survival rates for each combination of gender and passenger class


#Summary statistics for titanic
titanic_summary <- titanic %>%
  group_by(Ticket_class, Sex, Survived) %>%
  summarize(n = n()) %>%
  mutate(pct_survived = n / sum(n) * 100)

#Summary statistics for Lusitania
lusitania_summary <- lusitania %>%
  group_by(Ticket_class, Sex, Survived) %>%
  summarize(n = n()) %>%
  mutate(pct_survived = n / sum(n) * 100)



# Convert Ticket_class to a character variable in titanic_summary
titanic_summary$Ticket_class <- as.character(titanic_summary$Ticket_class)

# Combine titanic_summary and lusitania_summary
summary_combined <- bind_rows(titanic_summary, lusitania_summary, .id = "Ship")

# Convert Ticket_class back to an integer variable
summary_combined$Ticket_class <- as.integer(summary_combined$Ticket_class)



# Combine the datasets and remove the "stowaway"
titanic$Ticket_class <- as.character(titanic$Ticket_class)
lusitania$Ticket_class <- as.character(lusitania$Ticket_class)

combined <- bind_rows(titanic %>% mutate(dataset = "Titanic"), 
                      lusitania %>% mutate(dataset = "Lusitania"))

combined_summary <- combined %>%
  filter(!Ticket_class %in% "Stowaway") %>%
  group_by(dataset, Sex, Ticket_class, Survived) %>%
  summarize(n = n()) %>%
  mutate(pct_survived = n / sum(n) * 100)

#3. Visualize survival rates with bar-chart
ggplot(combined_summary, aes(x = Sex, y = n, fill = Survived)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.9) +
  facet_grid(rows = vars(Ticket_class), cols = vars(dataset)) +
  scale_fill_manual(values = c("#d55e00", "#0072b2"), name = "Survived") +
  geom_text(aes(label = paste0(n, " (", round(pct_survived), "%)")),
            position = position_dodge(width = 0.9), vjust = -1.0) +
  labs(title = "Survival rate of passengers on Titanic and Lusitania",
       x = "Sex", y = "Number of passengers") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "bottom")

#Bar-chart only with number of passengers 
ggplot(combined_summary, aes(x = Sex, y = n, fill = Survived)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.9) +
  facet_grid(rows = vars(Ticket_class), cols = vars(dataset)) +
  scale_fill_manual(values = c("#d55e00", "#0072b2"), name = "Survived") +
  geom_text(aes(label = n),
            position = position_dodge(width = 0.9), vjust = -1.0) +
  ylim(0, 600) +
  labs(title = "Survival rate of passengers on Titanic and Lusitania",
       x = "Sex", y = "Number of passengers") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "bottom")

#Bar-chart only in percentage // Problem -> y-Axis doesnt match that the data is shown in percentage and some labels arent visible
ggplot(combined_summary, aes(x = Sex, y = n, fill = Survived)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.9) +
  facet_grid(rows = vars(Ticket_class), cols = vars(dataset)) +
  scale_fill_manual(values = c("#d55e00", "#0072b2"), name = "Survived") +
  geom_text(aes(label = paste0(round(pct_survived), "%")),
            position = position_dodge(width = 0.9), vjust = -1.0) +
  ylim(0, 600) +
  labs(title = "Survival rate of passengers on Titanic and Lusitania",
       x = "Sex", y = "Percentage of passengers") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "bottom")




#1. Create a new data frame with the total number of passengers for each ship
ship_totals <- rbind(
  titanic %>% summarise(dataset = "Titanic", total_passengers = n()),
  lusitania %>% summarise(dataset = "Lusitania", total_passengers = n())
)

# Create a bar chart of total passengers by ship
ggplot(ship_totals, aes(x = dataset, y = total_passengers, fill = dataset)) +
  geom_col() +
  labs(title = "Total passengers by ship",
       x = "Ship", y = "Total passengers") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values = c("#0072b2", "#d55e00")) +
  geom_text(aes(label = total_passengers), position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 3.5)




#2a. Create a grouped bar chart to compare the number of survivors and non-survivors by gender and dataset

grouped_summary <- titanic %>%
  group_by(Sex, Survived) %>%
  summarize(count = n()) %>%
  mutate(dataset = "Titanic") %>%
  bind_rows(lusitania %>%
              group_by(Sex, Survived) %>%
              summarize(count = n()) %>%
              mutate(dataset = "Lusitania"))

ggplot(grouped_summary, aes(x = dataset, y = count, fill = Survived)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  facet_grid(rows = vars(Sex)) +
  scale_fill_manual(values = c("#d55e00", "#0072b2"), name = "Survived",
                    labels = c("No", "Yes")) +
  labs(title = "Survival of passengers on Titanic and Lusitania by gender",
       x = "Dataset", y = "Number of passengers") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "bottom")

#2b. Pie-chart for the same plot



#3. Differences between the genders on both ships and how it effected their survival rate

# Calculate the survival rate by gender for the Titanic dataset
titanic_summary <- titanic %>%
  group_by(Sex, Survived) %>%
  summarise(n = n()) %>%
  mutate(pct_survived = n/sum(n) * 100)

# Calculate the survival rate by gender for the Lusitania dataset
lusitania_summary <- lusitania %>%
  group_by(Sex, Survived) %>%
  summarise(n = n()) %>%
  mutate(pct_survived = n/sum(n) * 100)

# Combine the summaries into one dataset
combined_summary <- bind_rows(
  mutate(titanic_summary, dataset = "Titanic"),
  mutate(lusitania_summary, dataset = "Lusitania")
)

ggplot(combined_summary, aes(x = Sex, y = n, fill = Survived)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_grid(rows = vars(dataset)) +
  scale_fill_manual(values = c("#d55e00", "#0072b2"), name = "Survived") +
  geom_text(aes(label = paste0(round(pct_survived), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Survival rate of passengers on Titanic and Lusitania by gender",
       x = "Sex", y = "Number of passengers") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "bottom")





#Differences between the passenger class on both ships and how it effected their survival rate




#------------------------------------------------------------------------------------------------------

#Hypothesis 2 - :







#------------------------------------------------------------------------------------------------------

#Hypothesis 3 - :









#------------------------------------------------------------------------------------------------------

#Hypothesis 4 - :








