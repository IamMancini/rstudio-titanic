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
original_titanic <- read.csv(url_Titanic)
original_lusitania <- read.csv(url_RMS_Lusitania)



clean_data_titanic <- function(original_titanic) {
  
  #Change sequence in Titanic
  original_titanic <- original_titanic %>%
    select(PassengerId, Name, Sex, Survived, everything())
  
  
  #Change the data from Titanic in Column "survived" to [yes = survived] and [no = not survived]
  original_titanic$Survived <- ifelse(original_titanic$Survived == 0, "no", "yes")
  
  
  #Seperate the name in Titanic
  original_titanic <- original_titanic %>%
    separate(Name, into = c("Family_name", "first_name"), sep = ", ") 
  
  
  #Change abbreviation in the column "Embarked"
  original_titanic$Embarked[original_titanic$Embarked == "C"] <- "Cherbourg"
  original_titanic$Embarked[original_titanic$Embarked == "Q"] <- "Queenstown"
  original_titanic$Embarked[original_titanic$Embarked == "S"] <- "Southampton"
  
  
  #Delete the title of a person from first_name in Titanic
  original_titanic$first_name <- gsub("\\w+\\.", "", original_titanic$first_name)
  
  
  #Change the name of the column from Pclass	to Ticket_class in Dataset Titanic
  colnames(original_titanic)[colnames(original_titanic) == "Pclass"] <- "Ticket_class"
  
  return(original_titanic)
}

titanic <- clean_data_titanic(original_titanic)



#---------------------------------------------------------------------

clean_data_lusitania <- function(original_lusitania) {
  
  #change the first column from Luistania to "PassengerId"
  names(original_lusitania)[1] <- "PassengerId"
  
  
  #rename "Family name" to "Family_name" in Lusitania
  original_lusitania <- original_lusitania %>% 
    rename(Family_name = `Family.name`)
  
  
  #rename Personal name to first_name in Lusitania
  original_lusitania <- rename(original_lusitania, first_name = Personal.name)
  
  
  #remove Title from Lusitania
  original_lusitania <- original_lusitania %>% 
    select(-Title)
  
  
  #Change Column for "Sex" from Lusitania
  original_lusitania <- original_lusitania %>%
    select(PassengerId, Family_name, first_name, Sex, everything())
  
  
  #Change the name of the column "Fate" to "Survived" in Lusitania
  original_lusitania <- original_lusitania %>%
    rename(Survived = Fate)
  
  
  #Change the data from Lusitania in Column "Survival" to [yes =îf survived] and [no if not survived]
  original_lusitania$Survived <- ifelse(original_lusitania$Survived == "Lost", "no", "yes")
  
  
  #Write the data in the column "Sex" lower case
  original_lusitania$Sex <- tolower(original_lusitania$Sex)
  
  
  #Write the first letter in Family_name capital and the rest in lower Cases
  original_lusitania$Family_name <- sapply(original_lusitania$Family_name, function(x) {
    paste(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))), sep="")
  })
  
  
  #Change PassengerId from 0 to 1 in Lusitania and the rest of the numbers in the column should increase by 1
  original_lusitania$PassengerId <- original_lusitania$PassengerId - min(original_lusitania$PassengerId) + 1
  
  
  #Change the name of the column from Department.Class to Ticket_class in Dataset Lusitania
  colnames(original_lusitania)[colnames(original_lusitania) == "Department.Class"] <- "Ticket_class"
  
  
  #In the Lusitania Dataset the data is going to be renamed, so that they match the ticket class from titanic. Saloon is going to be 1 / Second is going to be 2 / Thrid is going to be 3 / The rest of the Ticket classes are going to be 4
  original_lusitania$Ticket_class <- gsub("Saloon", "1", original_lusitania$Ticket_class)
  original_lusitania$Ticket_class <- gsub("Second", "2", original_lusitania$Ticket_class)
  original_lusitania$Ticket_class <- gsub("Third", "3", original_lusitania$Ticket_class)
  original_lusitania$Ticket_class <- gsub("Victualling|Engineering|Deck|Band", "4", original_lusitania$Ticket_class)
  
  
  
  #Delete stowaway from dataset Lusitiana, because they dont include any data or value for this project
  original_lusitania <- filter(original_lusitania, Ticket_class != "Stowaway")
  
  return(original_lusitania)
}


lusitania <- clean_data_lusitania(original_lusitania)



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

#histogram of age distribution in titanic dataset
ggplot(titanic, aes(x = Age)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Age Distribution of Passengers on the Titanic",
       x = "Age (years)", y = "Count")

#histogram of age distribution in lusitania dataset
ggplot(lusitania, aes(x = Age)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Age Distribution of Passengers on the Lusitania",
       x = "Age (years)", y = "Count")

#Results show that there were more passengers in the middle age range (20-40) in both datasets.

# bar chart of survival rate in titanic dataset
ggplot(titanic, aes(x = Survived, fill = Survived)) +
  geom_bar() +
  labs(title = "Survival Rate of Passengers on the Titanic",
       x = "Survived", y = "Count") +
  scale_fill_manual(values = c("no" = "red", "yes" = "green"))

# bar chart of survival rate in lusitania dataset
ggplot(lusitania, aes(x = Survived, fill = Survived)) +
  geom_bar() +
  labs(title = "Survival Rate of Passengers on the Lusitania",
       x = "Survived", y = "Count") +
  scale_fill_manual(values = c("no" = "red", "yes" = "green"))

# scatterplot of age and survival rate in titanic dataset
ggplot(titanic, aes(x = Age, y = factor(Survived), color = Survived)) +
  geom_jitter(width = 0.5) +
  labs(title = "Relationship between Age and Survival Rate on the Titanic",
       x = "Age (years)", y = "Survived") +
  scale_color_manual(values = c("no" = "red", "yes" = "green"))

# scatterplot of age and survival rate in lusitania dataset
ggplot(lusitania, aes(x = Age, y = factor(Survived), color = Survived)) +
  geom_jitter(width = 0.5) +
  labs(title = "Relationship between Age and Survival Rate on the Lusitania",
       x = "Age (years)", y = "Survived") +
  scale_color_manual(values = c("no" = "red", "yes" = "green"))






# Titanic dataset
titanic_survivors <- subset(titanic, Survived == "yes")
titanic_non_survivors <- subset(titanic, Survived == "no")

ggplot(titanic_survivors, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "green", alpha = 0.5) +
  ggtitle("Age Distribution of Titanic Survivors") +
  xlab("Age (years)") + ylab("Count") +
  theme_bw()

ggplot(titanic_non_survivors, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "red", alpha = 0.5) +
  ggtitle("Age Distribution of Titanic Non-Survivors") +
  xlab("Age (years)") + ylab("Count") +
  theme_bw()

#Most survivors are between 25-35
#Most non survivors are 20 and 30

# Lusitania dataset
lusitania_survivors <- subset(lusitania, Survived == "yes")
lusitania_non_survivors <- subset(lusitania, Survived == "no")

ggplot(lusitania_survivors, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "green", alpha = 0.5) +
  ggtitle("Age Distribution of Lusitania Survivors") +
  xlab("Age (years)") + ylab("Count") +
  theme_bw()

ggplot(lusitania_non_survivors, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "red", alpha = 0.5) +
  ggtitle("Age Distribution of Lusitania Non-Survivors") +
  xlab("Age (years)") + ylab("Count") +
  theme_bw()

#T-Test
# Filter out missing age values
titanic_age <- titanic %>% filter(!is.na(Age))

# Split the dataset by survival status
survivors <- titanic_age %>% filter(Survived == "yes")
non_survivors <- titanic_age %>% filter(Survived == "no")

# Perform t-test
t_test_age <- t.test(survivors$Age, non_survivors$Age, var.equal = TRUE)

# Print t-test results
print(t_test_age)




# Filter out missing age values
lusitania_age <- lusitania %>% filter(!is.na(Age))

# Split the dataset by survival status
survivors <- lusitania_age %>% filter(Survived == "yes")
non_survivors <- lusitania_age %>% filter(Survived == "no")

# Perform t-test
t_test_age <- t.test(survivors$Age, non_survivors$Age, var.equal = TRUE)

# Print t-test results
print(t_test_age)







#------------------------------------------------------------------------------------------------------

#Hypothesis 3 - :









#------------------------------------------------------------------------------------------------------

#Hypothesis 4 - :

# Calculate survival rates
titanic_survival_rate <- mean(ifelse(titanic$Survived == "yes", 1, 0))
lusitania_survival_rate <- mean(ifelse(lusitania$Survived == "yes", 1, 0))

# Print survival rates
cat("Titanic survival rate:", titanic_survival_rate, "\n")
cat("Lusitania survival rate:", lusitania_survival_rate, "\n")

# Create a bar chart comparing the survival rates of both ships
survival_rates <- data.frame(Ship = c("Titanic", "Lusitania"), Survival_Rate = c(titanic_survival_rate, lusitania_survival_rate))
ggplot(survival_rates, aes(x = Ship, y = Survival_Rate, fill = Ship)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue", "red")) +
  ggtitle("Survival Rate Comparison") +
  xlab("Ship") +
  ylab("Survival Rate")

# Calculate percentage of survivors and non-survivors
titanic_survivors <- sum(titanic$Survived == "yes")
titanic_non_survivors <- sum(titanic$Survived == "no")
lusitania_survivors <- sum(lusitania$Survived == "yes")
lusitania_non_survivors <- sum(lusitania$Survived == "no")

titanic_survival_percent <- titanic_survivors / nrow(titanic) * 100
titanic_non_survival_percent <- titanic_non_survivors / nrow(titanic) * 100
lusitania_survival_percent <- lusitania_survivors / nrow(lusitania) * 100
lusitania_non_survival_percent <- lusitania_non_survivors / nrow(lusitania) * 100
# Print survival rates and percentages
cat("Titanic survival rate:", titanic_survival_rate, "\n")
cat("Titanic survivors:", titanic_survivors, "(", round(titanic_survival_percent, 2), "%)", "\n")
cat("Titanic non-survivors:", titanic_non_survivors, "(", round(titanic_non_survival_percent, 2), "%)", "\n")

cat("Lusitania survival rate:", lusitania_survival_rate, "\n")
cat("Lusitania survivors:", lusitania_survivors, "(", round(lusitania_survival_percent, 2), "%)", "\n")
cat("Lusitania non-survivors:", lusitania_non_survivors, "(", round(lusitania_non_survival_percent, 2), "%)", "\n")

# Create a data frame with the survival data
titanic_pie <- data.frame(Status = c("Survivors", "Non-survivors"), Percent = c(titanic_survival_percent, titanic_non_survival_percent))

# Create a pie chart of the survival data
ggplot(titanic_pie, aes(x = "", y = Percent, fill = Status)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  ggtitle("Titanic Survival Status") +
  scale_fill_manual(values = c("darkgreen", "darkred")) +
  theme_void() +
  geom_text(aes(label = paste(round(Percent), "%")), position = position_stack(vjust = 0.5))

# Create a data frame with the survival data
lusitania_pie <- data.frame(Status = c("Survivors", "Non-survivors"), Percent = c(lusitania_survival_percent, lusitania_non_survival_percent))

# Create a pie chart of the survival data
ggplot(lusitania_pie, aes(x = "", y = Percent, fill = Status)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  ggtitle("Lusitania Survival Status") +
  scale_fill_manual(values = c("darkgreen", "darkred")) +
  theme_void() +
  geom_text(aes(label = paste(round(Percent), "%")), position = position_stack(vjust = 0.5))





