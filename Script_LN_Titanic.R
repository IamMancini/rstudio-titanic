#loading packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(tidyverse)

#Create URL for dataset
url_Titanic <- "https://raw.githubusercontent.com/IamMancini/rstudio-titanic/main/train.csv"
url_RMS_Lusitania <- "https://raw.githubusercontent.com/IamMancini/rstudio-titanic/main/LusitaniaManifest.csv"

#load Dataset
original_titanic <- read.csv(url_Titanic)
original_lusitania <- read.csv(url_RMS_Lusitania)


#Create a function that cleans the Dataset Titanic
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
#Create a function that cleans the dataset lusitania
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



#-------------------------------------------------------------------------------

#Hypothesis - survival rate in relation to gender and class:

#1. Step - To start, its important to know, how many people were female and how many were male on both ships

# Calculate the percentage of males and females on Lusitania
lusitania_gender <- lusitania %>%
  group_by(Sex) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100, Ship = "Lusitania")

# Calculate the percentage of males and females on Titanic
titanic_gender <- titanic %>%
  group_by(Sex) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100, Ship = "Titanic")

# Combine the gender data from both ships
combined_gender <- bind_rows(lusitania_gender, titanic_gender)

# Plot the percentage of males and females on both ships
ggplot(combined_gender, aes(x = Ship, y = percentage, fill = Sex)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = paste0(round(percentage), "%")), 
            position = position_fill(vjust = 0.5), 
            color = "white", size = 4, fontface = "bold") +
  labs(title = "Percentage of Males and Females on Lusitania and Titanic",
       x = "Ship", y = "Percentage") +
  scale_fill_manual(values = c("#E69F00", "#0072B2"), name = "Sex") +
  theme_minimal()

################################################################################


#2. Step -  Preperation for the folowing plots


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
  group_by(dataset, Sex, Ticket_class, Survived) %>%
  summarize(n = n()) %>%
  mutate(pct_survived = n / sum(n) * 100)

################################################################################


#2. Step -  Influence of the different ticket classes on the survival rate of passengers and whether their gender made a difference

# Filter data for each ticket class
class_1_data <- combined_summary[combined_summary$Ticket_class == "1", ]
class_2_data <- combined_summary[combined_summary$Ticket_class == "2", ]
class_3_data <- combined_summary[combined_summary$Ticket_class == "3", ]
class_4_data <- combined_summary[combined_summary$Ticket_class == "4", ]

# Plot for Ticket Class 1
plot_class_1 <- ggplot(class_1_data, aes(x = Sex, y = n, fill = Survived)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.9) +
  facet_grid(rows = vars(dataset)) +
  scale_fill_manual(values = c("#D55E00", "#009E73"), name = "Survived") +
  geom_text(aes(label = paste0(n, " (", round(pct_survived), "%)")),
            position = position_dodge(width = 0.9), vjust = -1.5) +
  labs(title = "Ticket Class 1 - Survival rate of passengers",
       x = "Sex", y = "Number of passengers") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "bottom") +
  coord_cartesian(ylim = c(0, 200))

# Plot for Ticket Class 2
plot_class_2 <- ggplot(class_2_data, aes(x = Sex, y = n, fill = Survived)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.9) +
  facet_grid(rows = vars(dataset)) +
  scale_fill_manual(values = c("#D55E00", "#009E73"), name = "Survived") +
  geom_text(aes(label = paste0(n, " (", round(pct_survived), "%)")),
            position = position_dodge(width = 0.9), vjust = -1.5) +
  labs(title = "Ticket Class 2 - Survival rate of passengers",
       x = "Sex", y = "Number of passengers") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "bottom") +
  coord_cartesian(ylim = c(0, 200))

# Plot for Ticket Class 3
plot_class_3 <- ggplot(class_3_data, aes(x = Sex, y = n, fill = Survived)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.9) +
  facet_grid(rows = vars(dataset)) +
  scale_fill_manual(values = c("#D55E00", "#009E73"), name = "Survived") +
  geom_text(aes(label = paste0(n, " (", round(pct_survived), "%)")),
            position = position_dodge(width = 0.9), vjust = -1.5) +
  labs(title = "Ticket Class 3 - Survival rate of passengers",
       x = "Sex", y = "Number of passengers") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "bottom") +
  coord_cartesian(ylim = c(0, 350))

# Plot for Ticket Class 4
plot_class_4 <- ggplot(class_4_data, aes(x = Sex, y = n, fill = Survived)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.9) +
  facet_grid(rows = vars(dataset)) +
  scale_fill_manual(values = c("#D55E00", "#009E73"), name = "Survived") +
  geom_text(aes(label = paste0(n, " (", round(pct_survived), "%)")),
            position = position_dodge(width = 0.9), vjust = -1.5) +
  labs(title = "Ticket Class 4 - Survival rate of passengers",
       x = "Sex", y = "Number of passengers") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "bottom") +
  coord_cartesian(ylim = c(0, 450))

# Display the individual plots for all the Ticket_classes
plot_class_1
plot_class_2
plot_class_3
plot_class_4



################################################################################


#Bar-chart only with number of passengers 
ggplot(combined_summary, aes(x = Sex, y = n, fill = Survived)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.9) +
  facet_grid(rows = vars(Ticket_class), cols = vars(dataset)) +
  scale_fill_manual(values = c("#D55E00", "#009E73"), name = "Survived") +
  geom_text(aes(label = n),
            position = position_dodge(width = 0.9), vjust = -1.0) +
  ylim(0, 600) +
  labs(title = "Survival rate of passengers on Titanic and Lusitania",
       x = "Sex", y = "Number of passengers") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "bottom")


################################################################################

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
  scale_fill_manual(values = c("#6f63a0", "#ABDBD9")) +
  geom_text(aes(label = total_passengers), position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 3.5)


################################################################################

#2. Create a grouped bar chart to compare the number of survivors and non-survivors by gender and dataset

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
  scale_fill_manual(values = c("#d55e00", "#009E73"), name = "Survived",
                    labels = c("No", "Yes")) +
  labs(title = "Survival of passengers on Titanic and Lusitania by gender",
       x = "Dataset", y = "Number of passengers") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "bottom")

################################################################################

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
  scale_fill_manual(values = c("#d55e00", "#009E73"), name = "Survived") +
  geom_text(aes(label = paste0(round(pct_survived), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Survival rate of passengers on Titanic and Lusitania by gender",
       x = "Sex", y = "Number of passengers") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "bottom")




#-------------------------------------------------------------------------------

#Hypothesis - age and survival:

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

# select only the relevant columns from the Titanic and Lusitania data frames
titanic_selected <- titanic %>% select(Age)
lusitania_selected <- lusitania %>% select(Age)

# add a "Ship" column to each data frame
titanic_selected$Ship <- "Titanic"
lusitania_selected$Ship <- "Lusitania"

# combine the two data frames using rbind()
combined_data <- rbind(titanic_selected, lusitania_selected)

# create a histogram of the combined age distribution
ggplot(combined_data, aes(x = Age)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Ship, ncol = 2) +
  labs(title = "Age Distribution of Passengers on the Titanic and Lusitania",
       x = "Age (years)", y = "Count")

#Results show that there were more passengers in the middle age range (20-40) in both datasets.

# bar chart of survival rate in titanic dataset
ggplot(titanic, aes(x = Survived, fill = Survived)) +
  geom_bar() +
  labs(title = "Survival Rate of Passengers on the Titanic",
       x = "Survived", y = "Count") +
  scale_fill_manual(values = c("no" = "#D55E00", "yes" = "#009E73"))

# bar chart of survival rate in lusitania dataset
ggplot(lusitania, aes(x = Survived, fill = Survived)) +
  geom_bar() +
  labs(title = "Survival Rate of Passengers on the Lusitania",
       x = "Survived", y = "Count") +
  scale_fill_manual(values = c("no" = "#D55E00", "yes" = "#009E73"))

###############################################################################

# calculate survival rate for Titanic
titanic_survival_rate <- round(mean(titanic$Survived == "yes") * 100, 2)

# calculate survival rate for Lusitania
lusitania_survival_rate <- round(mean(lusitania$Survived == "yes") * 100, 2)

# print survival rates
cat("Titanic Survival Rate: ", titanic_survival_rate, "%\n")
cat("Lusitania Survival Rate: ", lusitania_survival_rate, "%\n")

# create bar chart of survival rate for both ships
ggplot(data.frame(Ship = c("Titanic", "Lusitania"),
                  Survival_Rate = c(titanic_survival_rate, lusitania_survival_rate)), 
       aes(x = Ship, y = Survival_Rate, fill = Ship)) +
  geom_bar(stat = "identity") +
  labs(title = "Survival Rate Comparison between Titanic and Lusitania",
       x = "", y = "Survival Rate (%)") +
  scale_fill_manual(values = c("Titanic" = "#ABDBD9", "Lusitania" = "#6F63A0"))

###############################################################################

# Titanic dataset
titanic_survivors <- subset(titanic, Survived == "yes")
titanic_non_survivors <- subset(titanic, Survived == "no")

ggplot(titanic_survivors, aes(x = Age)) +
  geom_histogram(binwidth = 5, alpha = 0.5) +
  ggtitle("Age Distribution of Titanic Survivors") +
  xlab("Age (years)") + ylab("Count") +
  theme_bw()

ggplot(titanic_non_survivors, aes(x = Age)) +
  geom_histogram(binwidth = 5, alpha = 0.5) +
  ggtitle("Age Distribution of Titanic Non-Survivors") +
  xlab("Age (years)") + ylab("Count") +
  theme_bw()

#Most survivors are between 25-35
#Most non survivors are 20 and 30

# Lusitania dataset
lusitania_survivors <- subset(lusitania, Survived == "yes")
lusitania_non_survivors <- subset(lusitania, Survived == "no")

ggplot(lusitania_survivors, aes(x = Age)) +
  geom_histogram(binwidth = 5, alpha = 0.5) +
  ggtitle("Age Distribution of Lusitania Survivors") +
  xlab("Age (years)") + ylab("Count") +
  theme_bw()

ggplot(lusitania_non_survivors, aes(x = Age)) +
  geom_histogram(binwidth = 5, alpha = 0.5) +
  ggtitle("Age Distribution of Lusitania Non-Survivors") +
  xlab("Age (years)") + ylab("Count") +
  theme_bw()

###############################################################################

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

###############################################################################

# create a new age group variable
titanic <- titanic %>%
  mutate(age_group = cut(Age, breaks = seq(0, 80, 10), labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79")))

lusitania <- lusitania %>%
  mutate(age_group = cut(Age, breaks = seq(0, 80, 10), labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79")))

# calculate survival rates by age group for both ships
titanic_survival_rate_age <- titanic %>%
  group_by(age_group) %>%
  summarise(survival_rate = round(mean(Survived == "yes") * 100, 2))

lusitania_survival_rate_age <- lusitania %>%
  group_by(age_group) %>%
  summarise(survival_rate = round(mean(Survived == "yes") * 100, 2))

# combine the survival rate data for both ships
combined_survival_rate_age <- bind_rows(
  titanic_survival_rate_age %>% mutate(Ship = "Titanic"),
  lusitania_survival_rate_age %>% mutate(Ship = "Lusitania")
)

ggplot(combined_survival_rate_age, aes(x = age_group, y = survival_rate, fill = Ship)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Survival Rate by Age Group Comparison between Titanic and Lusitania",
       x = "Age Group", y = "Survival Rate (%)", fill = "Ship") +
  scale_fill_manual(values = c("#6F63A0","#ABDBD9")) +
  theme(legend.position = "bottom")


#------------------------------------------------------------------------------------------------------

# Hypothesis - survival rate

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
  scale_fill_manual(values = c("#6f63a0", "#ABDBD9")) +
  ggtitle("Survival Rate Comparison") +
  xlab("Ship") +
  ylab("Survival Rate")

# Calculate percentage of survivors
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
  scale_fill_manual(values = c("#D55E00", "#009E73")) +
  theme_void() +
  geom_text(aes(label = paste(round(Percent), "%")), position = position_stack(vjust = 0.5))

# Create a data frame with the survival data
lusitania_pie <- data.frame(Status = c("Survivors", "Non-survivors"), Percent = c(lusitania_survival_percent, lusitania_non_survival_percent))

# Create a pie chart of the survival data
ggplot(lusitania_pie, aes(x = "", y = Percent, fill = Status)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  ggtitle("Lusitania Survival Status") +
  scale_fill_manual(values = c("#D55E00", "#009E73")) +
  theme_void() +
  geom_text(aes(label = paste(round(Percent), "%")), position = position_stack(vjust = 0.5))

#-------------------------------------------------------------------------------

# Hypothesis - family name first letter

# Create a function to assign group based on the first letter of family name
assign_group <- function(name) {
  first_letter <- substr(name, 1, 1)  # Get the first letter of the name
  first_letter <- toupper(first_letter)  # Convert it to uppercase
  
  if (first_letter %in% c("A", "B", "C")) {
    return(1)  # Group 1
  } else if (first_letter %in% c("D", "E", "F")) {
    return(2)  # Group 2
  } else if (first_letter %in% c("G", "H", "I")) {
    return(3)  # Group 3
  } else if (first_letter %in% c("J", "K", "L")) {
    return(4)  # Group 4
  } else if (first_letter %in% c("M", "N", "O")) {
    return(5)  # Group 5
  } else if (first_letter %in% c("P", "Q", "R")) {
    return(6)  # Group 6
  } else if (first_letter %in% c("S", "T", "U")) {
    return(7)  # Group 7
  } else if (first_letter %in% c("V", "W", "X", "Y", "Z")) {
    return(8)  # Group 8
  } else {
    return(NA)  # For names that don't fall into any group
  }
}


# Apply the function to create a new column "Group" in Titanic
titanic$Group <- sapply(titanic$Family_name, assign_group)

# Create a new column "Survived_binary" for Titanic
titanic$Survived_binary <- ifelse(titanic$Survived %in% c("yes"), 1, ifelse(titanic$Survived %in% c("no"), 0, NA))

# Calculate survival rates by group for Titanic
survival_rates1 <- aggregate(Survived_binary ~ Group, data = titanic, FUN = mean)

# Create a vector of labels for each group
group_labels <- c("A, B, C", "D, E, F", "G, H, I", "J, K, L", "M, N, O", "P, Q, R", "S, T, U", "V, W, X, Y, Z")

# Create a bar plot to visualize survival rates for Titanic
ggplot(survival_rates1, aes(x = Group, y = Survived_binary)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Group", y = "Survival Rate") +
  ggtitle("Survival Rates by last name initials - Titanic") +
  scale_x_continuous(breaks = 1:8, labels = group_labels)


# Apply the function to create a new column "Group" in Lusitania
lusitania$Group <- sapply(lusitania$Family_name, assign_group)

# Create a new column "Survived_binary" for Lusitania
lusitania$Survived_binary <- ifelse(lusitania$Survived %in% c("yes"), 1, ifelse(lusitania$Survived %in% c("no"), 0, NA))

# Calculate survival rates by group for Lusitania
survival_rates2 <- aggregate(Survived_binary ~ Group, data = lusitania, FUN = mean)

# Create a bar plot to visualize survival rates for Lusitania
ggplot(survival_rates2, aes(x = Group, y = Survived_binary)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Group", y = "Survival Rate") +
  ggtitle("Survival Rates by last name initials - Lusitania") +
  scale_x_continuous(breaks = 1:8, labels = group_labels)


# Ticket class added to calculation

# Calculate survival rates by group and ticket class for Titanic
survival_rates_titanic <- titanic %>%
  group_by(Group, Ticket_class) %>%
  summarize(Survival_Rate = mean(Survived_binary, na.rm = TRUE))

# Create a bar plot to visualize survival rates for Titanic by group and ticket class
ggplot(survival_rates_titanic, aes(x = Group, y = Survival_Rate, fill = factor(Ticket_class))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Group", y = "Survival Rate") +
  ggtitle("Survival Rates by Last Name Initials and Ticket Class - Titanic") +
  scale_x_continuous(breaks = 1:8, labels = group_labels) +
  scale_fill_manual(values = c("#EE6A50", "#FF7F00", "#F08080", "#ffe4c4")) +
  theme_minimal()


# Calculate survival rates by group and ticket class for Lusitania
survival_rates_lusitania <- lusitania %>%
  group_by(Group, Ticket_class) %>%
  summarize(Survival_Rate = mean(Survived_binary, na.rm = TRUE))

# Create a bar plot to visualize survival rates for Lusitania by group and ticket class
ggplot(survival_rates_lusitania, aes(x = Group, y = Survival_Rate, fill = factor(Ticket_class))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Group", y = "Survival Rate") +
  ggtitle("Survival Rates by Last Name Initials and Ticket Class - Lusitania") +
  scale_x_continuous(breaks = 1:8, labels = group_labels) +
  scale_fill_manual(values = c("#EE6A50", "#FF7F00", "#F08080", "#ffe4c4")) +
  theme_minimal()


#-------------------------PREDICTION TITANIC--------------------------------------------------

# Convert Survived, Ticket_class, and Sex to factors
titanic$Survived <- as.factor(titanic$Survived)
titanic$Ticket_class <- as.factor(titanic$Ticket_class)
titanic$Sex <- as.factor(titanic$Sex)

set.seed(123)  # Set a random seed for reproducibility
train_indices_t <- sample(1:nrow(titanic), nrow(titanic) * 0.9)
train_data_t <- titanic[train_indices_t, ]
test_data_t <- titanic[-train_indices_t, ]

# Train the logistic regression model
logistic_model_t <- glm(Survived ~ Ticket_class + Sex, data = train_data_t, family = binomial)

# Print the summary of the model
summary(logistic_model_t)

# Make predictions on the test set
test_predictions_t <- predict(logistic_model_t, newdata = test_data_t, type = "response")

# Convert predicted probabilities to class labels
predicted_classes_t <- ifelse(test_predictions_t > 0.5, levels(test_data_t$Survived)[2], levels(test_data_t$Survived)[1])


# Compare predicted classes with the actual values
comparison_t <- data.frame(Actual = test_data_t$Survived, Predicted = predicted_classes_t)

# View the comparison
head(comparison_t)



# Create a confusion matrix
confusion_matrix_t <- table(Actual = test_data_t$Survived, Predicted = predicted_classes_t)

# Plot the confusion matrix
confusion_matrix_plot_t <- ggplot(data = as.data.frame(confusion_matrix_t), aes(x = Actual, y = Predicted, fill = as.factor(Predicted))) +
  geom_tile(color = "white") +
  labs(title = "Confusion Matrix - Titanic", x = "Actual", y = "Predicted", fill = "Predicted") +
  scale_fill_manual(values = c("No" = "lightblue", "Yes" = "lightgreen")) +
  geom_text(aes(label = as.character(confusion_matrix_t)), color = "black", size = 15) +
  theme_minimal()

# Display the plot
print(confusion_matrix_plot_t)

# Calculate accuracy
accuracy_t <- sum(comparison_t$Actual == comparison_t$Predicted) / nrow(comparison_t)

# Print the accuracy
print(paste("Accuracy:", accuracy_t))

#---------------------PREDICTION LUSITANIA----------------------------------------------------

# Convert Survived, Ticket_class, and Sex to factors
lusitania$Survived <- as.factor(lusitania$Survived)
lusitania$Ticket_class <- as.factor(lusitania$Ticket_class)
lusitania$Sex <- as.factor(lusitania$Sex)

# Filter lusitania dataset to include only Ticket_class values 1, 2, and 3
lusitania <- lusitania[lusitania$Ticket_class %in% c(1, 2, 3), ]

set.seed(123)  # Set a random seed for reproducibility
train_indices_l <- sample(1:nrow(lusitania), nrow(lusitania) * 0.9)
train_data_l <- lusitania[train_indices_l, ]
test_data_l <- lusitania[-train_indices_l, ]

# Train the logistic regression model
logistic_model_l <- glm(Survived ~ Ticket_class + Sex, data = train_data_l, family = binomial)

# Print the summary of the model
summary(logistic_model_l)

# Make predictions on the test set
test_predictions_l <- predict(logistic_model_l, newdata = test_data_l, type = "response")

# Convert predicted probabilities to class labels
predicted_classes_l <- ifelse(test_predictions_l > 0.5, levels(test_data_l$Survived)[2], levels(test_data_l$Survived)[1])

# Compare predicted classes with the actual values
comparison_l <- data.frame(Actual = test_data_l$Survived, Predicted = predicted_classes_l)

# View the comparison
head(comparison_l)

# Create a confusion matrix
confusion_matrix_l <- table(Actual = test_data_l$Survived, Predicted = predicted_classes_l)

# Plot the confusion matrix
confusion_matrix_plot_l <- ggplot(data = as.data.frame(confusion_matrix_l), aes(x = Actual, y = Predicted, fill = as.factor(Predicted))) +
  geom_tile(color = "white") +
  labs(title = "Confusion Matrix - Lusitania", x = "Actual", y = "Predicted", fill = "Predicted") +
  scale_fill_manual(values = c("No" = "lightblue", "Yes" = "lightgreen")) +
  geom_text(aes(label = as.character(confusion_matrix_l)), color = "black", size = 15) +
  theme_minimal()

# Display the plot
print(confusion_matrix_plot_l)

# Calculate accuracy
accuracy_l <- sum(comparison_l$Actual == comparison_l$Predicted) / nrow(comparison_l)

# Print the accuracy
print(paste("Accuracy:", accuracy_l))

