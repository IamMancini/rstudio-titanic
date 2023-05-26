#Dataset Titanic https://www.kaggle.com/c/titanic/data
#Dataset RMS Lusitania Complete Passanger Manifest

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



#------------------------------------------------------------

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
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  ggtitle("Survival Rate Comparison") +
  xlab("Ship") +
  ylab("Survival Rate")

ggplot(survival_rates, aes(x = Ship, y = Survival_Rate * 100, fill = Ship)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  ggtitle("Survival Rate Comparison") +
  xlab("Ship") +
  ylab("Survival Rate (%)")

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
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_void() +
  geom_text(aes(label = paste(round(Percent), "%")), position = position_stack(vjust = 0.5))

# Create a data frame with the survival data
lusitania_pie <- data.frame(Status = c("Survivors", "Non-survivors"), Percent = c(lusitania_survival_percent, lusitania_non_survival_percent))

# Create a pie chart of the survival data
ggplot(lusitania_pie, aes(x = "", y = Percent, fill = Status)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  ggtitle("Lusitania Survival Status") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_void() +
  geom_text(aes(label = paste(round(Percent), "%")), position = position_stack(vjust = 0.5))



# -----------------------------------------------------------

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

# Create a new column "Survived_binary" and populate it with binary values based on "Survived" column for Titanic
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

# Create a new column "Survived_binary" and populate it with binary values based on "Survived" column for Lusitania
lusitania$Survived_binary <- ifelse(lusitania$Survived %in% c("yes"), 1, ifelse(lusitania$Survived %in% c("no"), 0, NA))

# Calculate survival rates by group for Lusitania
survival_rates2 <- aggregate(Survived_binary ~ Group, data = lusitania, FUN = mean)

# Create a bar plot to visualize survival rates for Lusitania
ggplot(survival_rates2, aes(x = Group, y = Survived_binary)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Group", y = "Survival Rate") +
  ggtitle("Survival Rates by last name initials - Lusitania") +
  scale_x_continuous(breaks = 1:8, labels = group_labels)

#-------------------------GROUP AND CLASS

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
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
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
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
  theme_minimal()

#-------------PREDICTION--1-----------------------------

# Convert "Survived" column to factor
titanic$Survived <- factor(titanic$Survived, levels = c("no", "yes"))

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(titanic), 0.9 * nrow(titanic))
train_data <- titanic[train_indices, ]
test_data <- titanic[-train_indices, ]

# Perform logistic regression
model <- glm(Survived ~ Sex + Ticket_class, data = train_data, family = binomial())
summary(model)

# Make predictions on test data
test_data$predicted <- predict(model, newdata = test_data, type = "response")
test_data$predicted_class <- ifelse(test_data$predicted > 0.5, "yes", "no")

# Evaluate the model
accuracy <- mean((test_data$predicted > 0.5) == (test_data$Survived == "yes"))
cat("Accuracy:", accuracy, "\n")

# Create a scatter plot of predicted probabilities
ggplot(test_data, aes(x = Sex, y = Ticket_class, color = predicted > 0.5)) +
  geom_point() +
  scale_color_manual(values = c("blue", "red"), labels = c("Survived", "Did not survive")) +
  labs(title = "Logistic Regression Predictions",
       x = "Sex",
       y = "Ticket_class")

#-----

# Sort the predicted probabilities and corresponding actual outcomes
sorted_data <- test_data[order(-test_data$predicted), ]
sorted_outcomes <- sorted_data$Survived

# Calculate True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
tp_rate <- cumsum(sorted_outcomes == "yes") / sum(sorted_outcomes == "yes")
fp_rate <- cumsum(sorted_outcomes == "no") / sum(sorted_outcomes == "no")

# Plot the ROC curve
plot(fp_rate, tp_rate, type = "l", main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")

#-------------------------PREDICTION2------------

# Convert Survived, Ticket_class, and Sex to factors
titanic$Survived <- as.factor(titanic$Survived)
titanic$Ticket_class <- as.factor(titanic$Ticket_class)
titanic$Sex <- as.factor(titanic$Sex)

set.seed(123)  # Set a random seed for reproducibility
train_indices <- sample(1:nrow(titanic), nrow(titanic) * 0.9)
train_data <- titanic[train_indices, ]
test_data <- titanic[-train_indices, ]

# Train the logistic regression model
logistic_model <- glm(Survived ~ Ticket_class + Sex, data = train_data, family = binomial)

# Print the summary of the model
summary(logistic_model)

# Make predictions on the test set
test_predictions <- predict(logistic_model, newdata = test_data, type = "response")

# Convert predicted probabilities to class labels
#predicted_classes <- ifelse(test_predictions > 0.5, "Yes", "No")

predicted_classes <- ifelse(test_predictions > 0.5, levels(test_data$Survived)[2], levels(test_data$Survived)[1])


# Compare predicted classes with the actual values
comparison <- data.frame(Actual = test_data$Survived, Predicted = predicted_classes)

# View the comparison
head(comparison)



# Create a confusion matrix
confusion_matrix <- table(Actual = test_data$Survived, Predicted = predicted_classes)

# Plot the confusion matrix
confusion_matrix_plot <- ggplot(data = as.data.frame(confusion_matrix), aes(x = Actual, y = Predicted, fill = as.factor(Predicted))) +
  geom_tile(color = "white") +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted", fill = "Predicted") +
  scale_fill_manual(values = c("No" = "lightblue", "Yes" = "lightgreen")) +
  geom_text(aes(label = as.character(confusion_matrix)), color = "black", size = 15) +
  theme_minimal()

# Display the plot
print(confusion_matrix_plot)

# Calculate accuracy
accuracy <- sum(comparison$Actual == comparison$Predicted) / nrow(comparison)

# Print the accuracy
print(paste("Accuracy:", accuracy))

