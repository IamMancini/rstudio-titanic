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



#------------------------------------------------------------

initial_counts <- table(substr(lusitania$Family_name, 1, 1), lusitania$Group)

# Print the counts for each initial in each group
print(initial_counts)


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
  geom_bar(stat = "identity", fill = "blue") +
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
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Group", y = "Survival Rate") +
  ggtitle("Survival Rates by last name initials - Lusitania") +
  scale_x_continuous(breaks = 1:8, labels = group_labels)
