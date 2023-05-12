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
titanic <- read.csv(url_Titanic)
lusitania <- read.csv(url_RMS_Lusitania)

#Relationship between age and survival rate:
#Hypothesis: The age of passengers affects their survival rate on both ships (titanic and lusitania). Children and older people may have a lower survival rate than passengers of middle age, as they might be less able to move in emergency situations or respond appropriately to them.

#histogram of age distribution in titanic dataset
ggplot(titanic, aes(x = Age)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Age Distribution of Passengers on the Titanic",
       x = "Age (years)", y = "Count")
#Most passengers are around 20 years old

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

