#loading packages
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(jsonlite)

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

# Define the UI
ui <- fluidPage(
  titlePanel("Cruise Ship catastrophes"),
  
  # Create a tabset panel with tabs for each hypothesis
  tabsetPanel(
    tabPanel("Titanic vs Lusitania",
             fluidRow(
               column(
                 width = 12,
                 h4("Hypothesis:"),
                 h6("The survival rate of passengers on the Titanic is higher than that of passengers on the Lusitania. The differences could be due to factors such as differing rescue measures, ship constructions, or circumstances of the disasters.")
               )
             ),
             fluidRow(
               column(
                 width = 6,
                 plotOutput("plot1", height = "400px")
               ),
               column(
                 width = 6,
                 plotOutput("plot12", height = "400px")
               )
             )
    ),
    tabPanel("Gender and Class -> Survival Rate",
             fluidRow(
               column(
                 width = 12,
                 h4("Hypothesis:"),
                 h6("The survival rate of passengers on the Titanic and the Lusitania differs depending on gender and passenger class. Specifically, women and first-class passengers have a higher survival rate than men and passengers from lower classes.")
               )
             ),
             fluidRow(
               column(
                 width = 6,
                 plotOutput("plot2", height = "400px")
               ),
               column(
                 width = 6,
                 plotOutput("plot21", height = "400px")
               )
             ),
             fluidRow(
               column(
                 width = 6,
                 plotOutput("plot22", height = "400px")
               ),
               column(
                 width = 6,
                 plotOutput("plot23", height = "400px")
               )
             ),
             fluidRow(
               column(
                 width = 6,
                 plotOutput("plot24", height = "400px")
               )
             )
    ),
    tabPanel("Age -> Survival Rate",
             fluidRow(
               column(
                 width = 12,
                 h4("Hypothesis:"),
                 h6("The age of passengers affects their survival rate on both ships. Children and older people may have a lower survival rate than passengers of middle age, as they might be less able to move in emergency situations or respond appropriately to them.")
               )
             ),
             fluidRow(
               column(
                 width = 6,
                 plotOutput("plot3", height = "400px")
               ),
               column(
                 width = 6,
                 plotOutput("plot31", height = "400px")
               )
             ),
             fluidRow(
               column(
                 width = 6,
                 plotOutput("plot32", height = "400px")
               )
             )
    ),
    tabPanel("Last Name -> Survival Rate",
             fluidRow(
               column(
                 width = 12,
                 h4("Hypothesis:"),
                 h6("The survival rate of passengers, whose last name begin with a letter in the beginning of the alphabet have a higher survival rate than those with a last name at the end of the alphabet. The difference between them could be due to the evacuation order.")
               )
             ),
             fluidRow(
               column(
                 width = 6,
                 plotOutput("plot4", height = "400px")
               ),
               column(
                 width = 6,
                 plotOutput("plot41", height = "400px")
               )
             ),
             fluidRow(
               column(
                 width = 6,
                 plotOutput("plot42", height = "400px")
               ),
               column(
                 width = 6,
                 plotOutput("plot43", height = "400px")
               )
             )
    ),
    tabPanel("Predication Case",
             fluidRow(
               column(
                 width = 12,
                 h4("Hypothesis:"),
                 h6("The logistic regression models trained on the Titanic and Lusitania datasets, considering factors such as Ticket_class and Sex, can predict the survival outcome of passengers with a certain degree of accuracy. By comparing the confusion matrices and accuracy scores of both models, we can evaluate if there are significant differences in their predictive capabilities. These differences may suggest variations in the factors influencing survival rates between the two ship disasters.")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 plotOutput("plot5", height = "400px")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 plotOutput("plot51", height = "400px")
               )
             )
    )
    
  )
)

# Define the server logic
server <- function(input, output) {
  # Hypothesis 1 plot
  output$plot1 <- renderPlot({
    p("p creates a paragraph of text.")
    
    lusitania_survivors <- sum(lusitania$Survived == "yes")
    lusitania_non_survivors <- sum(lusitania$Survived == "no")
   
    lusitania_survival_percent <- lusitania_survivors / nrow(lusitania) * 100
    lusitania_non_survival_percent <- lusitania_non_survivors / nrow(lusitania) * 100

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
    
    })
  
  output$plot12 <- renderPlot({
    
    # Calculate percentage of survivors
    titanic_survivors <- sum(titanic$Survived == "yes")
    titanic_non_survivors <- sum(titanic$Survived == "no")

    titanic_survival_percent <- titanic_survivors / nrow(titanic) * 100
    titanic_non_survival_percent <- titanic_non_survivors / nrow(titanic) * 100

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

  })
  # Hypothesis 2 plot
  output$plot2 <- renderPlot({
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
  })
  
  output$plot21 <- renderPlot({
    
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
    
    #2. Step -  Influence of the different ticket classes on the survival rate of passengers and whether their gender made a difference
    
    # Filter data for each ticket class
    class_1_data <- combined_summary[combined_summary$Ticket_class == "1", ]

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
    
    plot_class_1

  })
  
  output$plot22 <- renderPlot({
    plot_class_2
    
  })
  
  output$plot23 <- renderPlot({
    plot_class_3
  })
  
  output$plot24 <- renderPlot({
    plot_class_4
  })
  
  # Hypothesis 3 plot
  output$plot3 <- renderPlot({
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
  })
  
  output$plot31 <- renderPlot({
    # calculate survival rate for Titanic
    titanic_survival_rate <- round(mean(titanic$Survived == "yes") * 100, 2)
    
    # calculate survival rate for Lusitania
    lusitania_survival_rate <- round(mean(lusitania$Survived == "yes") * 100, 2)
    
    # create bar chart of survival rate for both ships
    ggplot(data.frame(Ship = c("Titanic", "Lusitania"),
                      Survival_Rate = c(titanic_survival_rate, lusitania_survival_rate)), 
           aes(x = Ship, y = Survival_Rate, fill = Ship)) +
      geom_bar(stat = "identity") +
      labs(title = "Survival Rate Comparison between Titanic and Lusitania",
           x = "", y = "Survival Rate (%)") +
      scale_fill_manual(values = c("Titanic" = "#ABDBD9", "Lusitania" = "#6F63A0"))
  })
  
  output$plot32 <- renderPlot({
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
  })
  
  # Hypothesis 4 plot
  output$plot4 <- renderPlot({
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
    
  })
  
  output$plot41 <- renderPlot({
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
  })
  
  output$plot42 <- renderPlot({
    # Create a bar plot to visualize survival rates for Titanic by group and ticket class
    ggplot(survival_rates_titanic, aes(x = Group, y = Survival_Rate, fill = factor(Ticket_class))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Group", y = "Survival Rate") +
      ggtitle("Survival Rates by Last Name Initials and Ticket Class - Titanic") +
      scale_x_continuous(breaks = 1:8, labels = group_labels) +
      scale_fill_manual(values = c("#EE6A50", "#FF7F00", "#F08080", "#ffe4c4")) +
      theme_minimal()
  })
  
  output$plot43 <- renderPlot({
    # Create a bar plot to visualize survival rates for Lusitania by group and ticket class
    ggplot(survival_rates_lusitania, aes(x = Group, y = Survival_Rate, fill = factor(Ticket_class))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Group", y = "Survival Rate") +
      ggtitle("Survival Rates by Last Name Initials and Ticket Class - Lusitania") +
      scale_x_continuous(breaks = 1:8, labels = group_labels) +
      scale_fill_manual(values = c("#EE6A50", "#FF7F00", "#F08080", "#ffe4c4")) +
      theme_minimal()
  })
  
  output$plot5 <- renderPlot({
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
  })
  
  output$plot51 <- renderPlot({
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
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
