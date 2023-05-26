# Install required packages if not already installed
if (!require("shiny")) {
  install.packages("shiny")
}
if (!require("dplyr")) {
  install.packages("dplyr")
}
if (!require("tidyr")) {
  install.packages("tidyr")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
if (!require("stringr")) {
  install.packages("stringr")
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
}

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(tidyverse)

# UI
ui <- fluidPage(
  titlePanel("Titanic and Lusitania Analysis"),
  sidebarLayout(
    sidebarPanel(
      h3("Select an analysis:"),
      selectInput(
        inputId = "analysis",
        label = NULL,
        choices = c("Gender Distribution", "Survival Rate by Gender and Class", "Survival Rate by Ticket Class"),
        selected = "Gender Distribution"
      )
    ),
    mainPanel(
      fluidRow(
        uiOutput("plotOutput")
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Clean the Titanic dataset
  clean_data_titanic <- function(original_titanic) {
    # Extract relevant columns and clean missing values
    titanic_cleaned <- original_titanic %>%
      select(Survived, Sex, Pclass) %>%
      mutate(
        Survived = factor(Survived, levels = c(0, 1), labels = c("No", "Yes")),
        Sex = factor(Sex, levels = c("female", "male"), labels = c("Female", "Male")),
        Pclass = factor(Pclass, levels = c(1, 2, 3), labels = c("First", "Second", "Third"))
      ) %>%
      na.omit()
    
    return(titanic_cleaned)
  }
  
  # Clean the Lusitania dataset
  clean_data_lusitania <- function(original_lusitania) {
    # Extract relevant columns and clean missing values
    lusitania_cleaned <- original_lusitania %>%
      select(Sex) %>%
      mutate(
        Sex = ifelse(Sex %in% c("F", "FEMALE", "WOMAN"), "Female",
                     ifelse(Sex %in% c("M", "MALE", "MAN"), "Male", "Unknown"))
      ) %>%
      filter(Sex != "Unknown") %>%
      na.omit()
    
    return(lusitania_cleaned)
  }
  
  # Load the Titanic dataset
  original_titanic <- read.csv("https://raw.githubusercontent.com/IamMancini/rstudio-titanic/main/train.csv")
  titanic <- clean_data_titanic(original_titanic)
  
  # Load the Lusitania dataset
  original_lusitania <- read.csv("https://raw.githubusercontent.com/IamMancini/rstudio-titanic/main/LusitaniaManifest.csv")
  lusitania <- clean_data_lusitania(original_lusitania)
  
  # Generate the gender distribution plot
  output$genderDistributionPlot <- renderPlot({
    # Calculate gender distribution
    titanic_gender <- titanic %>%
      group_by(Sex) %>%
      summarise(Count = n())
    
    lusitania_gender <- lusitania %>%
      group_by(Sex) %>%
      summarise(Count = n())
    
    total_titanic <- sum(titanic_gender$Count)
    total_lusitania <- sum(lusitania_gender$Count)
    
    # Create a combined dataset
    gender_data <- bind_rows(
      data.frame(Dataset = "Titanic", titanic_gender),
      data.frame(Dataset = "Lusitania", lusitania_gender)
    )
    
    # Exclude "Unknown" category if it doesn't exist
    if (!"Unknown" %in% gender_data$Sex) {
      gender_data <- gender_data[gender_data$Sex != "Unknown", ]
    }
    
    # Plot the gender distribution
    ggplot(gender_data, aes(x = Dataset, y = Count, fill = Sex)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Gender Distribution",
        x = "Dataset",
        y = "Count"
      ) +
      scale_fill_manual(
        values = c("#0073C2FF", "#FC4E07FF"),
        labels = c("Female", "Male")
      ) +
      theme_minimal()
  })
  
  # Generate the survival rate by gender and class plot
  output$genderClassSurvivalPlot <- renderPlot({
    # Calculate survival rate by gender and class
    survival_gender_class <- titanic %>%
      group_by(Sex, Pclass) %>%
      summarise(
        SurvivalRate = mean(Survived == "Yes") * 100
      )
    
    # Plot the survival rate by gender and class
    ggplot(survival_gender_class, aes(x = Pclass, y = SurvivalRate, fill = Sex)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Survival Rate by Gender and Class",
        x = "Class",
        y = "Survival Rate (%)"
      ) +
      scale_fill_manual(
        values = c("#0073C2FF", "#FC4E07FF"),
        labels = c("Female", "Male")
      ) +
      theme_minimal()
  })
  
  # Generate the survival rate by ticket class plot
  output$ticketClassSurvivalPlot <- renderPlot({
    # Calculate survival rate by ticket class
    survival_ticket_class <- titanic %>%
      group_by(Pclass) %>%
      summarise(
        SurvivalRate = mean(Survived == "Yes") * 100
      )
    
    # Plot the survival rate by ticket class
    ggplot(survival_ticket_class, aes(x = Pclass, y = SurvivalRate)) +
      geom_bar(stat = "identity", fill = "#0073C2FF") +
      labs(
        title = "Survival Rate by Ticket Class",
        x = "Class",
        y = "Survival Rate (%)"
      ) +
      theme_minimal()
  })
  
  # Render the selected plot based on user input
  output$plotOutput <- renderUI({
    analysis <- input$analysis
    
    if (analysis == "Gender Distribution") {
      if ("Unknown" %in% lusitania$Sex) {
        plotOutput("genderDistributionPlot")
      } else {
        HTML("<p style='text-align:center;font-size:16px;color:red;'>Gender distribution data is not available for the Lusitania dataset.</p>")
      }
    } else if (analysis == "Survival Rate by Gender and Class") {
      plotOutput("genderClassSurvivalPlot")
    } else if (analysis == "Survival Rate by Ticket Class") {
      plotOutput("ticketClassSurvivalPlot")
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
