#Dataset Titanic https://www.kaggle.com/c/titanic/data

#loading packages
library(dplyr)
library(tidyr)
library(ggplot2)

#Create URL for dataset
url <- "https://raw.githubusercontent.com/IamMancini/rstudio-titanic/main/train.csv?token=GHSAT0AAAAAACBE3PV2QOQJFXOEROJJC2TSZBRKWLQ"

#load Dataset
titanic <- read.csv(url)


#Show first rows of dataset to check
head(titanic)

#summary of dataset
summary(titanic)


