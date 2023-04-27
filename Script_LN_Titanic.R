#Dataset Titanic https://www.kaggle.com/c/titanic/data
#Dataset RMS Lusitania Complete Passanger Manifest

#loading packages
library(dplyr)
library(tidyr)
library(ggplot2)

#Create URL for dataset
url_Titanic <- "https://raw.githubusercontent.com/IamMancini/rstudio-titanic/main/train.csv?token=GHSAT0AAAAAACBE3PV2QOQJFXOEROJJC2TSZBRKWLQ"
url_RMS_Lusitania <- "https://raw.githubusercontent.com/IamMancini/rstudio-titanic/main/LusitaniaManifest.csv?token=GHSAT0AAAAAACBE3PV3K5V7XFZRNFSKS6QMZB2HHAQ"

#load Dataset
titanic <- read.csv(url_Titanic)
rms_Lusitania <- read.csv(url_RMS_Lusitania)

#Testtt

#Show first rows of dataset Titanic to check
head(titanic)

#Show first rows of dataset Lusitania to check
head(rms_Lusitania)

#summary of dataset Titanic
summary(titanic)

#summary of dataset Lusitania
summary(rms_Lusitania)

#Start Data-Cleaning:





