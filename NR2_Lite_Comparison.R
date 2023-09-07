#Create Script to Compare NR2 Data

#Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)

#Set Working directory
setwd("C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/Pecan_Tornillo_5R/ECTower/Maintenance/NR2_Lite_CalTest/")

#Import file from datalogger
data.head <- read.table("CR3000_Table1.dat", sep = ",", skip = 1, header = TRUE)[1,]
data <- read.table("CR3000_Table1.dat", sep = ",", skip = 4, header = FALSE, col.names = colnames(data.head))

#Format timestamp
data <- data%>%
  mutate(datetime = ymd_hms(TIMESTAMP))

# change data to long format
data.long <- data %>%
  pivot_longer(!c(TIMESTAMP,RECORD), names_to="variable",values_to="value") %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP),
         date=as.Date(TIMESTAMP))