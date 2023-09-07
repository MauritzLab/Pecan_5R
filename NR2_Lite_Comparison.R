#Create Script to Compare NR2 Data

#Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

#Set Working directory
setwd("C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/Pecan_Tornillo_5R/ECTower/Maintenance/NR2_Lite_CalTest/")

#Import file from datalogger
data.head <- read.table("CR3000_Table1.dat", sep = ",", skip = 1, header = TRUE)[1,]
data <- read.table("CR3000_Table1.dat", sep = ",", skip = 4, header = FALSE, col.names = colnames(data.head))

#Format timestamp
data <- data%>%
  mutate(datetime = ymd_hms(TIMESTAMP))

#transform data to long format
data.long <- data%>%
  filter(datetime>ymd("2023-08-11"))%>%
  select(!c(TIMESTAMP, RECORD))%>%
  pivot_longer(!datetime, names_to = "sensor", values_to = "values")

#graph time series from wide format
ggplot(data.long, aes(x=datetime, y=values, color=sensor)) +
geom_point() +
geom_line() 

#graph sensors as scatter plot
ggplot(data, aes(x=NR_Wm2_Avg, y=NR_Wm2_2_Avg)) +
  geom_point() +
  geom_abline(slope=1, intercept=0)


