# This code is for checking the data from the AP200 profile system
# Written by Marguerite Mauritz and Victoria Martinez
# Created February 19, 2024

library(data.table)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(shiny)
library(flexdashboard)
library(data.table)

#import data
wd <- ("C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/Pecan_Tornillo_5R/ECTower/ProfileSystem/Data/RoofTest")
setwd(wd)

#trying to add new files?
#AP200.data <- rbindlist(lapply(list.files(path = wd, pattern = "IntAvg"), fread),fill=TRUE)

#set column names
profile_colnames1 <- fread("TOA5_45644.IntAvg_2023_12_15_1430.dat",
                       header = TRUE, skip=1,sep=",", fill=TRUE,
                       na.strings=c(-9999,"#NAME?"))[1,]
    

#set up data table
profile_dat1 <- fread("TOA5_45644.IntAvg_2023_12_15_1430.dat",
              header = FALSE, skip=4, sep=",", fill=TRUE,
              na.strings=c(-9999,"#NAME?"),
              col.names=colnames(profile_colnames1))


#putting in long format for pump data
profile.long.pump <- profile_dat1 %>%
  select(TIMESTAMP, L1_NumSamples, L1_CO2, L1_H2O, L1_cell_tmpr, L1_sample_flow, L2_NumSamples, L2_CO2, L2_H2O, L2_cell_tmpr, L2_cell_press, L3_sample_flow, L3_NumSamples, L3_CO2, L3_H2O, L3_cell_tmpr, L3_cell_press, L3_sample_flow, L4_NumSamples, L4_CO2, L4_H2O, L4_cell_tmpr, L4_cell_press, L4_sample_flow, L5_NumSamples, L5_CO2, L5_H2O, L5_cell_tmpr, L5_cell_press, L5_sample_flow, L6_NumSamples, L6_CO2, L6_H2O, L6_cell_tmpr, L6_cell_press, L6_sample_flow) %>%
  pivot_longer(!c(TIMESTAMP), names_to="sampleID",values_to="value")

#seperate into pump level by using _
profile.long.pump <- profile.long.pump %>%
separate_wider_delim(sampleID, "_", names = c("pump", "variable"), too_many = "merge")
   
#renaming columns then putting into long format for temp probe data
profile.long.temp <- profile_dat1 %>%
select(TIMESTAMP, `T_air_Avg(1)`, `T_air_Avg(2)`, `T_air_Avg(3)`, `T_air_Avg(4)`, `T_air_Avg(5)`, `T_air_Avg(6)`) %>%
         rename(L1_Tair = `T_air_Avg(1)`,
         L2_Tair = `T_air_Avg(2)`,
         L3_Tair = `T_air_Avg(3)`,
         L4_Tair = `T_air_Avg(4)`,
         L5_Tair = `T_air_Avg(5)`,
         L6_Tair = `T_air_Avg(6)`)%>%
pivot_longer(!c(TIMESTAMP), names_to="sampleID",values_to="value")

#seperate into level by using _
profile.long.temp <- profile.long.temp %>%
separate_wider_delim(sampleID, "_", names = c("pump", "variable"))
 

#long format for system check data
profile.long.sys <- profile_dat1 %>%
select(TIMESTAMP, RECORD, pump_press_Avg, pump_control_Avg, pump_speed_Avg, PumpTmprOK_Avg, pump_tmpr_Avg, pump_heat_Avg, pump_fan_Avg, ValveTmprOK_Avg, valve_tmpr_Avg, valve_heat_Avg, valve_fan_Avg, intake_heat_Avg, batt_volt_Avg, BattVoltLOW_Avg, panel_tmpr_Avg) %>%
  pivot_longer(!c(TIMESTAMP), names_to="sampleID",values_to="value")


#graph average CO2 per pump data
profile.long.pump %>% filter(variable %in% c("CO2"))%>%
ggplot(., aes(TIMESTAMP, value, color=pump))+
geom_line()

#graph average H2O per pump data
profile.long.pump %>% filter(variable %in% c("H2O"))%>%
ggplot(., aes(TIMESTAMP, value, color=pump))+
geom_line()

#graph temperaure per level of pump 
profile.long.temp%>%
ggplot(., aes(TIMESTAMP, value, color=pump))+
geom_line()

#graph for system pump check data
profile.long.sys %>% filter(sampleID %in% c("pump_press_Avg", "pump_control_Avg", "pump_speed_Avg", "PumpTmprOK_Avg", "pump_tmpr_Avg", "pump_heat_Avg", "pump_fan_Avg"))%>%
ggplot(., aes(TIMESTAMP, value))+
geom_line()+
facet_grid(sampleID~.,scales="free_y")

#graph for system valve check data
profile.long.sys %>% filter(sampleID %in% c("ValveTmprOK_Avg", "valve_tmpr_Avg", "valve_heat_Avg", "valve_fan_Avg", "intake_heat_Avg"))%>%
ggplot(., aes(TIMESTAMP, value))+
geom_line()+
facet_grid(sampleID~.,scales="free_y")

#graph for battery & panel temp check
profile.long.sys %>% filter(sampleID %in% c("batt_volt_Avg", "BattVoltLOW_Avg", "panel_tmpr_Avg"))%>%
ggplot(., aes(TIMESTAMP, value))+
geom_line()+
facet_grid(sampleID~.,scales="free_y")
              

