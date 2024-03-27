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

library(data.table)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)


#import data
# C:\Users\memauritz\OneDrive - University of Texas at El Paso\Pecan_Tornillo_5R\ECTower\ProfileSystem\Data\raw
# C:\Users\vmartinez62\OneDrive - University of Texas at El Paso\Pecan_Tornillo_5R\ECTower\ProfileSystem\Data\raw

#wd <- ("C:/Users/memauritz/OneDrive - University of Texas at El Paso/Pecan_Tornillo_5R/ECTower/ProfileSystem/Data/raw")
#setwd(wd)

#or 

wd <- ("C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/Pecan_Tornillo_5R/ECTower/ProfileSystem/Data/raw")
setwd(wd)

#set column names
profile_colnames1 <- fread("TOA5_45644.IntAvg_2024_02_20_1530.dat",
                           header = TRUE, skip=1,sep=",", fill=TRUE,
                           na.strings=c(-9999,"#NAME?"))[1,]

# Roof Test Data

#set up data table 
profile_dat1 <- fread("TOA5_45644.IntAvg_2024_02_20_1530.dat",
                      header = FALSE, skip=4, sep=",", fill=TRUE,
                      na.strings=c(-9999,"#NAME?"),
                      col.names=colnames(profile_colnames1))


#set up data table
profile_dat2 <- fread("TOA5_45644.IntAvg_2024_02_20_1530.dat",
                      header = FALSE, skip=4, sep=",", fill=TRUE,
                      na.strings=c(-9999,"#NAME?"),
                      col.names=colnames(profile_colnames1))


# combine dat1, and dat2

profile_dat <- rbind(profile_dat1, profile_dat2)


# read in raw data to check diagnostics
raw_colnames1 <- fread("TOA5_45644.RawData_2024_02_20_1501.dat",
                       header = TRUE, skip=1,sep=",", fill=TRUE,
                       na.strings=c(-9999,"#NAME?"))[1,]

raw_dat1 <- fread("TOA5_45644.RawData_2024_02_20_1501.dat",
                  header = FALSE, skip=4, sep=",", fill=TRUE,
                  na.strings=c(-9999,"#NAME?"),
                  col.names=colnames(raw_colnames1))

#putting in long format for pump data
profile.long.pump <- profile_dat %>%
  select(TIMESTAMP, L1_NumSamples, L1_CO2, L1_H2O, L1_cell_tmpr, L1_cell_press, L1_sample_flow, L2_NumSamples, L2_CO2, L2_H2O, L2_cell_tmpr, L2_cell_press, L2_sample_flow, L3_sample_flow, L3_NumSamples, L3_CO2, L3_H2O, L3_cell_tmpr, L3_cell_press, L3_sample_flow, L4_NumSamples, L4_CO2, L4_H2O, L4_cell_tmpr, L4_cell_press, L4_sample_flow, L5_NumSamples, L5_CO2, L5_H2O, L5_cell_tmpr, L5_cell_press, L5_sample_flow, L6_NumSamples, L6_CO2, L6_H2O, L6_cell_tmpr, L6_cell_press, L6_sample_flow) %>%
  pivot_longer(!c(TIMESTAMP), names_to="sampleID",values_to="value")

#seperate into pump level by using _
profile.long.pump <- profile.long.pump %>%
  separate_wider_delim(sampleID, "_", names = c("pump", "variable"), too_many = "merge")

#renaming columns then putting into long format for temp probe data
profile.long.temp <- profile_dat %>%
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
profile.long.sys <- profile_dat %>%
  select(TIMESTAMP, diag_AP200_Avg, RECORD, pump_press_Avg, pump_control_Avg, pump_speed_Avg, PumpTmprOK_Avg, pump_tmpr_Avg, pump_heat_Avg, pump_fan_Avg, ValveTmprOK_Avg, valve_tmpr_Avg, valve_heat_Avg, valve_fan_Avg, intake_heat_Avg, batt_volt_Avg, BattVoltLOW_Avg, panel_tmpr_Avg) %>%
  pivot_longer(!c(TIMESTAMP), names_to="variable",values_to="value")


# show variables in pump data
levels(as.factor(profile.long.pump$variable))

#graph cell temperature per pump data
#Temperature of IRGA cell. 
#The value of cell_tmpr, which is measured by the IRGA. 
#The normal range for the sample cell temperature is 48 to 52 °C
profile.long.pump %>% filter(variable %in% c("cell_tmpr")&value<53&value>47)%>%
  ggplot(., aes(TIMESTAMP, value, color=pump))+
  geom_line()+
  facet_wrap(pump~.)+
  labs(title="cell temp")


#count of out of range cell temperatures
cell_tmpr_outofrange <- profile.long.pump %>% filter(variable %in% c("cell_tmpr")&(value<48|value>52))%>%
  arrange(TIMESTAMP,pump)
  

#graph cell pressure per pump data
# cell pressure of the IRGA
# This should be within ± 2 kPa of the pressure setpoint which was set to 54
# Compare cell_press (pressure measured by the IRGA) to pump_press
# (pressure measured at the pump inlet). These two points are physically 
# connected by a tube with relatively low flow, such that they should be at 
# similar pressures. The pressure values should agree within the combined 
# uncertainty of the respective pressure sensors.They shoud be within 4 kPa of each other
profile.long.pump %>% filter(variable %in% c("cell_press")&value<75)%>%
  ggplot(., aes(TIMESTAMP, value, color=pump))+
  geom_line()+
  facet_wrap(pump~.)+
  labs(title="cell pressure")

profile.long.sys %>% filter(variable %in% c("pump_press_Avg")&value<75)%>%
  ggplot(., aes(TIMESTAMP, value))+
  geom_line()+
  labs(title="pump pressure")

# graph cell_press and pump_press_Avg in the same figure

# graph Number of samples per pump
# for 6 levels should be 200 samples/30 mins
profile.long.pump %>% filter(variable %in% c("NumSamples"))%>%
  ggplot(., aes(TIMESTAMP, value, color=pump))+
  geom_line()+
  facet_wrap(pump~.)+
  labs(title="number of samples")

# graph sample flow for each pump
#  The normal expected range for the flow would be from ~200 to ~250 ml/min
#  As a general guideline, the filters should be replaced when the flow decreases by 25%. 
# The filters will normally last a few months, but will require more frequent changes in dirty conditions
profile.long.pump %>% filter(variable %in% c("sample_flow"))%>%
  ggplot(., aes(TIMESTAMP, value, color=pump))+
  geom_line()+
  geom_hline(yintercept = c(200,250))+
  facet_wrap(pump~.)+
  labs(title="sample flow")

#graph average CO2 per pump data in ppm
profile.long.pump %>% filter(variable %in% c("CO2"))%>%
  ggplot(., aes(TIMESTAMP, value, color=pump))+
  geom_line()+
  facet_wrap(pump~.)+
  labs(title="CO2")

#graph average H2O per pump data in ppt
profile.long.pump %>% filter(variable %in% c("H2O"))%>%
  ggplot(., aes(TIMESTAMP, value, color=pump))+
  geom_line()+
  facet_wrap(pump~.)+
  labs(title="H2O")

#graph air temperaure per level of pump 
profile.long.temp%>%
  ggplot(., aes(TIMESTAMP, value, color=pump))+
  geom_line()+
  facet_wrap(pump~.)+
  labs(title="air temp")

# graph AP200 diag for system
# should be 0 if there are no diagnostic problems
profile.long.sys %>%
  filter(variable %in% c("diag_AP200_Avg")) %>%
  ggplot(., aes(TIMESTAMP, value,color=factor(value)))+
  geom_point()+
  geom_hline(yintercept = 0)+
  labs(title="AP200 diag")

#zoom in on low diag values
profile.long.sys %>%
  filter(variable %in% c("diag_AP200_Avg")&value<15) %>%
  ggplot(., aes(hour(TIMESTAMP), value,color=factor(value)))+
  geom_point()+
  geom_hline(yintercept = 0)+
  labs(title="AP200 diag")+
  facet_wrap(date(TIMESTAMP)~.)

#graph diag value by hour
profile.long.sys %>%
  filter(variable %in% c("diag_AP200_Avg")) %>%
  ggplot(., aes(hour(TIMESTAMP), value,color=factor(value)))+
  geom_point()+
  geom_hline(yintercept = 0)+
  labs(title="AP200 diag")+
  facet_wrap(date(TIMESTAMP)~.)


#graph for system pump check data
# if pump control is = 0, the AP200 has turned the pump off
# The pump module has a fan that turns on if pump_tmpr rises above 45 °C. 
# The fan will stay on until the pump temperature falls below 40 °C.
# pump heat turns on if if pump temp falls below 2 °C
# pump pressure should not disagree more than 4 kPa with cell pressure
# pimp speed should not oscillate
# A value of 1 on pumptmprOK indicates no pump temperature problem at any time during the averaging period. 
# A value of 0 indicates a pump temperature problem during the entire time.
profile.long.sys %>% filter(variable %in% c("pump_press_Avg", "pump_control_Avg", "pump_speed_Avg", "PumpTmprOK_Avg", "pump_tmpr_Avg", "pump_heat_Avg", "pump_fan_Avg"))%>%
  ggplot(., aes(TIMESTAMP, value))+
  geom_line()+
  facet_grid(variable~.,scales="free_y")

#graph for system valve check data
profile.long.sys %>% filter(sampleID %in% c("ValveTmprOK_Avg", "valve_tmpr_Avg", "valve_heat_Avg", "valve_fan_Avg", "intake_heat_Avg"))%>%
  ggplot(., aes(TIMESTAMP, value))+
  geom_line()+
  facet_grid(sampleID~.,scales="free_y")

#graph for battery & panel temp check
# The AP200 supply voltage must be 10.0 Vdc to 16.0 Vdc
profile.long.sys %>% filter(variable %in% c("batt_volt_Avg", "BattVoltLOW_Avg", "panel_tmpr_Avg"))%>%
  ggplot(., aes(TIMESTAMP, value))+
  geom_line()+
  facet_grid(variable~.,scales="free_y")

