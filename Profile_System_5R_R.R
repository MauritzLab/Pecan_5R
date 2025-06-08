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


#Height per Level
#L1 = 1.02 m
#L2 = 2.89 m
#L3 = 5.30 m
#L4 =8.16 m
#L5 = 11.4 m
#L6 = 15 m

#import data for roof test data
################################Import Data from Manual Download################################
# C:\Users\memauritz\OneDrive - University of Texas at El Paso\Pecan_Tornillo_5R\ECTower\ProfileSystem\Data\raw
# C:\Users\vmartinez62\OneDrive - University of Texas at El Paso\Pecan_Tornillo_5R\ECTower\ProfileSystem\Data\raw

#wd <- ("C:/Users/memauritz/OneDrive - University of Texas at El Paso/Pecan_Tornillo_5R/ECTower/ProfileSystem/Data/raw")
#setwd(wd)

#or 

# wd <- ("C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/Pecan_Tornillo_5R/ECTower/ProfileSystem/Data/raw")
# setwd(wd)
# 
# #set column names
# profile_colnames1 <- fread("TOA5_45644.IntAvg_2024_02_20_1530.dat",
#                            header = TRUE, skip=1,sep=",", fill=TRUE,
#                            na.strings=c(-9999,"#NAME?"))[1,]
# 
# 
# #set up data table 
# profile_dat1 <- fread("TOA5_45644.IntAvg_2024_02_20_1530.dat",
#                       header = FALSE, skip=4, sep=",", fill=TRUE,
#                       na.strings=c(-9999,"#NAME?"),
#                       col.names=colnames(profile_colnames1))
# 
# 
# #set up data table
# profile_dat2 <- fread("TOA5_45644.IntAvg_2024_02_20_1530.dat",
#                       header = FALSE, skip=4, sep=",", fill=TRUE,
#                       na.strings=c(-9999,"#NAME?"),
#                       col.names=colnames(profile_colnames1))
#
# # combine dat1, and dat2
#profile_dat <- rbind(profile_dat1, profile_dat2)
#
# # read in raw data to check diagnostics
# raw_colnames1 <- fread("TOA5_45644.RawData_2024_02_20_1501.dat",
#                        header = TRUE, skip=1,sep=",", fill=TRUE,
#                        na.strings=c(-9999,"#NAME?"))[1,]
# 
# raw_dat1 <- fread("TOA5_45644.RawData_2024_02_20_1501.dat",
#                   header = FALSE, skip=4, sep=",", fill=TRUE,
#                   na.strings=c(-9999,"#NAME?"),
#                   col.names=colnames(raw_colnames1))
# 
# raw_colnames <- fread("TOA5_45644.RawData_2024_02_20_1501.dat",
#                       header = TRUE, skip=1,sep=",", fill=TRUE,
#                       na.strings=c(-9999,"#NAME?"))[1,]
# 
# raw_dat <- fread("TOA5_45644.RawData_2024_02_20_1501.dat",
#                  header = FALSE, skip=4, sep=",", fill=TRUE,
#                  na.strings=c(-9999,"#NAME?"),
#                  col.names=colnames(raw_colnames1))
#################################################################################################################################

# #read data from Automatic loggernet download located in CZO_data
# wde <- ("C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/CZO_Data/Pecan5R/CR1000X-Profile/L1/IntAvg")
#  setwd(wde)

#read data from Automatic loggernet download located in E: Drive
wde <- ("Y:/Pecan5R/CR1000X-Profile/L1/IntAvg")
setwd(wde)
 
# #extract column names and units for 2024
# profile_colnames <- fread("Pecan5R_CR1000X-Profile_IntAvg_L1_2024.csv",
#                             header = TRUE, skip=1,sep=",", fill=TRUE,
#                             na.strings=c(-9999,"#NAME?"))[1,]

#extract column names and units for 2025
profile_colnames <- fread("Pecan5R_CR1000X-Profile_IntAvg_L1_2025.csv",
                           header = TRUE, skip=1,sep=",", fill=TRUE,
                           na.strings=c(-9999,"#NAME?"))[1,]

#set up data table 
#data was split in 2024 in CZO_data
# profile_dat1 <- fread("Pecan5R_CR1000X-Profile_IntAvg_L1_2024_20241013_153000.csv",
#                       header = FALSE, skip=4, sep=",", fill=TRUE,
#                       na.strings=c(-9999,"#NAME?"),
#                       col.names=colnames(profile_colnames))
# 
# profile_dat2 <- fread("Pecan5R_CR1000X-Profile_IntAvg_L1_2024.csv",
#                        header = FALSE, skip=4, sep=",", fill=TRUE,
#                        na.strings=c(-9999,"#NAME?"),
#                        col.names=colnames(profile_colnames))

# #data for 2025
profile_dat <- fread("Pecan5R_CR1000X-Profile_IntAvg_L1_2025.csv",
                       header = FALSE, skip=4, sep=",", fill=TRUE,
                       na.strings=c(-9999,"#NAME?"),
                       col.names=colnames(profile_colnames))

# #combine 2024 data in CZO_data
# profile_dat <- rbind(profile_dat1, profile_dat2)

# Print first and last record in dataset
min(profile_dat$TIMESTAMP)
max(profile_dat$TIMESTAMP)

# read in raw data to check diagnostics

# raw_files <- list.files(path="C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/CZO_Data/Pecan5R/CR1000X-Profile/L1/RawData/2024",full.names=TRUE)
# raw_colnames <- fread(raw_files[1],
#                       header = TRUE, skip=1,sep=",", fill=TRUE,
#                       na.strings=c(-9999,"#NAME?"))[1,]
# raw_data <- do.call("rbind", lapply(raw_files, header = FALSE, fread, sep=",", dec=".",skip = 4, fill=TRUE, na.strings= c("-9999"), col.names=colnames(raw_colnames)))

#putting in long format for pump data
profile.long.pump <- profile_dat %>%
  select(TIMESTAMP, L1_NumSamples, L1_CO2, L1_H2O, L1_cell_tmpr, L1_cell_press, L1_sample_flow, 
          L2_NumSamples, L2_CO2, L2_H2O, L2_cell_tmpr, L2_cell_press, L2_sample_flow, 
          L3_NumSamples, L3_CO2, L3_H2O, L3_cell_tmpr, L3_cell_press, L3_sample_flow, 
          L4_NumSamples, L4_CO2, L4_H2O, L4_cell_tmpr, L4_cell_press, L4_sample_flow, 
          L5_NumSamples, L5_CO2, L5_H2O, L5_cell_tmpr, L5_cell_press, L5_sample_flow, 
          L6_NumSamples, L6_CO2, L6_H2O, L6_cell_tmpr, L6_cell_press, L6_sample_flow) %>%
   pivot_longer(!c(TIMESTAMP), names_to="sampleID",values_to="value")

#separate into pump level by using _
profile.long.pump <- profile.long.pump %>%
  separate_wider_delim(sampleID, "_", names = c("pump", "variable"), too_many = "merge")

#renaming columns then putting into long format for temp probe data
profile.long.temp <- profile_dat %>%
  select(TIMESTAMP, `T_air_Avg(1)`, `T_air_Avg(2)`, `T_air_Avg(3)`, `T_air_Avg(4)`, `T_air_Avg(5)`, `T_air_Avg(6)`)%>%
dplyr::rename(L1_Tair = `T_air_Avg(1)`,
         L2_Tair = `T_air_Avg(2)`,
         L3_Tair = `T_air_Avg(3)`,
         L4_Tair = `T_air_Avg(4)`,
         L5_Tair = `T_air_Avg(5)`,
         L6_Tair = `T_air_Avg(6)`)%>%
  pivot_longer(!c(TIMESTAMP), names_to="sampleID",values_to="value")

#separate into level by using _
profile.long.temp <- profile.long.temp %>%
  separate_wider_delim(sampleID, "_", names = c("pump", "variable"))

#long format for system check data
profile.long.sys <- profile_dat %>%
  select(TIMESTAMP, diag_AP200_Avg, RECORD, pump_press_Avg, pump_control_Avg, pump_speed_Avg, PumpTmprOK_Avg, pump_tmpr_Avg, pump_heat_Avg, pump_fan_Avg, ValveTmprOK_Avg, valve_tmpr_Avg, valve_heat_Avg, valve_fan_Avg, intake_heat_Avg, batt_volt_Avg, BattVoltLOW_Avg, panel_tmpr_Avg) %>%
  pivot_longer(!c(TIMESTAMP), names_to="variable",values_to="value")

# show variables in pump data
levels(as.factor(profile.long.pump$variable))

#Summary graphs with only good data


#create a new data frame with only 0 diag values
diags <- profile_dat%>%
  select(TIMESTAMP, diag_AP200_Avg)

#filter for only 0 diag values
diags_0 <- profile_dat%>%
  select(TIMESTAMP, diag_AP200_Avg)%>%
  filter(diag_AP200_Avg == 0)

#show AP200 diag in binary format where 0 = good, > 0 = bad
diags <- diags%>%
  mutate(diag_AP200_bin = 
           case_when(diag_AP200_Avg == 0 ~ "good",
                     diag_AP200_Avg > 0 ~ "bad"))

#graph binary diag variable
diags%>%
  filter(!is.na(diag_AP200_bin))%>%
  ggplot(., aes(TIMESTAMP, diag_AP200_bin))+
  geom_point()

#graph binary diag table by hour
diags%>%
  filter(!is.na(diag_AP200_bin))%>%
  ggplot(., aes(TIMESTAMP, diag_AP200_bin))+
  geom_point()+
  facet_wrap(hour(TIMESTAMP)~.)

#binary diag table in bar graph
diags%>%
  filter(!is.na(diag_AP200_bin))%>%
  ggplot(., aes(diag_AP200_bin, fill = diag_AP200_bin))+
  geom_bar(width = 0.5)+
  facet_wrap(hour(TIMESTAMP)~.)


#join profile.long.pump df with diags & graph
diags.pump <- inner_join(profile.long.pump, diags_0,
                         by = NULL,
                         copy = FALSE,
                         suffix = c(".x", ".y"),
                         keep = NULL)


diags.pump %>% filter(variable %in% c("CO2", "H2O"))%>%
  ggplot(., aes(TIMESTAMP, value, color=pump))+
  geom_line()+
  facet_grid(variable~.,scales="free_y")+
  labs(title = "CO2 ppm/H2O ppt", x = "Timestamp", y = "Gas Concentration", color = "Level" )+
  theme_bw()+
  theme(text=element_text(size=18))
 
#join profile.long.temp df with diags & graph
diags.temp <- inner_join(profile.long.temp, diags_0,
                         by = NULL,
                         copy = FALSE,
                         suffix = c(".x", ".y"),
                         keep = NULL)

diags.temp %>% filter(variable %in% c("Tair"))%>%
  ggplot(., aes(TIMESTAMP, value, color=pump))+
  geom_line()+
  facet_grid(variable~.,scales="free_y")+
  labs(title = "Temperature")



#More detailed data


#graph cell temperature per pump data
#Temperature of IRGA cell on log scale to see full range of values
profile.long.pump %>% filter(variable %in% c("cell_tmpr"))%>%
  #filter(TIMESTAMP>as.Date("2025-02-11")&TIMESTAMP<as.Date("2025-02-16"))%>%
  ggplot(., aes(TIMESTAMP, log(value), color=pump))+
  geom_line()+
  facet_wrap(pump~.)+
  labs(title="IRGA cell temp")

#The value of cell_tmpr, which is measured by the IRGA, zoomed into expected values 
#The normal range for the sample cell temperature is 48 to 52 °C
profile.long.pump %>% filter(variable %in% c("cell_tmpr")&value<53&value>47)%>%
  ggplot(., aes(TIMESTAMP, value, color=pump))+
  geom_line()+
  facet_wrap(pump~.)+
  labs(title="IRGA cell temp")


#show of out of range cell temperatures
cell_tmpr_outofrange <- profile.long.pump %>% 
  filter(variable %in% c("cell_tmpr")&(value<48|value>52))%>%
  arrange(TIMESTAMP,pump)

#graph out of range cell temp by pump
cell_tmpr_outofrange %>% 
  ggplot(., aes(TIMESTAMP, value, color=pump))+
  geom_point()+
  facet_wrap(pump~.)+
  labs(title="out of range cell temp")

#graph out of range cell temp by hour
cell_tmpr_outofrange %>% 
  ggplot(., aes(TIMESTAMP, value, color=pump))+
  geom_point()+
  facet_wrap(hour(TIMESTAMP)~.)+
  labs(title="out of range cell temp")

#graph out of range cell temp by hour per day
# profile.long.sys %>% filter(variable %in% c("diag_AP200_Avg")&value<30)%>% 
#   ggplot(., aes(hour(TIMESTAMP), value))+
#   geom_point()+
#   facet_wrap(as.Date(TIMESTAMP)~.)+
#   labs(title="diag")


#graph cell pressure per pump data
# cell pressure of the IRGA

#graph cell temperature per pump data
#Temperature of cell pressure on log scale to see full range of values
profile.long.pump %>% filter(variable %in% c("cell_press"))%>%
  #filter(TIMESTAMP>as.Date("2025-02-11")&TIMESTAMP<as.Date("2025-02-16"))%>%
  ggplot(., aes(TIMESTAMP, log(value), color=pump))+
  geom_line()+
  facet_wrap(pump~.)+
  labs(title="cell pressure")

# This should be within ± 2 kPa of the pressure setpoint which was set to 54
# Compare cell_press (pressure measured by the IRGA) to pump_press
# (pressure measured at the pump inlet). These two points are physically 
# connected by a tube with relatively low flow, such that they should be at 
# similar pressures. The pressure values should agree within the combined 
# uncertainty of the respective pressure sensors.They should be within 4 kPa of each other
profile.long.pump %>% filter(variable %in% c("cell_press")&value<75)%>%
  ggplot(., aes(TIMESTAMP, value, color=pump))+
  geom_line()+
  geom_hline(yintercept = c(52,56), linetype = "dashed")+
  facet_wrap(pump~.)+
  labs(title="cell pressure")

#graph average daily pump pressure for each pump
profile.long.sys %>% filter(variable %in% c("pump_press_Avg")&value<75)%>%
  ggplot(., aes(TIMESTAMP, value))+
  geom_line()+
  labs(title="pump pressure")

#select out of range cell pressure
cell_pressure_outofrange <- profile.long.pump %>% 
  filter(variable %in% c("cell_press")&(value<52|value>56))%>%
  arrange(TIMESTAMP,pump)

#graph out of range cell pressure by pump
cell_pressure_outofrange %>% 
  ggplot(., aes(TIMESTAMP, value, color=pump))+
  geom_point()+
  facet_wrap(pump~.)+
  labs(title="out of range cell pressure")

#graph out of range cell pressure by hour
cell_pressure_outofrange %>% 
  ggplot(., aes(TIMESTAMP, value, color=pump))+
  geom_point()+
  facet_wrap(hour(TIMESTAMP)~.)+
  labs(title="out of range cell pressure")

#cell pressure to pump pressure comparison graphs
# graph cell_press and pump_press_Avg in the same figure
# profile.pressure.cell <- profile.long.pump %>% 
#   filter(variable %in% c("cell_press")&value<75)%>%
#   rename(cell_pressure = value)%>% 
#   select(TIMESTAMP, pump, cell_pressure)
# 
# 
# profile.pressure.pump <- profile.long.sys %>% 
#   filter(variable %in% c("pump_press_Avg")&value<75)%>%
#   rename(pump_pressure = value)%>% 
#   select(TIMESTAMP, pump_pressure)
#   
# 
# profile.pressure.all <- full_join(profile.pressure.cell, profile.pressure.pump)
# 
# profile.pressure.all%>%
#   filter(as.Date(TIMESTAMP)>as.Date("2024-02-28"))%>%
#   ggplot(., aes(pump_pressure, cell_pressure))+
#   geom_point()+
#   facet_wrap(pump~.)
# 
# #detailed daily cell_press and pump_press_Avg in same graph
# profile.pressure.all%>%
#   filter(as.Date(TIMESTAMP)>as.Date("2024-02-28"))%>%
#   ggplot(., aes(pump_pressure, cell_pressure))+
#   geom_point()+
#   geom_abline(intercept=0, slope=1)+
#   geom_abline(intercept=4, slope=1, linetype="dashed")+
#   geom_abline(intercept=-4, slope=1, linetype="dashed")+
#   facet_wrap(hour(TIMESTAMP)~.,scales = "free")

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

#graph air temperature per pump 
profile.long.temp%>%
  ggplot(., aes(TIMESTAMP, value, color=pump))+
  geom_line()+
  facet_wrap(pump~.)+
  labs(title="air temp")

# check CO2 concentration together with cell pressure 
ggplot()+
  geom_line(data=profile.long.pump%>%filter(variable %in% "cell_press"),aes(TIMESTAMP, value, color=pump))+
  geom_point(data=profile.long.pump%>%filter(variable %in% "CO2"),aes(TIMESTAMP, value, color="CO2"),size=0.5,color="black")+
  facet_wrap(pump~.)+
  labs(title="Cell Pressure and CO2 concentrations for each L")

# check CO2 concentration together with cell temperature 
ggplot()+
  geom_line(data=profile.long.pump%>%filter(variable %in% "cell_tmpr"),aes(TIMESTAMP, value, color=pump))+
  geom_point(data=profile.long.pump%>%filter(variable %in% "CO2"),aes(TIMESTAMP, value, color="CO2"),size=0.5,color="black")+
  facet_wrap(pump~.)+
  labs(title="Cell Temperature and CO2 concentrations for each L")


# plot 1:1 of cell pressure and CO2
plot(profile.long.pump[profile.long.pump$variable=="cell_press",]$value,profile.long.pump[profile.long.pump$variable=="CO2",]$value,
     xlab="Cell Pressure",ylab="CO2 concentration")+
  abline(v=c(52,56),h=c(400,650))

# plot 1:1 of cell pressure and temperature
plot(profile.long.pump[profile.long.pump$variable=="cell_press",]$value,profile.long.pump[profile.long.pump$variable=="cell_tmpr",]$value,
     xlab="Cell Pressure",ylab="Cell Temperature")+
  abline(v=c(52,56),h=c(48,52))

# plot 1:1 of cell temperature and CO2
plot(profile.long.pump[profile.long.pump$variable=="cell_tmpr",]$value,profile.long.pump[profile.long.pump$variable=="CO2",]$value,
     xlab="Cell Temperature",ylab="CO2 concentration")+
  abline(v=c(48,52),h=c(400,650))


# graph AP200 diag for system
# should be 0 if there are no diagnostic problems
# every sample of diag_AP200 is stored in the RawData output table and is preferred o diagnose problems.
# Only an average on diag_AP200 is stored in the output tables: IntAvg, CalAvg, and SiteAvg.
profile.long.sys %>%
  filter(variable %in% c("diag_AP200_Avg")) %>%
  ggplot(., aes(TIMESTAMP, value,color=factor(value)))+
  geom_point()+
  geom_hline(yintercept = 0)+
  labs(title="AP200 diag")
# 
# #graph average diag value by hour
profile.long.sys %>%
  filter(variable %in% c("diag_AP200_Avg")) %>%
  ggplot(., aes(TIMESTAMP, value,color=factor(value)))+
  geom_point()+
  geom_hline(yintercept = 0)+
  labs(title="AP200 diag")+
  facet_wrap(hour(TIMESTAMP)~.)
# 
# #zoom in on low diag values
# profile.long.sys %>%
#   filter(variable %in% c("diag_AP200_Avg")&value<15) %>%
#   ggplot(., aes(hour(TIMESTAMP), value,color=factor(value)))+
#   geom_point()+
#   geom_hline(yintercept = 0)+
#   labs(title="AP200 diag")+
#   facet_wrap(date(TIMESTAMP)~.)
# 
# #zoom in on low diag values with diag = 0 as black points and diag>0 colored
# 
ggplot(profile.long.sys)+
  geom_point(aes(x=hour(TIMESTAMP),y=value),color="black",data=subset(profile.long.sys,variable %in% c("diag_AP200_Avg")&value==0))+
  geom_point(aes(x=hour(TIMESTAMP),y=value,color=factor(value)),data=subset(profile.long.sys,variable %in% c("diag_AP200_Avg")&value>0&value<15))+
  geom_hline(yintercept = 0)+
  labs(title="AP200 diag")+
  facet_wrap(date(TIMESTAMP)~.)

# 
# #graph diag value by hour and each date
# profile.long.sys %>%
#   filter(variable %in% c("diag_AP200_Avg")) %>%
#   ggplot(., aes(hour(TIMESTAMP), value,color=factor(value)))+
#   geom_point()+
#   geom_hline(yintercept = 0)+
#   labs(title="AP200 diag")+
#   facet_wrap(date(TIMESTAMP)~.)
# 
# raw_data%>%
#   filter(as.Date(TIMESTAMP)>as.Date("2024-04-12")&as.Date(TIMESTAMP)<as.Date("2024-04-16"))%>%
#   ggplot(., aes(TIMESTAMP, diag_AP200, color=factor(valve_number)))+
#   geom_point()+
#   labs(title="AP200 diag")+
#   facet_wrap(valve_number~.)

#graph for system pump check data
# if pump control is = 0, the AP200 has turned the pump off
# The pump module has a fan that turns on if pump_tmpr rises above 45 °C and records a fraction of the time pump is on. 
# The fan will stay on until the pump temperature falls below 40 °C.
# pump heat turns on if if pump temp falls below 2 °C
# pump pressure should not disagree more than 4 kPa with cell pressure
# pump speed should not oscillate
# A value of 1 on pumptmprOK indicates no pump temperature problem at any time during the averaging period. 
# A value of 0 indicates a pump temperature problem during the entire time.
profile.long.sys %>% filter(variable %in% c("pump_press_Avg", "pump_control_Avg", "pump_speed_Avg", "PumpTmprOK_Avg", "pump_tmpr_Avg", "pump_heat_Avg", "pump_fan_Avg"))%>%
  ggplot(., aes(TIMESTAMP, value))+
  geom_line()+
  facet_grid(variable~.,scales="free_y")

#graph for system valve check data
# valve manifold temp operating range is 4-49 °C, outside temp range will disable the valves and pump.
# valve_heat_Avg should only increase to 0.5 in the event the temp falls below 5 °C to heat up the valve module.
# valve fan turns on if the temp rises above 45 °C amd will stay on until it falls below 43 °C,
# the fraction of time the valve fan is on is what is reported in the output tables.
# ValveTmprOK_Avg value of 1 indicates noproblem at any time during he averaging period, 0 indicates a problem the entire time.
profile.long.sys %>% filter(variable %in% c("ValveTmprOK_Avg", "valve_tmpr_Avg", "valve_heat_Avg", "valve_fan_Avg", "intake_heat_Avg"))%>%
  ggplot(., aes(TIMESTAMP, value))+
  geom_line()+
  facet_grid(variable~.,scales="free_y")

#graph for battery & panel temp check
# The AP200 supply voltage must be 10.0 Vdc to 16.0 Vdc
# temp of datalogger wiring panel
# red intercept line indicates battery added
profile.long.sys %>% filter(variable %in% c("batt_volt_Avg", "BattVoltLOW_Avg", "panel_tmpr_Avg"))%>%
  ggplot(., aes(TIMESTAMP, value))+
  geom_line()+
  geom_vline(xintercept=ymd_hms("2024-04-09 16:00:00"), color="red")+
  facet_grid(variable~.,scales="free_y")

# graph for hourly battery voltage and panel temp readings per day
profile.long.sys %>% filter(variable %in% c("batt_volt_Avg", "BattVoltLOW_Avg", "panel_tmpr_Avg"))%>%
  ggplot(., aes(hour(TIMESTAMP), value, color=factor(as.Date(TIMESTAMP))))+
  geom_line()+
  facet_grid(variable~.,scales="free_y")+
  theme(legend.position="none")


# add column to profile.long.sys to show when LI-850 cell temp and pump pressure are out of range
profile.long.sys <- profile.long.sys %>%
  mutate(cell.temp.out = case_when(TIMESTAMP %in% cell_tmpr_outofrange$TIMESTAMP ~ "OUT",
                                   .default = "IN"),
         cell.press.out = case_when(TIMESTAMP %in% cell_pressure_outofrange$TIMESTAMP ~ "OUT",
                                   .default = "IN"))


# graph battery voltage showing overlap with cell temp or cell pressure out of range
profile.long.sys %>% filter(variable %in% c("batt_volt_Avg", "BattVoltLOW_Avg", "panel_tmpr_Avg"))%>%
  #filter(TIMESTAMP>as.Date("2025-02-11")&TIMESTAMP<as.Date("2025-02-16"))%>%
       ggplot(., aes(TIMESTAMP, value,color=cell.temp.out))+
       geom_point()+
       #geom_vline(xintercept=ymd_hms("2024-04-09 16:00:00"), color="red")+
       facet_grid(variable~.,scales="free_y")+
  scale_color_manual(values=c("IN"="green","OUT"="red"))+
  labs(title="Battery and Panel Temp when LI-850 Cell Temp is in/out of range")


profile.long.sys %>% filter(variable %in% c("batt_volt_Avg", "BattVoltLOW_Avg", "panel_tmpr_Avg"))%>%
  #filter(TIMESTAMP>as.Date("2025-02-11")&TIMESTAMP<as.Date("2025-02-16"))%>%
  ggplot(., aes(TIMESTAMP, value,color=cell.press.out))+
  geom_point()+
  #geom_vline(xintercept=ymd_hms("2024-04-09 16:00:00"), color="red")+
  facet_grid(variable~.,scales="free_y")+
  scale_color_manual(values=c("IN"="green","OUT"="red"))+
  labs(title="Battery and Panel Temp when LI-850 Cell Pressure is in/out of range")

