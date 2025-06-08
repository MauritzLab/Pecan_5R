# This code is for checking the data from the AP200 profile system compared to Biomet data 
# Written by Marguerite Mauritz and Victoria Martinez
# Created September 16, 2024

library(data.table)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(shiny)
library(flexdashboard)
library(readxl)
library(tidyxl)
library(patchwork)


#read data from Automatic loggernet download located in E: Drive
#wde <- ("Y:/Pecan5R/CR1000X-Profile/L1/IntAvg")
wde <- ("C:/Users/vmartinez62/Downloads")
setwd(wde)

#extract column names and units for profile system data
profile_colnames <- fread("Pecan5R_CR1000X-Profile_IntAvg_L1_2024.csv",
                          header = TRUE, skip=1,sep=",", fill=TRUE,
                          na.strings=c(-9999,"#NAME?"))[1,]


#set up data table 
profile_dat <- fread("Pecan5R_CR1000X-Profile_IntAvg_L1_2024.csv",
                     header = FALSE, skip=4, sep=",", fill=TRUE,
                     na.strings=c(-9999,"#NAME?"),
                     col.names=colnames(profile_colnames))


#read in VPD, Temp, and RH from Above Canopy data
wde <- ("C:/Users/vmartinez62/OneDrive - University of Texas at El Paso/Pecan_Tornillo_5R/ECTower/ProfileSystem/Data/VPDAnalysis/")
setwd(wde)

#extract data and setup with correct column names 
AC_colnames <- read_excel("AboveCanopy_Meteo_24_VPD.xlsx", sheet = 1, col_names = TRUE, na = c("-9999", "NAN"))[1,]
AC <- read_excel("AboveCanopy_Meteo_24_VPD.xlsx", sheet = 1, col_names = FALSE, skip = 2, na = c("-9999", "NAN")) %>%
  rename_with(~ colnames(AC_colnames))

# Here are the scripts definitions:
# TA_1_1_1: Air temperature from EC100 107 temperature probe
# RH_1_1_1:Calculated from 107 temperature, H2O, and pressure.
# TA_1_1_2: Air temperature calculated from sonic temperature, water vapor density, and pressure
# RH_1_1_2:Calculated from sonic temperature, H2O, and pressure. 
# TA_1_1_3 and RH_1_1_3: Measured from temperature and humidity probe.

AC <- AC%>%
  rename(TA_1 = TA_1_1_1, RH_1 = RH_1_1_1, TDP_1 = T_DP_1_1_1, esatAMB = e_sat_amb, TA_2 = TA_1_1_2, RH_2 = RH_1_1_2,
         TDP_2 = T_DP_1_1_2, esat = e_sat, TA_3 = TA_1_1_3, RH_3 = RH_1_1_3, TDP_3 = T_DP_1_1_3, eprobe = e_probe, esatPROBE = e_sat_probe,
         WSMAX = WS_MAX)

#R wants to read these columns as characters, so change to numeric
AC <- AC %>%
  mutate(esatAMB = as.numeric(esatAMB),
         esat = as.numeric(esat),
         VPD = as.numeric(VPD))

#set up VPD data table in long format for variables without "_" 
AC.long.VPD <- AC %>%
  select(TIMESTAMP, esatAMB, esat, eprobe, esatPROBE, PA, VPD, WD, WSMAX, 
         P) %>%
  pivot_longer(!c(TIMESTAMP), names_to="variable",values_to="value")

#set up VPD data table in long format for variables with "level"_" 
AC.long <- AC %>%
select(TIMESTAMP, TA_1, RH_1, TDP_1, TA_2,RH_2, TDP_2, TA_3, RH_3, TDP_3) %>%
pivot_longer(!c(TIMESTAMP), names_to="variable",values_to="value")

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

# show variables in pump data
levels(as.factor(profile.long.pump$variable))

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

# Combine the datasets to show VPD, temp, and RH
#TA_1 and TA_2 have corresponding values so they will be used for temp
biomet.data <- bind_rows(
  AC.long.VPD %>% filter(variable == "VPD", variable %in% c("VPD")&(value<100)&(value>0))%>% mutate(type = "VPD"),
  AC.long %>% filter(variable == "TA_1")%>% mutate(type = "T"),
  AC.long %>% filter(variable == "TA_2")%>% mutate(type = "T"),
  AC.long %>% filter(variable == "TA_3")%>% mutate(type = "TA_3"),
  AC.long %>% filter (variable == "RH_1")%>% mutate(type = "RH"),
  profile.long.sys %>% filter(variable == "diag_AP200_Avg")%>% mutate(type = "diag_AP200"),
  profile.long.temp %>% filter(variable == "Tair")%>% mutate(type = "Tair"))%>%
  filter(TIMESTAMP >= as.Date("2024-02-20"))  

#Combined and individual graphs of the data as a whole
#create combined plot
biomet.plot <- ggplot(biomet.data, aes(x = TIMESTAMP, y = value, color = type)) +
  geom_point(data = biomet.data %>% filter(type == "diag_AP200")) + 
geom_line(data = biomet.data %>% filter(type == "VPD"))+
geom_line(data = biomet.data %>% filter(type == "T"))+
geom_line(data = biomet.data %>% filter(type == "RH"))

print(biomet.plot)

#compare AP200 diag values with RH
biomet.RH <- ggplot(biomet.data, aes(x = TIMESTAMP, y = value, color = type)) +
  geom_point(data = biomet.data %>% filter(type == "diag_AP200")) +
  geom_line(data = biomet.data %>% filter(type == "RH"))

print(biomet.RH)

#compare AP200 diag values with VPD
biomet.VPD <- ggplot(biomet.data, aes(x = TIMESTAMP, y = value, color = type)) +
  geom_point(data = biomet.data %>% filter(type == "diag_AP200")) +
  geom_line(data = biomet.data %>% filter(type == "VPD"))

print(biomet.VPD)

#Hourly data 
biomet.data %>% 
  ggplot(., aes(TIMESTAMP, value, color=type))+ 
  geom_point(data = biomet.data %>% filter(type == "diag_AP200"))+
  geom_line(data = biomet.data %>% filter(type == "VPD"))+
  geom_line(data = biomet.data %>% filter(type == "T"))+
  geom_line(data = biomet.data %>% filter(type == "RH"))+
  facet_wrap(hour(TIMESTAMP)~.)+
  labs(title="Hourly Combined")

#Hourly data diag AP200 and VPD 
biomet.data %>% 
  ggplot(., aes(TIMESTAMP, value, color=type))+ 
  geom_point(data = biomet.data %>% filter(type == "diag_AP200"))+
  geom_line(data = biomet.data %>% filter(type == "VPD"))+
  facet_wrap(hour(TIMESTAMP)~.)+
  labs(title="Hourly VPD")

#Hourly data diag AP200 and RH 
biomet.data %>% 
  ggplot(., aes(TIMESTAMP, value, color=type))+ 
  geom_point(data = biomet.data %>% filter(type == "diag_AP200"))+
  geom_line(data = biomet.data %>% filter(type == "RH"))+
  facet_wrap(hour(TIMESTAMP)~.)+
  labs(title="Hourly RH")


#Monthly data 
# degree C average hline added for scale reference to temp
biomet.data %>% 
  ggplot(., aes(TIMESTAMP, value, color=type))+ 
  geom_point(data = biomet.data %>% filter(type == "diag_AP200"))+
  geom_line(data = biomet.data %>% filter(type == "VPD"))+
  geom_line(data = biomet.data %>% filter(type == "T"))+
  geom_line(data = biomet.data %>% filter(type == "RH"))+
  geom_hline(yintercept=38)+
  facet_wrap(month(TIMESTAMP)~.,scales = "free")+
  labs(title="Monthly Combined")

#Monthly data diag AP200 and VPD 
biomet.data %>% 
  ggplot(., aes(TIMESTAMP, value, color=type))+ 
  geom_point(data = biomet.data %>% filter(type == "diag_AP200"))+
  geom_line(data = biomet.data %>% filter(type == "VPD"))+
  facet_wrap(month(TIMESTAMP)~.)+
  facet_wrap(month(TIMESTAMP)~.,scales = "free")+
  labs(title="Monthly VPD")

#Monthly data diag AP200 and RH 
biomet.data %>% 
  ggplot(., aes(TIMESTAMP, value, color=type))+ 
  geom_point(data = biomet.data %>% filter(type == "diag_AP200"))+
  geom_line(data = biomet.data %>% filter(type == "RH"))+
  facet_wrap(month(TIMESTAMP)~.)+
  facet_wrap(month(TIMESTAMP)~.,scales = "free")+
  labs(title="Monthly RH")

#Monthly data diag AP200 and T 
biomet.data %>% 
  ggplot(., aes(TIMESTAMP, value, color=type))+ 
  geom_line(data = biomet.data %>% filter(type == "T"))+
  facet_wrap(month(TIMESTAMP)~., scales = "free_x")+
  labs(title="Monthly T")


# AC variable sources:
# TA_1: Air temperature from EC100 107 temperature probe
# RH_1: Calculated from 107 temperature, H2O, and pressure.
# TA_2: Air temperature calculated from sonic temperature, water vapor density, and pressure
# RH_2: Calculated from sonic temperature, H2O, and pressure. 
# TA_3 and RH_3: Measured from temperature and humidity probe.


#TA value comparisons
ggplot(AC, aes(TA_1, TA_3))+
  geom_point()+
  geom_abline(intercept=0, slope=1)+
  labs(x = "EC100 107", y = "Temp/Humidity Probe")

ggplot(AC, aes(TA_1, TA_2))+
  geom_point()+
  geom_abline(intercept=0, slope=1)+
  labs(x = "EC100 107", y = "Sonic")

ggplot(AC, aes(TA_3, TA_2))+
  geom_point()+
  geom_abline(intercept=0, slope=1)+
  labs(x = "Temp/Humidity Probe", y = "Sonic")


#Combined temperatures from TA_1/TA-2 and profile system temp join datasets and graph
biomet.profile.temp <- bind_rows(biomet.data %>% filter(variable == "TA_1")%>% mutate(type = "EC100 107"),
                                 biomet.data %>% filter(variable == "TA_2")%>% mutate(type = "Sonic"),
                                 profile.long.temp %>% filter(variable == "Tair")%>% mutate(type = "Profile"),)%>%
  filter(TIMESTAMP >= as.Date("2024-02-20"))

biomet.profile.temp %>% 
  ggplot(., aes(TIMESTAMP, value, color=type))+
  geom_line()+
  labs(y = "degC", title="EC100 107, Sonic, and Profile Temp")

#Time series of TAs + Profile temp
temp.timeseries <-  ggplot(biomet.data, aes(TIMESTAMP, value, color=variable))+ 
  geom_line(data = biomet.data %>% filter(variable == "TA_1"))+
  geom_line(data = biomet.data %>% filter(variable == "TA_2"))+
  geom_line(data = biomet.data %>% filter(variable == "TA_3"))+
  geom_line(data = biomet.data %>% filter(variable == "Tair"))

print(temp.timeseries)
