library(tidyverse)
source("https://raw.githubusercontent.com/jogaudard/common/datadoc/fun-fluxes.R")
# source("https://raw.githubusercontent.com/jogaudard/common/master/fun-fluxes.R")
library(lubridate, warn.conflicts = FALSE)
library(broom)
library(fs)
library("dataDownloader")

measurement <- 120 #the length of the measurement taken on the field in seconds
startcrop <- 0 #how much to crop at the beginning of the measurement in seconds
endcrop <- 0 #how much to crop at the end of the measurement in seconds

#download and unzip files from OSF
get_file(node = "pk4bg",
         file = "Three-D_cflux_2020.zip",
         path = "data/C-Flux/summer_2020",
         remote_path = "RawData/C-Flux") #getting raw files from the loggers with CO2 concentration, air temperature and PAR.
#Logger kept running in between INCLINE and Three-D, so the raw files are the same. There is no need to download the INCLINE raw files.

get_file(node = "pk4bg",
         file = "Three-D_light-response_2020.csv",
         path = "data/C-Flux/summer_2020",
         remote_path = "RawData/C-Flux") #light-response measurements for the Three-D plots

get_file(node = "zhk3m",
         file = "INCLINE_light-response_2020.csv",
         path = "data/C-Flux/summer_2020",
         remote_path = "RawData/C-Flux") #light-response measurements for the INCLINE plots

# Unzip files
zipFile <- "data/C-Flux/summer_2020/Three-D_cflux_2020.zip"
if(file.exists(zipFile)){
  outDir <- "data/C-Flux/summer_2020"
  unzip(zipFile, exdir = outDir)
}

location <- "data/C-Flux/summer_2020/rawData" #location of datafiles
#import all squirrel files and select date/time and CO2_calc columns, merge them, name it fluxes
fluxes <-
  dir_ls(location, regexp = "*CO2*") %>% 
  map_dfr(read_csv,  na = c("#N/A", "Over")) %>% 
  rename(CO2 = "CO2 (ppm)") %>%  #rename the column to get something more practical without space
  mutate(
    date = dmy(Date), #convert date in POSIXct
    datetime = as_datetime(paste(date, Time))  #paste date and time in one column
  ) %>%
  select(datetime,CO2)

#import date/time and PAR columns from PAR file
PAR <-
  list.files(path = location, pattern = "*PAR*", full.names = T) %>% 
  map_df(~read_table2(., "", na = c("NA"), col_names = paste0("V",seq_len(12)))) %>% #need that because logger is adding columns with useless information
  rename(date = V2, time = V3, PAR = V4) %>% 
  mutate(
    PAR = as.numeric(as.character(.$PAR)), #removing any text from the PAR column (again, the logger...)
    datetime = paste(date, time),
    datetime = ymd_hms(datetime)
  ) %>% 
  select(datetime, PAR)

#import date/time and value column from iButton file

temp_air <-dir_ls(location, regexp = "*temp*") %>% 
  map_dfr(read_csv,  na = c("#N/A"), skip = 20, col_names = c("datetime", "unit", "temp_value", "temp_dec"), col_types = "ccnn") %>%
  mutate(temp_dec = replace_na(temp_dec,0),
         temp_air = temp_value + temp_dec/1000, #because stupid iButtons use comma as delimiter AND as decimal point
         datetime = dmy_hms(datetime)
  ) %>% 
  select(datetime, temp_air)


#join the df


combined <- fluxes %>% 
  left_join(PAR, by = "datetime") %>% 
  left_join(temp_air, by = "datetime")

#import the record file


light_response_three_d <- read_csv("data/C-Flux/summer_2020/Three-D_light-response_2020.csv", na = c("")) %>% 
  rename(plot_ID = turf_ID) %>% 
  mutate(
    date = dmy(date),
    start = as_datetime(paste(date, starting_time)), #converting the date as posixct, pasting date and starting time together
    # Datetime = Start, #useful for left_join
    end = start + measurement - endcrop, #creating column End and cropping the end of the measurement
    start = start + startcrop #cropping the start
  ) %>%  
  select(plot_ID,type,replicate,starting_time,date,campaign,remarks,start,end)

light_response_incline <- read_csv("data/C-Flux/summer_2020/INCLINE_light-response_2020.csv", na = c("")) %>% 
  mutate(
    date = dmy(date),
    start = as_datetime(paste(date, starting_time)), #converting the date as posixct, pasting date and starting time together
    # Datetime = Start, #useful for left_join
    end = start + measurement - endcrop, #creating column End and cropping the end of the measurement
    start = start + startcrop #cropping the start
  ) %>%  
  select(plot_ID,type,replicate,starting_time,date,campaign,remarks,start,end)

# light-response <- bind_rows(light-response_incline, light-response_three_d) #to create one measurements record for the two projects

#matching fluxes

#ACHTUNG!! chambers are not the same size. Needs two different set of CO2 concentration
co2_light_response_three_d <- match.flux(combined, light_response_three_d)
co2_light_response_incline <- match.flux(combined, light_response_incline)

# Flux calculation
fluxes_light_response_three_d <- flux.calc(co2_light_response_three_d) %>% 
  rename(
    turf_ID = plot_ID
  ) %>% 
write_csv("Three-D_light-response_c-flux_2020.csv")

fluxes_light_response_incline <- flux.calc(co2_light_response_incline,
                                           chamber_volume = 34.3,
                                           plot_area = 0.08575) %>% 
write_csv("INCLINE_light-response_c-flux_2020.csv")



#graph CO2 fluxes to visually check the data



ggplot(co2_light_response_incline, aes(x=datetime, y=CO2)) + 
  # geom_point(size=0.005) +
  facet_wrap(vars(date), ncol = 1, scales = "free") +
  geom_line(size = 0.1, aes(group = ID)) +
  scale_x_datetime(date_breaks = "30 min") +
  ggtitle("INCLINE")
  # geom_line(size=0.05)

ggplot(co2_light_response_three_d, aes(x=datetime, y=CO2)) + 
  # geom_point(size=0.005) +
  facet_wrap(vars(date), ncol = 1, scales = "free") +
  geom_line(size = 0.1, aes(group = ID)) +
  scale_x_datetime(date_breaks = "30 min") +
  ggtitle("Three-D")
# geom_line(size=0.05)