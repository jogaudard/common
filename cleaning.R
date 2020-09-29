library(tidyverse)
library(fuzzyjoin)

# measurement <- 120 #the length of the measurement taken on the field in seconds
# startcrop <- 20 #how much to crop at the beginning of the measurement in seconds
# endcrop <- 20 #how much to crop at the end of the measurement in seconds



#import flux data from the logger
# fluxes <- 
#   read_csv("BIO102_fluxes_raw.csv", na = c("#N/A"), col_types = "ctcnnnnn") %>%
#   rename(CO2 = "CO2 (ppm)", PAR = "PAR (umolsm2)", Temp_air = "Temp_air ('C)") %>%  #rename the columns to get something more practical without spaces
#   mutate(Date = strptime(as.character(.$Date), "%d.%m.%Y"), #convert date in strptime
#          Datetime = as.POSIXct(paste(Date, Time))) %>%  #paste date and time in one column
#   select(Datetime,CO2,PAR,Temp_air)
# 
# #import field data from the data sheet
# field_data <- read_csv("Field_data_BIO102.csv", na = c(""), col_types = "cnctDcn") %>% 
#   mutate(
#     Start = as.POSIXct(paste(Date, Starting_time), format="%Y-%m-%d %H:%M:%S"), #converting the date as posixct, pasting date and starting time together
#     End = Start + measurement - endcrop, #creating column End and cropping the end of the measurement
#     Start = Start + startcrop #cropping the start
#   ) %>%  
#   select(Site,Type,Replicate,Starting_time,Date,Remarks,Start,End)
# 

#extract from fluxes the data that are between "starting_time" and +2mn, associate Site, type, and replicate to each CO2 entry
#https://community.rstudio.com/t/tidy-way-to-range-join-tables-on-an-interval-of-dates/7881/2
co2conc <- fuzzy_left_join( #there is probably a better way to do that than fuzzy join, I need to find out. Fuzzy join is using too much memory
  fluxes, field_data,
  by = c(
    "Datetime" = "Start",
    "Datetime" = "End"
  ),
  match_fun = list(`>=`, `<=`)
) %>% 
  drop_na(Start) %>% 
  mutate(ID = group_indices(., Date, Site, Type, Replicate)) %>% #gives a unique ID to each flux
  select(Datetime, CO2, PAR, Temp_air, Site, Type, Replicate, ID, Remarks, Date)

# write_csv(co2conc_bio102, "co2conc_bio102.csv")
# 
# #graph CO2 fluxes to visually check the data
# 
# 
ggplot(co2conc, aes(x=Datetime, y=CO2)) +
  # geom_point(size=0.005) +
  geom_line(size = 0.1, aes(group = ID)) +
  coord_fixed(ratio = 10) +
  scale_x_datetime(date_breaks = "30 min") +
  # geom_line(size=0.05)
  ggsave("bio102.png", height = 20, width = 40, units = "cm")