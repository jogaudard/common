library(tidyverse)
library(lubridate)

soil.moist <- function(rawsoilmoist, soil_temp, soilclass){
  #based on appendix A of Wild 2019 (https://www-sciencedirect-com.pva.uib.no/science/article/pii/S0168192318304118#sec0095)
  #creating df with parameters for each soil type
  soilclass.df <- tibble(
    soil = c("sand", "loamy_sand_A", "loamy_sand_B", "sandy_loam_A", "sandy_loam_B", "loam", "silt_loam", "peat"),
    a = c(-3E-9, -1.9e-8, -2.3e-8, -3.8e-8, -9e-10, -5.1e-8, 1.7e-8, 1.23e-7),
    b = c(1.61e-4, 2.66e-4, 2.82e-4, 3.39e-4, 2.62e-4, 3.98e-4, 1.18e-4, -1.45e-4),
    c = c(-0.11, -0.154, -0.167, -0.215, -0.159, -0.291, -0.101, 0.203)
      )
  
  #filtering soilclass.df based on which soilclass was entered in the function
  soilclass.df <- soilclass.df %>% 
    filter(
      soil == soilclass
    )
  
  #calculating the volumetric soil moisture with the parameters corresponding to the soil class and the raw soil moisture from the logger
  volmoist = (soilclass.df$a * rawsoilmoist^2) + (soilclass.df$b * rawsoilmoist) + soilclass.df$c
  
  #temperature correction
  temp_ref <- 24
  delta_air <- 1.91132689118083
  delta_water <- 0.64108
  #this part needs more work as I don't really understand what is supposed to happen
  temp_corr = rawsoilmoist + ((temp_ref-soil_temp) * (delta_air + ((delta_water-delta_air) * volmoist)))
  # return(volmoistcorr)
  return(volmoist) #let's just use the soil moisture without temperature correction for now
}

# testing the function

data <- read_delim("tomst/data_ExampleTMS3.csv", delim = ";", col_names = c("index", "date", "time_zone", "T1", "T2", "T3", "raw_soilmoist", "shake", "errFlag")) %>% 
  mutate(
    date = dmy_hm(date)
  )

tomst_test <- data %>% 
  mutate(
    soil_moist_nocorr = soil.moist(raw_soilmoist, T1, "peat")
  )

test_tempcorr <- read_csv("tomst/tomst_tempcorr_peat.csv") %>% 
  rename(
    "date" = "Date & time"
  ) %>% 
  mutate(
    date = mdy_hm(date)
  ) %>% 
  rename(soil_moist_corr = "Vol. moisture") %>% 
  drop_na(date)

#graph the difference between temp correction and no temp correction
joined_soilmoist <- left_join(test_tempcorr, tomst_test, by = c("date")) %>% 
  pivot_longer(cols = c(soil_moist_nocorr, soil_moist_corr), names_to = "correction", values_to = "soil_moist")

ggplot(joined_soilmoist, aes(x = date, y = soil_moist)) +
  geom_line(size = 0.2, aes(color = correction))

  
  


