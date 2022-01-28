# functions I use to clean and treat my data from carbon fluxes measurements

# match flux will attribute CO2 concentration to each flux measurements
# raw_flux is the continuous measurement from the logger
# field_record is the manually written record of what plot was measured when

match.flux <- function(raw_flux, field_record){
 co2conc <- full_join(raw_flux, field_record, by = c("datetime" = "start"), keep = TRUE) %>% #joining both dataset in one
    fill(PAR,temp_air,plot_ID,type,replicate,campaign,starting_time,date,start,end) %>% #filling all rows (except Remarks) with data from above
    group_by(date, plot_ID, type, replicate) %>% #this part is to fill Remarks while keeping the NA (some fluxes have no remark)
    fill(remarks) %>% 
    ungroup() %>% 
    mutate(ID = group_indices(., date, plot_ID, type, replicate)) %>% #assigning a unique ID to each flux, useful for plotting uzw
    filter(
      datetime <= end
      & datetime >= start) %>% #cropping the part of the flux that is after the End and before the Start
    select(datetime, CO2, PAR, temp_air, plot_ID, type, replicate, campaign, ID, remarks, date)
  
  
  return(co2conc)
}

match.flux2 <- function(raw_flux, field_record){

  #some measurements were used both for LRC and flux measurements because lack of time or battery on the field
  #that means we need to pick the CO2 concentration twice...
  field_record_LRC <- filter(field_record, campaign == "LRC")
  field_record_fluxes <- filter(field_record, campaign != "LRC")
  
  co2conc_LRC <- full_join(raw_flux, field_record_LRC, by = c("datetime" = "start"), keep = TRUE) %>% #joining both dataset in one
    fill(PAR,temp_air,plot_ID,type,replicate,campaign,starting_time,date,start,end,start_window, end_window) %>% #filling all rows (except Remarks) with data from above
    group_by(date, plot_ID, type, replicate) %>% #this part is to fill Remarks while keeping the NA (some fluxes have no remark)
    fill(remarks) %>% 
    # mutate(ID = cur_group_id()) %>% #assigning a unique ID to each flux, useful for plotting uzw
    ungroup() %>% 
    filter(
      datetime <= end
      & datetime >= start) #%>% #cropping the part of the flux that is after the End and before the Start
  # select(datetime, CO2, PAR, temp_air, plot_ID, type, replicate, campaign, ID, remarks, date)
  co2conc_fluxes <- full_join(raw_flux, field_record_fluxes, by = c("datetime" = "start"), keep = TRUE) %>% #joining both dataset in one
    fill(PAR,temp_air,plot_ID,type,replicate,campaign,starting_time,date,start,end,start_window, end_window) %>% #filling all rows (except Remarks) with data from above
    group_by(date, plot_ID, type, replicate) %>% #this part is to fill Remarks while keeping the NA (some fluxes have no remark)
    fill(remarks) %>% 
    # mutate(ID = cur_group_id()) %>% #assigning a unique ID to each flux, useful for plotting uzw
    ungroup() %>% 
    filter(
      datetime <= end
      & datetime >= start) #%>% #cropping the part of the flux that is after the End and before the Start
  
  co2conc <- full_join(co2conc_fluxes, co2conc_LRC) %>% 
    group_by(date, plot_ID, type, replicate) %>% 
    mutate(ID = cur_group_id()) %>% 
    ungroup()
  
  
  return(co2conc)
}

match.flux3 <- function(raw_flux, field_record){
  #some measurements were used both for LRC and flux measurements because lack of time or battery on the field
  #that means we need to pick the CO2 concentration twice...
  field_record_LRC <- filter(field_record,
                             type == "LRC1"
                              | type == "LRC2"
                              | type == "LRC3" 
                              | type == "LRC4" 
                              | type == "LRC5"
                            )
  field_record_fluxes <- filter(field_record,
                                type == "ER"
                                | type == "NEE"
                                )
  
  co2conc_LRC <- full_join(raw_flux, field_record_LRC, by = c("datetime" = "start"), keep = TRUE) %>% #joining both dataset in one
    fill(PAR,temp_air,turfID,type,campaign,starting_time,date,start,end,start_window, end_window) %>% #filling all rows (except Remarks) with data from above
    group_by(date, turfID, type) %>% #this part is to fill Remarks while keeping the NA (some fluxes have no remark)
    fill(comments) %>% 
    # mutate(ID = cur_group_id()) %>% #assigning a unique ID to each flux, useful for plotting uzw
    ungroup() %>% 
    filter(
      datetime <= end
      & datetime >= start) #%>% #cropping the part of the flux that is after the End and before the Start
  # select(datetime, CO2, PAR, temp_air, plot_ID, type, replicate, campaign, ID, remarks, date)
  co2conc_fluxes <- full_join(raw_flux, field_record_fluxes, by = c("datetime" = "start"), keep = TRUE) %>% #joining both dataset in one
    fill(PAR,temp_air,turfID,type,campaign,starting_time,date,start,end,start_window, end_window) %>% #filling all rows (except Remarks) with data from above
    group_by(date, turfID, type) %>% #this part is to fill Remarks while keeping the NA (some fluxes have no remark)
    fill(comments) %>% 
    # mutate(ID = cur_group_id()) %>% #assigning a unique ID to each flux, useful for plotting uzw
    ungroup() %>% 
    filter(
      datetime <= end
      & datetime >= start) #%>% #cropping the part of the flux that is after the End and before the Start
  
  co2conc <- full_join(co2conc_fluxes, co2conc_LRC) %>% 
    group_by(date, plot_ID, type) %>% 
    mutate(ID = cur_group_id()) %>% 
    ungroup()
  
  
  return(co2conc)
}

# flux.calc is to calculate fluxes based on a dataset with CO2 concentration vs time
# it requires broom
flux.calc <- function(co2conc, # dataset of CO2 concentration versus time (output of match.flux)
                      chamber_volume = 24.5, # volume of the flux chamber in L, default for Three-D chamber (25x24.5x40cm)
                      tube_volume = 0.075, # volume of the tubing in L, default for summer 2020 setup
                      atm_pressure = 1, # atmoshperic pressure, assumed 1 atm
                      plot_area = 0.0625 # area of the plot in m^2, default for Three-D
                      )
  {
  R = 0.082057 #gas constant, in L*atm*K^(-1)*mol^(-1)
  vol = chamber_volume + tube_volume
fluxes_final <- co2conc %>% 
  # group_by(ID) %>% 
  nest(-ID) %>% 
  mutate(
    data = map(data, ~.x %>% 
                 mutate(time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"), #add a column with the time difference between each measurements and the beginning of the measurement. Usefull to calculate the slope.
                        PARavg = mean(PAR, na.rm = TRUE), #mean value of PAR for each flux
                        temp_airavg = mean(temp_air, na.rm = TRUE)  #mean value of Temp_air for each flux
                        + 273.15 #transforming in kelvin for calculation
                 )), 
    fit = map(data, ~lm(CO2 ~ time, data = .)), #fit is a new column in the tibble with the slope of the CO2 concentration vs time (in secs^(-1))
    # slope = map_dbl(fit, "time")
    results = map(fit, glance), #to see the coefficients of the model
    slope = map(fit, tidy) #creates a tidy df with the coefficients of fit
  ) %>% 
  
  unnest(results, slope) %>% 
  unnest(data) %>% 
  filter(term == 'time'  #filter the estimate of time only. That is the slope of the CO2 concentration. We need that to calculate the flux.
         # & r.squared >= 0.7 #keeping only trendline with an r.squared above or equal to 0.7. Below that it means that the data are not good quality enough
         # & p.value < 0.05 #keeping only the significant fluxes
  ) %>% 
  # select(ID, Plot_ID, Type, Replicate, Remarks, Date, PARavg, Temp_airavg, r.squared, p.value, estimate, Campaign) %>% #select the column we need, dump the rest
  distinct(ID, plot_ID, type, replicate, remarks, date, PARavg, temp_airavg, r.squared, p.value, estimate, campaign, .keep_all = TRUE) %>%  #remove duplicate. Because of the nesting, we get one row per Datetime entry. We only need one row per flux. Select() gets rid of Datetime and then distinct() is cleaning those extra rows.
#calculate fluxes using the trendline and the air temperature
  mutate(flux = (estimate * atm_pressure * vol)/(R * temp_airavg * plot_area) #gives flux in micromol/s/m^2
         *3600 #secs to hours
         /1000 #micromol to mmol
  ) %>%  #flux is now in mmol/m^2/h, which is more common
  select(datetime, ID, plot_ID, type, replicate, remarks, date, PARavg, temp_airavg, r.squared, p.value, flux, campaign)

return(fluxes_final)

}

flux.calc2 <- function(co2conc, # dataset of CO2 concentration versus time (output of match.flux)
                       chamber_volume = 24.5, # volume of the flux chamber in L, default for Three-D chamber (25x24.5x40cm)
                       tube_volume = 0.075, # volume of the tubing in L, default for summer 2020 setup
                       atm_pressure = 1, # atmoshperic pressure, assumed 1 atm
                       plot_area = 0.0625 # area of the plot in m^2, default for Three-D
)
{
  R = 0.082057 #gas constant, in L*atm*K^(-1)*mol^(-1)
  vol = chamber_volume + tube_volume
  # co2conc <- co2_cut
  slopes <- co2conc %>% 
    group_by(ID) %>% 
    mutate(
      time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs")
    ) %>% 
    select(ID, time, CO2) %>%
    do({model = lm(CO2 ~ time, data=.)    # create your model
    data.frame(tidy(model),              # get coefficient info
               glance(model))}) %>%          # get model info
    filter(term == "time") %>% 
    rename(slope = estimate) %>% 
    select(ID, slope, p.value, r.squared, adj.r.squared, nobs) %>% 
    ungroup()
  
  means <- co2conc %>% 
    group_by(ID) %>% 
    summarise(
      PARavg = mean(PAR, na.rm = TRUE), #mean value of PAR for each flux
      temp_airavg = mean(temp_air, na.rm = TRUE)  #mean value of temp_air for each flux
      + 273.15, #transforming in kelvin for calculation
    ) %>% 
    ungroup()
  
  fluxes_final <- left_join(slopes, means, by = "ID") %>% 
    left_join(
      co2conc,
      by = "ID"
    ) %>% 
    select(ID, slope, p.value, r.squared, adj.r.squared, nobs, PARavg, temp_airavg, plot_ID, type, campaign, remarks, start_window) %>% 
    distinct() %>% 
    rename(
      datetime = start_window
    ) %>% 
    mutate(
      flux = (slope * atm_pressure * vol)/(R * temp_airavg * plot_area) #gives flux in micromol/s/m^2
      *3600 #secs to hours
      /1000 #micromol to mmol
    ) %>% #flux is now in mmol/m^2/h, which is more common
    arrange(datetime) %>% 
    select(!slope)
  
  return(fluxes_final)
  
}

flux.calc3 <- function(co2conc, # dataset of CO2 concentration versus time (output of match.flux)
                       chamber_volume = 24.5, # volume of the flux chamber in L, default for Three-D chamber (25x24.5x40cm)
                       tube_volume = 0.075, # volume of the tubing in L, default for summer 2020 setup
                       atm_pressure = 1, # atmoshperic pressure, assumed 1 atm
                       plot_area = 0.0625 # area of the plot in m^2, default for Three-D
)
{
  R = 0.082057 #gas constant, in L*atm*K^(-1)*mol^(-1)
  vol = chamber_volume + tube_volume
  # co2conc <- co2_cut
  slopes <- co2conc %>% 
    group_by(fluxID) %>% 
    mutate(
      time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs")
    ) %>% 
    select(fluxID, time, CO2) %>%
    do({model = lm(CO2 ~ time, data=.)    # create your model
    data.frame(tidy(model),              # get coefficient info
               glance(model))}) %>%          # get model info
    filter(term == "time") %>% 
    rename(slope = estimate) %>% 
    select(fluxID, slope, p.value, r.squared, adj.r.squared, nobs) %>% 
    ungroup()
  
  means <- co2conc %>% 
    group_by(fluxID) %>% 
    summarise(
      PARavg = mean(PAR, na.rm = TRUE), #mean value of PAR for each flux
      temp_airavg = mean(temp_air, na.rm = TRUE)  #mean value of temp_air for each flux
      + 273.15, #transforming in kelvin for calculation
      temp_soilavg = mean(temp_soil, na.rm = TRUE) #mean value of temp_soil for each flux
    ) %>% 
    ungroup()
  
  fluxes_final <- left_join(slopes, means, by = "fluxID") %>% 
    left_join(
      co2conc,
      by = "fluxID"
    ) %>% 
    select(fluxID, slope, p.value, r.squared, adj.r.squared, nobs, PARavg, temp_airavg, temp_soilavg, turfID, type, campaign, comments, start_window) %>% 
    distinct() %>% 
    rename(
      datetime = start_window
    ) %>% 
    mutate(
      flux = (slope * atm_pressure * vol)/(R * temp_airavg * plot_area) #gives flux in micromol/s/m^2
      *3600 #secs to hours
      /1000 #micromol to mmol
    ) %>% #flux is now in mmol/m^2/h, which is more common
    arrange(datetime) %>% 
    select(!slope)
  
  return(fluxes_final)
  
}