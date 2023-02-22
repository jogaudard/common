# functions I use to clean and treat my data from carbon fluxes measurements

# match flux will attribute CO2 concentration to each flux measurements
# raw_flux is the continuous measurement from the logger
# field_record is the manually written record of what plot was measured when

match.flux <- function(raw_flux, field_record){
 co2conc <- full_join(raw_flux, field_record, by = c("datetime" = "start"), keep = TRUE) %>% #joining both dataset in one
    fill(PAR,temp_air,plot_ID,type,replicate,campaign,starting_time,date,start,end,start_window, end_window) %>% #filling all rows (except Remarks) with data from above
    group_by(date, plot_ID, type, replicate) %>% #this part is to fill Remarks while keeping the NA (some fluxes have no remark)
    fill(remarks) %>% 
    ungroup() %>% 
    mutate(ID = group_indices(., date, plot_ID, type, replicate)) %>% #assigning a unique ID to each flux, useful for plotting uzw
    filter(
      datetime <= end
      & datetime >= start) #%>% #cropping the part of the flux that is after the End and before the Start
    # select(datetime, CO2, PAR, temp_air, plot_ID, type, replicate, campaign, ID, remarks, date)
  
  
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
    fill(PAR,temp_air,temp_soil, turfID,type,campaign,starting_time,date,start,end,start_window, end_window) %>% #filling all rows (except Remarks) with data from above
    group_by(date, turfID, type) %>% #this part is to fill Remarks while keeping the NA (some fluxes have no remark)
    fill(comments) %>% 
    # mutate(ID = cur_group_id()) %>% #assigning a unique ID to each flux, useful for plotting uzw
    ungroup() %>% 
    filter(
      datetime <= end
      & datetime >= start) #%>% #cropping the part of the flux that is after the End and before the Start
  # select(datetime, CO2, PAR, temp_air, plot_ID, type, replicate, campaign, ID, remarks, date)
  co2conc_fluxes <- full_join(raw_flux, field_record_fluxes, by = c("datetime" = "start"), keep = TRUE) %>% #joining both dataset in one
    fill(PAR,temp_air, temp_soil, turfID,type,campaign,starting_time,date,start,end,start_window, end_window) %>% #filling all rows (except Remarks) with data from above
    group_by(date, turfID, type) %>% #this part is to fill Remarks while keeping the NA (some fluxes have no remark)
    fill(comments) %>% 
    # mutate(ID = cur_group_id()) %>% #assigning a unique ID to each flux, useful for plotting uzw
    ungroup() %>% 
    filter(
      datetime <= end
      & datetime >= start) #%>% #cropping the part of the flux that is after the End and before the Start
  
  co2conc <- full_join(co2conc_fluxes, co2conc_LRC) %>% 
    group_by(date, turfID, type) %>% 
    mutate(fluxID = cur_group_id()) %>% 
    ungroup()
  
  
  return(co2conc)
}

match.flux4 <- function(raw_flux,
                         field_record,
                         window_length = 90,
                         startcrop = 10,
                         measurement_length = 210,
                         date_format = "dmy", # dmy, ymd or mdy
                         time_format = "time" # time is the form hh:mm:ss, "whole" is hhmmss
                         ){
  
  raw_flux <- raw_flux %>% 
    rename( #rename the columns with easier names to handle in the code
      datetime = "Date/Time",
      temp_air = "Temp_air ('C)",
      temp_soil = "Temp_soil ('C)",
      CO2 = "CO2 (ppm)",
      PAR = "PAR (umolsm2)"
    ) %>% 
    mutate(
      datetime = dmy_hms(datetime), #transform the date into R date format
      temp_air = as.numeric(temp_air),
      temp_soil = as.numeric(temp_soil),
      CO2 = as.numeric(CO2),
      PAR = as.numeric(PAR),
    ) %>% 
    select(datetime, temp_soil, temp_air, CO2, PAR)
  
  field_record <- field_record %>%
    mutate(
      starting_time = case_when(
        time_format == "whole" ~ gsub("(\\d{2})(?=\\d{2})", "\\1:", starting_time, perl = TRUE), # to add the : in the time
        time_format == "time" ~ starting_time
      ),
      date = case_when(
        # !is.na(ymd(date)) ~ ymd(date),
        # !is.na(dmy(date)) ~ dmy(date)
        date_format == "ymd" ~ ymd(date),
        date_format == "dmy" ~ dmy(date),
        date_format == "mdy" ~ mdy(date)
      ),
      # date = dmy(date), #date in R format
      start = ymd_hms(paste(date, starting_time)), #pasting date and time together to make datetime
      end = start + measurement_length, #creating column End
      start_window = start + startcrop, #cropping the start
      end_window = start_window + window_length, #cropping the end of the measurement
      # ) %>%
      # arrange(start) %>%
      # mutate(
      fluxID = row_number() #adding an individual ID to each flux, useful to join data or graph the fluxes
    ) %>% 
    select(start, end, start_window, end_window, fluxID, turfID, type, date)
  
  
  co2conc <- full_join(raw_flux, field_record, by = c("datetime" = "start"), keep = TRUE) %>% #joining both dataset in one
    fill(PAR,temp_air, temp_soil, turfID,type,start,end,start_window, end_window, fluxID, date) %>% #filling all rows with data from above
    
    filter(
      datetime <= end
      & datetime >= start) %>% #cropping the part of the flux that is after the End and before the Start
    mutate(
      type = as_factor(type),
      fluxID = as.numeric(fluxID)
    )
  
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
                        + 273.15, #transforming in kelvin for calculation
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
  select(datetime, ID, plot_ID, type, replicate, remarks, date, PARavg, temp_airavg, r.squared, p.value, nobs, flux, campaign)

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
    select(ID, slope, p.value, r.squared, adj.r.squared, nobs, PARavg, temp_airavg, plot_ID, type, campaign, remarks, start_window, replicate) %>% 
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

# zhao2018 ----------------------------------------------------------------

# function to select window automatically and calculate slope based on zhao et al 2018

# function providing the df with the two fits, linear and non linear
fitting.flux <- function(data,
                         weird_fluxesID = NA, # a vector of fluxes to discard because they are obviously wrong
                         t_window = 20, # enlarge focus window before and after tmin and tmax
                         Cz_window = 15, # window used to calculate Cz, at the beginning of cut window
                         b_window = 10, # window to estimate b. It is an interval after tz where it is assumed that C fits the data perfectly
                         # c = 3 # coefficient to define the interval around estimates to optimize function
                         noise = 10, # noise of the setup in ppm
                         r.squared_threshold = 0.1, #threshold to discard data based on r.squared of the linear fit at tz over the kept part
                         RMSE_threshold = 25, # threshold above which data are discarded
                         cor_threshold = 0.5, # delimits the window in which CO2 is considered not correlated with time
                         b_threshold = 1, # this value and its opposite define a window out of which data are being discarded
                         ambient_CO2 = 421,
                         error = 100 # error of the setup in ppm. fluxes starting outside of the window ambient_CO2 +/- error will be discarded
){ 
  
  # data <- co2_fluxes_liahovden# %>% # this sis just to test the function with data sample
  # filter(
  #   fluxID %in% c(160: 169)
  # )
  
  CO2_df <- data %>% 
    group_by(fluxID) %>% 
    mutate(
      time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
      time = as.double(time),
      tmax = max(datetime[CO2 == max(CO2)]),
      tmin = min(datetime[CO2 == min(CO2)]),
      start_cut = case_when(
        tmax > tmin ~ tmin - t_window,
        tmin > tmax ~ tmax - t_window
      ),
      end_cut = case_when(
        tmax < tmin ~ tmin + t_window,
        tmin < tmax ~ tmax + t_window
      ),
      start_window = case_when(
        start_cut > start_window ~ start_cut,
        TRUE ~ start_window
      ),
      end_window = case_when(
        end_cut < end_window ~ end_cut,
        TRUE ~ end_window
      ),
      cut = case_when(
        datetime < start_window | datetime > end_window ~ "cut",
        # fluxID ==  & datetime %in%  ~ "cut",
        # fluxID %in% weird_fluxesID ~ "cut",
        TRUE ~ "keep"
      ),
      cut = as_factor(cut),
      time_corr = difftime(start_window, start, units = "secs"), # need a correction because in this df time is starting at beginning, not at cut
      time_corr = as.double(time_corr)
    ) %>% 
    ungroup()
  
  cut_CO2_df <- CO2_df %>% 
    group_by(fluxID) %>% 
    filter(cut == "keep") %>% 
    mutate(
      time_cut = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
      time_cut = as.double(time_cut)
    ) %>% 
    ungroup()
  
  Cm_df <- cut_CO2_df %>% 
    group_by(fluxID) %>% 
    distinct(CO2, .keep_all = TRUE) %>% 
    # mutate(
    #   time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
    #   time = as.double(time)
    # ) %>% 
    mutate(
      Cmax = max(CO2),
      Cmin = min(CO2),
      tmax = time_cut[CO2 == Cmax],
      tmin = time_cut[CO2 == Cmin]
    ) %>% 
    select(fluxID, Cmax, Cmin, tmax, tmin) %>% 
    ungroup() %>% 
    distinct(Cmax, Cmin, .keep_all = TRUE)
  
  Cm_slope <- cut_CO2_df %>% 
    group_by(fluxID) %>% 
    # mutate(
    #   time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
    #   time = as.double(time)
    # ) %>% 
    do({model = lm(CO2 ~ time_cut, data=.)    # create your model
    data.frame(tidy(model),              # get coefficient info
               glance(model))}) %>%          # get model info
    filter(term == "time_cut") %>% 
    rename(slope_Cm = estimate) %>% 
    select(fluxID, slope_Cm) %>% 
    ungroup()
  
  Cm_df <- left_join(Cm_df, Cm_slope) %>% 
    mutate(
      Cm_est = case_when(
        slope_Cm < 0 ~ Cmin, #Cm is the maximum mixing point, which is when the limit of C(t) when t tends to infinite.
        slope_Cm > 0 ~ Cmax 
      ),
      tm = case_when(
        slope_Cm < 0 ~ tmin,
        slope_Cm > 0 ~ tmax
      )
    ) %>% 
    select(fluxID, Cm_est, tm, slope_Cm) %>% 
    ungroup()
  
  Cz_df <- cut_CO2_df %>%
    group_by(fluxID) %>%
    # mutate(
    #   time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
    #   time = as.double(time)
    # ) %>%
    # select(fluxID, time, CO2) %>%
    filter(
      time_cut <= Cz_window
    ) %>%
    do({model = lm(CO2 ~ time_cut, data=.)    # create your model
    data.frame(tidy(model),              # get coefficient info
               glance(model))}) %>%          # get model info
    pivot_wider(id_cols = fluxID, names_from = "term", values_from = "estimate") %>% 
    rename(
      Cz = "(Intercept)",
      slope_Cz = time_cut) %>%
    select(fluxID, Cz, slope_Cz) %>%
    ungroup()
  
  tz_df <- cut_CO2_df %>% 
    # left_join(Cm_df) %>% 
    left_join(Cz_df) %>% 
    group_by(fluxID) %>% 
    # mutate(
    #   time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
    #   time = as.numeric(time)
    # ) %>% 
    filter(
      time_cut > Cz_window
    ) %>% 
    mutate(
      Cd = abs(CO2-Cz),
      tz_est = min(time_cut[Cd == min(Cd)])
    ) %>% 
    ungroup() %>% 
    select(fluxID, tz_est) %>% 
    distinct()
  
  a_df <- cut_CO2_df %>% 
    group_by(fluxID) %>% 
    filter(
      datetime >= end_window - t_window
    ) %>% 
    do({model = lm(CO2 ~ time_cut, data=.)    # create your model
    data.frame(tidy(model),              # get coefficient info
               glance(model))}) %>%          # get model info
    pivot_wider(id_cols = fluxID, names_from = "term", values_from = "estimate") %>% 
    rename(
      a_est = time_cut
    ) %>% 
    select(fluxID, a_est) %>% 
    ungroup()
  
  Ct_df <- CO2_df %>% 
    left_join(tz_df) %>% 
    group_by(fluxID) %>% 
    mutate(
      Ct = CO2[time == tz_est + time_corr - Cz_window]
    ) %>% 
    ungroup() %>% 
    select(fluxID, Ct) %>% 
    distinct()
  
  estimates_df <- left_join(Cm_df, Cz_df) %>% 
    left_join(tz_df) %>% 
    left_join(a_df) %>% 
    left_join(Ct_df) %>% 
    mutate(
      b_est = log(abs((Ct - Cm_est + a_est * b_window)/(Cz - Cm_est))) * (1/b_window) # problem with the log? negative value? let's try with absolute value
    )
  
  # myfn <- function(time, CO2, par, Cz) {
  #   with(data, sqrt((1/length(time)) * sum((par[1]+par[2]*(time-par[4])+(Cz-par[1])*exp(-par[3]*(time-par[4]))-CO2)^2)))
  # }
  
  # myfn <- function(time, CO2, par, Cz) {
  #   sqrt((1/length(time)) * sum((par[1]+par[2]*(time-exp(par[4]))+(Cz-par[1])*exp(-(par[3]/(abs(par[3])+1))*(time-exp(par[4])))-CO2)^2))
  # }
  
  myfn <- function(time, CO2, par, Cz) {
    sqrt((1/length(time)) * sum((par[1]+par[2]*(time-exp(par[4]))+(Cz-par[1])*exp(-par[3]*(time-exp(par[4])))-CO2)^2))
  }
  
  myfn1 <- function(time, CO2, Cz, Cm, b, tz) {
    sqrt((1/length(time)) * sum((Cm+(Cz-Cm)*exp(-b*(time-tz))-CO2)^2))
  }
  
  myfn2 <- function(time, CO2, a, Cz, Cm, b, tz) {
    sqrt((1/length(time)) * sum((Cm+a*(time-tz)+(Cz-Cm)*exp(-b*(time-tz))-CO2)^2))
  }
  
  
  
  
  
  # myoptim <- function(Cm_est, a_est, b_est, tz_est, data) {
  #   results <- optim(par = c(Cm_est, a_est, b_est, tz_est), fn = myfn, data = data)
  #   return(results$par)
  # }
  
  # problem: need to loop the optim on each group of fluxID with specific par per group. MAybe I need to do a for loop
  # other idea: make a tibble with a column with par as vector and CO2 and time in another column as a df, and summarize optim on that
  
  fitting_par <- cut_CO2_df %>% 
    left_join(estimates_df) %>% 
    # filter(fluxID %in% c(111)) %>% # this is used when testing the function
    select(fluxID, Cm_est, a_est, b_est, tz_est, Cz, time_cut, CO2) %>%
    group_by(fluxID, Cm_est, a_est, b_est, tz_est, Cz) %>%
    # rowwise(fluxID, Cm_est, a_est, b_est, tz_est, Cz) %>%
    # nest() %>% 
    # tibble(
    #   ID = fluxID,
    #   par = unique(Cm_est)
    # )
    # filter(fluxID == 111) %>%
    # as_tibble(modify_at(list(
    #   Cm_est = Cm_est,
    #   a_est = a_est,
    #   CO2 = tibble(time_cut = time_cut, CO2 = CO2)
  # ), vars(Cm_est, a_est), unique))
  # mutate(
  # Cm = map(., optim(par = c(estimates_df$Cm_est[fluxID], estimates_df$a_est[fluxID], estimates_df$b_est[fluxID], estimates_df$tz_est[fluxID]), fn = myfn, data = .))$par[1],
  # a = apply(., c(1,2,3,4), optim(par = c(Cm_est, a_est, b_est, tz_est), fn = myfn, data = .))$par[2],
  # b = mean(CO2)
  # b = apply(myoptim(Cm_est, a_est, b_est, tz_est, data = CO2))
  # ID = fluxID
  # Cm_est = unique(Cm_est),
  # a_est = unique(a_est),
  # b_est = unique(b_est),
  # tz_est = unique(tz_est),
  
  # par = tibble(distinct(Cm_est, a_est, b_est, tz_est)),
  # data = tibble(time_cut, CO2)
  # CO2 = CO2
  # b = optim(par = c(estimates_df$Cm_est[estimates_df$fluxID==ID], estimates_df$a_est[estimates_df$fluxID==ID], estimates_df$b_est[estimates_df$fluxID==ID], estimates_df$tz_est[estimates_df$fluxID==ID]), fn = myfn, data = .)$par[3]
  # tz = optim(par = c(.$Cm_est, .$a_est, .$b_est, .$tz_est), fn = myfn, data = .)$par[4],
  # slope_tz = a + b * (Cm - Cz)
  # ) %>% 
  # nest(data = c(time_cut, CO2)) %>%
  nest() %>% 
    # mutate(
    #   rowID = cur_group_id()
    # ) %>%
    rowwise() %>%
    
    
    # data <- fitting_par$data %>%
    #   rename(
    #     time_cut = data$time_cut,
    #     CO2 = data$CO2
    #   )
    # a = optim(par = c(Cm_est = 526.39, a_est = -0.263391, b_est = -0.000181677, tz_est = 29), fn = myfn, CO2 = data$CO2, time = data$time_cut, Cz = 557.0225)$par[2]
    
    
    # ungroup()
  # summarise(
  #   data = tibble_row(Cm_est, a_est, b_est, tz_est)
  # par = list(Cm_est, a_est, b_est, tz_est)
  summarize(
    # model = map(data, optim(par = c(Cm_est, a_est, b_est, tz_est), fn = myfn, data = data))
    # model = map(data, function(df) lm(CO2 ~ time_cut, data = df))
    # results = map(data, myoptim(data))
    # results = map(data, myoptim(Cm_est, a_est, b_est, tz_est, df))
    #   par = c(Cm_est, a_est, b_est, tz_est)
    
    # I would like to do something more resilient to avoid stopping everything if there is a problem with optim. Maybe tryCatch can be an idea
    results = list(optim(par = c(Cm_est, a_est, b_est, log(tz_est)), fn = myfn, CO2 = data$CO2, time = data$time_cut, Cz = Cz)), #, lower=c(0, -Inf, -Inf, 0),  method="L-BFGS-B"
    Cm = results$par[1],
    a = results$par[2],
    b = results$par[3],#/(abs(results$par[3])+1), 
    #need to find the fit with the b closest to 0 (negative or positive)
    tz = exp(results$par[4]), #we force tz to be positive
    # b = bmin$minimum
    # Cm = Cm_est,
    # b = results1$minimum[1],
    # tz = tz_est,
    # Cm = optimize(myfn1, c(0, (c+1) * Cm_est), CO2 = data$CO2, time = data$time_cut, Cz = Cz, tz = tz_est, b = b_est)$minimum,
    # tz = optimize(myfn1, c(tz_est, (c+1) * tz_est), CO2 = data$CO2, time = data$time_cut, Cz = Cz, Cm = Cm, b = b_est)$minimum,
    # b = optimize(myfn1, c(-10, 10), CO2 = data$CO2, time = data$time_cut, Cz = Cz, Cm = Cm, tz = tz)$minimum, #, lower=c(0, -Inf, -Inf, 0),  method="L-BFGS-B"
    # a = optimize(myfn2, c(-c * a_est, (c+1) * a_est), CO2 = data$CO2, time = data$time_cut, Cz = Cz, Cm = Cm, b = b, tz = tz)$minimum,
    # a = results2$minimum[1],
    
    # a = optim(par = c(Cm_est = 526.39, a_est = -0.263391, b_est = -0.000181677, tz_est = 29), fn = myfn, CO2 = data$CO2, time = data$time_cut, Cz = 557)$par[2]
    # Cm = optim(par = c(Cm_est, a_est, b_est, log(tz_est)), fn = myfn, CO2 = data$CO2, time = data$time_cut, Cz = Cz)$par[1],
    # a = optim(par = c(Cm_est, a_est, b_est, log(tz_est)), fn = myfn, CO2 = data$CO2, time = data$time_cut, Cz = Cz)$par[2],
    # b = optim(par = c(Cm_est, a_est, b_est, log(tz_est)), fn = myfn, CO2 = data$CO2, time = data$time_cut, Cz = Cz)$par[3],
    # tz = optim(par = c(Cm_est, a_est, b_est, log(tz_est)), fn = myfn, CO2 = data$CO2, time = data$time_cut, Cz = Cz)$par[4],
    slope_tz = a + b * (Cm - Cz),
    # RMSE = myfn(CO2 = data$CO2, time = data$time_cut, Cz = Cz, par = c(Cm, a, b, tz))
    # RMSE = myfn2(time = data$time_cut, CO2 = data$CO2, a = a, b = b, Cm = Cm, Cz = Cz, tz = tz)
    # test = sum(data$time_cut)
    # a = apply(., c(1,2,3,4), optim(par = c(Cm_est, a_est, b_est, tz_est), fn = myfn, data = .))$par[2]
  ) %>% 
    ungroup()
  # select(fluxID, Cm, a, b, tz, slope_tz, Cz, results, RMSE)
  
  # distinct()
  
  # myfn2 <- function(time, CO2, par, Cz) {
  #   sqrt((1/length(time)) * sum((par[1]+par[2]*(time-par[4])+(Cz-par[1])*exp(-par[3]*(time-par[4]))-CO2)^2))
  # }
  
  # optim(par = c(fitting_par$Cm_est, fitting_par$a_est, fitting_par$b_est, fitting_par$tz_est), fn = myfn2, CO2 = fitting_par$data[[1]]$CO2, time = fitting_par$data[[1]]$time_cut, Cz = fitting_par$Cz)
  
  CO2_fitting <- CO2_df %>% 
    left_join(fitting_par) %>% 
    mutate(
      # time_corr = difftime(start_window, start, units = "secs"), # need a correction because in this df time is starting at beginning, not at cut
      # time_corr = as.double(time_corr),
      fit = Cm + a * (time - tz - time_corr) + (Cz - Cm) * exp(- b * (time - tz - time_corr)),
      fit_slope = slope_tz * (time - time_corr) + Cz - slope_tz * tz,
      # fit = Cm_est + a_est * (time - tz_est - time_corr) + (Cz - Cm_est) * exp(- b_est * (time - tz_est - time_corr)),
      # fit_slope = (a_est + b_est * (Cm_est - Cz) ) * (time - time_corr) + Cz - slope_tz * tz_est,
      start_z = start_window + tz # this is for graph purpose, to have a vertical line showing where tz is for each flux
      
    )# %>% 
  # group_by(fluxID, Cm, a, b, tz, Cz) %>% 
  #   # nest() %>% 
  #   mutate(
  #     RMSE = myfn2(time = time, CO2 = CO2, a = a, b = b, Cm = Cm, Cz = Cz, tz = tz - time_corr)
  #   ) %>% 
  #   ungroup
  
  r2_general <-function(preds,actual){ 
    return(1 - (sum((preds - actual)^2)/sum((preds - mean(actual))^2)))
  }
  
  model_fit <- CO2_fitting %>%
    group_by(fluxID) %>% 
    nest() %>% 
    rowwise() %>% 
    summarize(
      cor_coef = cor(data$CO2, data$time)
    )
  
  model_fit_cut <- CO2_fitting %>%
    filter(
      cut == "keep"
    ) %>%
    group_by(fluxID) %>% 
    nest() %>% 
    rowwise() %>%
    # select(fluxID, time, CO2, fit, fit_slope, Cm, a, b, Cz, tz, time_corr) %>% 
    summarize(
      cor_coef_keep = cor(data$CO2, data$time),
      r.squared = r2_general(data$fit, data$CO2),
      RMSE = sqrt((1/length(data$time)) * sum((data$fit - data$CO2)^2)),
      # RMSE = myfn2(time = data$time, CO2 = data$CO2, a = data$a, b = data$b, Cm = data$Cm, Cz = data$Cz, tz = data$tz - data$time_corr),
      norm_RMSE = RMSE / (max(data$CO2) - min(data$CO2)),
      r.squared_slope = r2_general(data$fit_slope, data$CO2),
      start_error = case_when(
        data$CO2[1] < (ambient_CO2 - error) ~ "error",
        data$CO2[1] > (ambient_CO2 + error) ~ "error",
        TRUE ~ "ok"
      )
    ) %>% 
    ungroup()
  
  CO2_fitting <- CO2_fitting %>% 
    left_join(model_fit) %>% 
    left_join(model_fit_cut)
  
  # now we need a set of rules to assess if the data should be kept, discarded, or replaced by 0
  # kept: good fit
  # discarded: bad fit, but with changes in CO2 concentration over time
  # replaced by 0: bad fit, and CO2 concentration does not change over time (just noise). How do we assess that?
  # Maybe we just set a threshold on the slope below which the slope is replaced by 0 if the fit is bad
  # This threshold can be calculated as the amplitude of the noise of the setup (to be fed into the function, depnding on the setup) divided by the lenght of the measurement
  
  CO2_fitting <- CO2_fitting %>% 
    mutate(
      threshold_slope = noise / as.double(difftime(end_window, start_window, units = "secs")),
      fit_quality = case_when(
        b <= -b_threshold & b >= b_threshold ~ "bad",
        RMSE > RMSE_threshold ~ "bad",
        r.squared_slope < r.squared_threshold ~ "bad",
        # fluxID %in% weird_fluxesID ~ "bad",
        # start_error == "error" ~ "bad",
        TRUE ~ "ok"
      ),
      correlation = case_when(
        cor_coef < cor_threshold & cor_coef > -cor_threshold ~ "no",
        TRUE ~ "yes"
      ),
      flag = case_when(
        fluxID %in% weird_fluxesID ~ "weird_flux",
        start_error == "error" ~ "start_error",
        fit_quality == "bad" & correlation == "yes" ~ "discard",
        fit_quality == "bad" & correlation == "no" ~ "zero",
        # fit_quality == "bad" & correlation == "no" & type == "NEE" ~ "zero",
        # fit_quality == "bad" & correlation == "no" & type == "ER" ~ "discard", # the idea being that we cannot have a respiration of 0, but I am not sure I agree with that. Soil could be super dry, frozen, containing very little organic matter...
        fit_quality == "ok" ~ "ok"
      )
    )
  
  return(CO2_fitting)
}



# function to graph specific fluxID or RMSE above a certain threshold

# function calculating fluxes for each fluxID and providiing RMSE

flux.calc.zhao18 <- function(co2conc, # dataset of slopes per fluxID and environmental data
                             chamber_volume = 24.5, # volume of the flux chamber in L, default for Three-D chamber (25x24.5x40cm)
                             tube_volume = 0.075, # volume of the tubing in L, default for summer 2020 setup
                             atm_pressure = 1, # atmoshperic pressure, assumed 1 atm
                             plot_area = 0.0625 # area of the plot in m^2, default for Three-D
)
{
  R = 0.082057 #gas constant, in L*atm*K^(-1)*mol^(-1)
  vol = chamber_volume + tube_volume
  
  slopes <- co2conc %>% 
    select(fluxID, slope, turfID, type, start_window, RMSE, a, b, tz, Cm, Cz, r.squared_slope) %>% 
    distinct()
  
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
    # left_join(
    #   co2conc,
    #   by = "fluxID"
    # ) %>% 
    select(fluxID, slope, PARavg, temp_airavg, temp_soilavg, turfID, type, start_window, RMSE, a, b, tz, Cm, Cz, r.squared_slope) %>% 
    distinct() %>% 
    rename(
      datetime = start_window
    ) %>% 
    mutate(
      flux = (slope * atm_pressure * vol)/(R * temp_airavg * plot_area) #gives flux in micromol/s/m^2
      *3600 #secs to hours
      /1000 #micromol to mmol
      # PARavg = case_when(
      #   type == "ER" ~ NA_real_,
      #   type == "NEE" ~ PARavg
      # )
    ) %>% #flux is now in mmol/m^2/h, which is more common
    arrange(datetime) %>% 
    select(!c(slope, temp_airavg))
  
  return(fluxes_final)
  
}