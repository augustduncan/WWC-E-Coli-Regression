# functions !!!! this will hopefully make everything more organized as we start to need separate data for each site.

#import packages
library(dataRetrieval)
library(tidyverse)
library(sf)
library(readnoaa)
library(zoo)

# access Mountain True data 
mountain_true_data <- read.csv("mountain_true.csv", check.names=FALSE)


# retrieving USGS data and merging and filtering with Mountain True data
# input arguments are USGS site number, start and end dates, and the corresponding Mountain True site
get_the_data <- function(usgs_site, start_time, end_time, mountain_true_site){
  
  raw_usgs <- read_waterdata_daily(monitoring_location_id = usgs_site, time = c(start_time, end_time))
  
  filtered_usgs <- raw_usgs %>% 
    dplyr::select(parameter_code, statistic_id, time, value) %>% 
    st_drop_geometry() %>% 
    pivot_wider(names_from = time)
  
  get_codes <- read_waterdata_parameter_codes(parameter_code = filtered_usgs$parameter_code) %>% 
    dplyr::select(parameter_code, parameter_name)
  
  filtered_usgs <- filtered_usgs %>% left_join(get_codes, by = "parameter_code")
  
  mountain_true_data <- mountain_true_data %>% dplyr::select(c(-Latitude, -Longitude)) %>% 
    filter(Site == mountain_true_site) %>% 
    mutate(parameter_name = "E Coli", statistic_id = "00006")
 
  common_cols <- intersect(names(mountain_true_data), names(filtered_usgs))
  
  merged_data <- rbind(mountain_true_data[,common_cols], filtered_usgs[,common_cols])
  
  stats_text <- read_waterdata_metadata(collection = "statistic-codes")
  stats_text <- stats_text %>% rename(statistic_id = statistic_code) %>% 
    dplyr::select(-statistic_description)
  
  merged_data <- merged_data %>% left_join(stats_text, by = "statistic_id") %>% 
    mutate(variable = paste(parameter_name, statistic_name, sep=".")) %>% 
    dplyr::select(-statistic_id, -statistic_name, -parameter_name)
  
  merged_data <- merged_data %>% 
    pivot_longer(cols = -variable, names_to = "Date", values_to = "value", values_transform = list(value = as.numeric)) %>% 
    pivot_wider(id_cols = Date, names_from = variable, values_from = value) %>% 
    rename_with(make.names) %>% relocate(Date, E.Coli.SUM, sort(names(.))) %>% drop_na(E.Coli.SUM)
}

# if the USGS site contains precipitation, run this function instead. 
get_the_data_and_precip <- function(usgs_site, start_time, end_time, mountain_true_site){
  
  raw_usgs <- read_waterdata_daily(monitoring_location_id = usgs_site, time = c(start_time, end_time))
  
  filtered_usgs <- raw_usgs %>% 
    dplyr::select(parameter_code, statistic_id, time, value) %>% 
    st_drop_geometry()
  
  rain <- filtered_usgs %>% filter(parameter_code == "00045") %>% 
    rename(Precip.1.SUM = value) %>%
    mutate(Precip.2.SUM = rollsumr(Precip.1.SUM, k = 2, fill = NA)) %>% 
    mutate(Precip.3.SUM = rollsumr(Precip.1.SUM, k = 3, fill = NA)) %>% 
    mutate(Precip.5.SUM = rollsumr(Precip.1.SUM, k = 5, fill = NA)) %>% 
    mutate(Precip.7.SUM = rollsumr(Precip.1.SUM, k = 7, fill = NA)) %>%
    mutate(across(where(is.numeric), round, digits = 2)) %>%
    pivot_longer(cols = c(4:8), names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = time, values_from = value) %>% 
    select(-parameter_code, -statistic_id)
  
  filtered_usgs <- filtered_usgs %>% pivot_wider(names_from = time)
  
  get_codes <- read_waterdata_parameter_codes(parameter_code = filtered_usgs$parameter_code) %>% 
    dplyr::select(parameter_code, parameter_name)
  
  filtered_usgs <- filtered_usgs %>% left_join(get_codes, by = "parameter_code")
  
  stats_text <- read_waterdata_metadata(collection = "statistic-codes")
  stats_text <- stats_text %>% rename(statistic_id = statistic_code) %>% 
    dplyr::select(-statistic_description)
  
  filtered_usgs <- filtered_usgs %>% left_join(stats_text, by = "statistic_id") %>% 
    mutate(variable = paste(parameter_name, statistic_name, sep=".")) %>% 
    dplyr::select(-statistic_id, -statistic_name, -parameter_name, -parameter_code)
  
  filtered_usgs <- bind_rows(filtered_usgs, rain)
  
  mountain_true_data <- mountain_true_data %>% dplyr::select(c(-Latitude, -Longitude)) %>% 
    filter(Site == mountain_true_site) %>% 
    mutate(variable = "E.Coli.SUM")
  
  common_cols <- intersect(names(mountain_true_data), names(filtered_usgs))
  
  merged_data <- rbind(mountain_true_data[,common_cols], filtered_usgs[,common_cols])
  
  merged_data <- merged_data %>% 
    pivot_longer(cols = -variable, names_to = "Date", values_to = "value", values_transform = list(value = as.numeric)) %>% 
    pivot_wider(id_cols = Date, names_from = variable, values_from = value) %>% 
    rename_with(make.names) %>% relocate(Date, E.Coli.SUM, sort(names(.))) %>% drop_na(E.Coli.SUM) %>% 
    relocate(Precip.1.SUM, Precip.2.SUM, Precip.3.SUM, Precip.5.SUM, Precip.7.SUM, .after = last_col())
}

# use with an appropriate site if the USGS site does not contain precipitation
# merging NOAA precipitation data to a given dataframe, assuming given dataframe has "Date" as a column
# assumes NOAA location of downtown Asheville for precipitation as it is the most consistent

# if/when we need more locations: 
# a station in east avl near the VA hospital and the parkway: US1NCBC0058 has good coverage
# a station in the NC arboretum: USW00053877 has good coverage
add_noaa_precip <- function(df, start_date, end_date){
  daily <- noaa_daily("US1NCBC0051", start_date, end_date, datatypes = c("PRCP"), units = "standard")
  
  daily <- daily %>% 
    mutate(Precip.2.SUM = rollsumr(prcp, k = 2, fill = NA)) %>% 
    mutate(Precip.3.SUM = rollsumr(prcp, k = 3, fill = NA)) %>% 
    mutate(Precip.5.SUM = rollsumr(prcp, k = 5, fill = NA)) %>% 
    mutate(Precip.7.SUM = rollsumr(prcp, k = 7, fill = NA)) %>% 
    rename(Date = date, Precip.1.SUM = prcp) %>% 
    mutate(Date = as.character(Date)) %>% 
    mutate(Precip.1.SUM = replace_na(Precip.1.SUM, 0))
  
  merged_data <- inner_join(daily, df, by = "Date")
    
  merged_data <- merged_data %>% dplyr::select(-station, -name) %>% 
    relocate(Precip.1.SUM, Precip.2.SUM, Precip.3.SUM, Precip.5.SUM, Precip.7.SUM, .after = last_col())
}

# groups e Coli levels (observed or predicted) based on the categorizations published by Mountain True. 
# good to review accuracy of the models
add_groups <- function(df, column_name){
  group_cut <- cut(df[[column_name]], breaks = c(-1000000, 126, 866, 100000))
  levels(group_cut) = c("primary", "secondary", "unsafe")
  
  data <- cbind(df, group_cut)
  new_name <- paste(column_name, ".GROUP", sep = "")
  data <- data %>% rename(!!new_name := group_cut) %>% relocate(!!new_name, .after = all_of(column_name))
}

# getting data for TODAY (actually this is going to be set for yesterday) 
# in order to plug into the formulas to predict e coli values!
# this includes USGS precipitation. 
get_todays_data <- function(usgs_site){
  today <- as.character(Sys.Date() - 1)
  seven_days_ago <- as.character(Sys.Date() - 7)
  
  today_usgs <- read_waterdata_daily(monitoring_location_id = usgs_site, time = c(seven_days_ago, today)) %>% 
    dplyr::select(parameter_code, statistic_id, time, value) %>% st_drop_geometry()
  
  rain <- today_usgs %>% filter(parameter_code == "00045") %>% 
    rename(Precip.1.SUM = value) %>%
    mutate(Precip.2.SUM = rollsumr(Precip.1.SUM, k = 2, fill = NA)) %>% 
    mutate(Precip.3.SUM = rollsumr(Precip.1.SUM, k = 3, fill = NA)) %>% 
    mutate(Precip.5.SUM = rollsumr(Precip.1.SUM, k = 5, fill = NA)) %>% 
    mutate(Precip.7.SUM = rollsumr(Precip.1.SUM, k = 7, fill = NA)) %>%
    filter(time == today) %>% 
    pivot_longer(cols = c(4:8), names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = time, values_from = value) %>% 
    select(-parameter_code, -statistic_id)
  
  today_usgs <- today_usgs %>% filter(`time` == today)
  
  get_codes <- read_waterdata_parameter_codes(parameter_code = today_usgs$parameter_code) %>% 
    dplyr::select(parameter_code, parameter_name)
  
  today_usgs <- today_usgs %>% pivot_wider(names_from = time) %>% left_join(get_codes, by = "parameter_code")
  
  stats_text <- read_waterdata_metadata(collection = "statistic-codes")
  
  stats_text <- stats_text %>% rename(statistic_id = statistic_code) %>% 
    dplyr::select(-statistic_description)
  
  today_usgs <- today_usgs %>% left_join(stats_text, by = "statistic_id") %>% 
    mutate(variable = paste(parameter_name, statistic_name, sep=".")) %>% 
    dplyr::select(-statistic_id, -statistic_name, -parameter_name, -parameter_code)
  
  today_usgs <- rbind(today_usgs, rain)
  
  today_usgs <- today_usgs %>% pivot_wider(names_from = variable, values_from = all_of(today))
  today_usgs$Date <- today
  colnames(today_usgs) <- make.names(colnames(today_usgs))
  
  return(today_usgs)
}


















