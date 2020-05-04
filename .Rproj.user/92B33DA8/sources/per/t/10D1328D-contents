#This script will be used to create and store most of the functions 
#that will be used in the rest of my assignment

library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(sf)



#Gets latest ncov data and converts date to date class
get_ncov_data = function() {
  ncov <-
    read.csv(
      "https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv"
    )
  # convert Date from factor to date class
  ncov <- ncov %>%
    mutate(Date = ymd(Date))
  
  return(ncov)
}

#Get geometry data
get_geom_data = function(){
  world_ne0 = ne_countries(scale = "medium", returnclass = "sf", type = "countries")
  world_ne = world_ne0 %>%
    select(c(sovereignt, geometry))
  
  #Some countries have multiple entries in their geometry data e.g. for certain colonies of the UK
  #The ncov data obviously doesn't account for this so the graphics are greatly distorted since these tiny islands have 'thousands' of cases
  #These will be removed and only the "mainland" kept
  rows_to_remove = c(13,92,100,158, #Australia
                     91, 133, #China
                     55, #Cyprus
                     74,87, #Denmark
                     6, #Finland
                     14,28,134,156,181,200,236, #France
                     1, 53, 208, #Netherlands
                     47,161, #NZ
                     11,89,148,176,233, #US
                     4,31,54,72,79,98,101,109,151,170,192,193,211,232) #UK
  
  world_ne = world_ne[-rows_to_remove,]
  
  return(world_ne)
}

#Add countrycodes to both ncov and world_ne
#Countrycodes are used to join geometry data to ncov data
add_countrycodes = function(df_ncov = ncov, df_geom = world_ne) {
  #First create column in ncov for World Bank country code. Will then use this code to join geometry data
  df_ncov$iso3c = countrycode(
    sourcevar = ncov$Country,
    origin = "country.name",
    destination = "wb"
  )
  
  #Missing code for Holy See, Diamond Princess and MS Zaandam
  #Need to replace Holy See with Vatican and remove '*' from Taiwan
  df_ncov$Country = df_ncov$Country %>%
    recode("Holy See" = "Vatican") %>%
    recode("Taiwan*" = "Taiwan")
  
  #Add code for missing countries in ncov
  df_ncov = df_ncov %>%
    mutate(iso3c = ifelse(Country == "Vatican", "VAT", iso3c)) %>%
    mutate(iso3c = ifelse(Country == "Western Sahara", "ESH", iso3c)) %>%
    mutate(iso3c = ifelse(Country == "Diamond Princess", "CDP", iso3c)) %>%
    mutate(iso3c = ifelse(Country == "MS Zaandam", "CMZ", iso3c))
  
  df_geom$iso3c = countrycode(
    sourcevar = world_ne$sovereignt,
    origin = "country.name",
    destination = "wb"
  )
  
  #A few countries without countrycodes so need to insert manually in world geom data
  df_geom = df_geom %>%
    mutate(iso3c = ifelse(sovereignt == "Western Sahara", "ESH", iso3c)) %>%
    mutate(iso3c = ifelse(sovereignt == "Vatican", "VAT", iso3c))
  
  #world_ne contains 2 Israel entries. Row 179 is West Bank and Gaza so will be renamed
  df_geom[180, 1] = "West Bank and Gaza"
  df_geom = df_geom %>%
    mutate(iso3c = ifelse(sovereignt == "West Bank and Gaza", "PSE", iso3c))
  
  return(list(ncov = df_ncov, world_ne = df_geom))
}

#Join geom data to ncov data
dat_join = function(df_ncov = ncov, df_geom = world_ne) {
  #Join ncov and geometry data
  dat = left_join(df_ncov, df_geom, by = c("iso3c"))
  dat = dat %>%
    select(-sovereignt)
  
  return(dat)
}

#Remove cruise ship data
#As of 8 April 2020, both Diamond Princess and MS Zaandam had been evacuated and 
#passengers sent to their respective countries. These two cruise ships will be 
#ignored when plotting on a map but will be kept for total cases/deaths/recovered etc.
remove_cruises = function(df = dat){
  dat_cruise <<- df %>%
    filter(Country == "Diamond Princess") %>%
    filter(Country == "MS Zaandam")
  
  dat = df %>%
    filter(!(Country == "Diamond Princess")) %>%
    filter(!(Country == "MS Zaandam"))
  
  return(dat)
}

#Get newest ncov data
#Get latest ncov data. This can be used to plot the map since we only need one plot, whereas the 
#'dat' dataset contains a repeated geometry for each observation
get_newest = function(df) {
  date_newest = max(df$Date)
  ncov_newest = df %>%
    filter(Date == date_newest)
  return(ncov_newest)
}

#Add population data for each country to ncov data
get_pop_data = function(df = dat){
  #Get population data
  pop_dat = googleVis::Population %>%
    select(c("Country", "Population")) %>%
    rename(country2 = Country) %>%
    rename(pop = Population)
  
  #Add country codes as per above
  pop_dat$iso3c = countrycode(sourcevar = pop_dat$country2, 
                              origin = "country.name", destination = "wb")
  #Add Western Sahara and Vatican
  pop_dat = pop_dat %>%
    mutate(iso3c = ifelse(country2 == "Western Sahara", "ESH", iso3c)) %>%
    mutate(iso3c = ifelse(country2 == "Vatican City", "VAT", iso3c))
  
  #Add population data to dat
  df = left_join(df, pop_dat, by = c("iso3c")) %>%
    select(-c("country2")) %>%
    mutate(pop = ifelse(Country == "Taiwan", 23780000, pop)) %>%
    mutate(pop = ifelse(Country == "South Sudan", 12580000, pop)) %>%
    mutate(pop = ifelse(Country == "Kosovo", 1831000, pop)) %>% #Add missing populations
    mutate(cases_per_mil = Confirmed*1000000/pop) #Calc cases per million
  
  return(df)
}

add_centroids = function(df = dat){
  #Centroids are used when plotting geom_point instead of filling the countries based on a colour scale
  centers = st_centroid(df$geometry)
  # dat$centers = st_coordinates(centers)
  df = cbind(df, st_coordinates(centers))
  
  #Alaska throws off the center of USA so will set that manually
  df = df %>%
    mutate(X = ifelse(Country == "US", -100.522057, X)) %>%
    mutate(Y = ifelse(Country == "US", 39.975361, Y))
  
  return(df)
}