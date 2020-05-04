#Script is used to 'clean' data
source("Assignment-4-Functions.R")

#Get latest COVID-19 data
ncov = get_ncov_data()

#Get geometry data for the world and all its countries
world_ne = get_geom_data()

#Remove cruise ship data
#As of 8 April 2020, both Diamond Princess and MS Zaandam had been evacuated and 
#passengers sent to their respective countries. These two cruise ships will be 
#ignored when plotting on a map but will be kept for total cases/deaths/recovered etc.
ncov = remove_cruises(df = ncov)

#Need to add geometry data to ncov data
df_with_countrycodes = add_countrycodes(df_ncov = ncov, df_geom = world_ne)
ncov = df_with_countrycodes[[1]]
world_ne = df_with_countrycodes[[2]]

dat = dat_join(df_ncov = ncov, df_geom = world_ne)

#Remove cruise ship data
#As of 8 April 2020, both Diamond Princess and MS Zaandam had been evacuated and 
#passengers sent to their respective countries. These two cruise ships will be 
#ignored when plotting on a map but will be kept for total cases/deaths/recovered etc.
#dat = remove_cruises(df = dat)

#Population data can be used to work out cases per million people in a country
dat = get_pop_data(df = dat)

ncov_newest = get_newest(df = dat)


saveRDS(dat, file = "Shiny/app/ncov-dat.rds")
saveRDS(ncov_newest, file = "Shiny/app/ncov-newest.rds")