# Jai_Dog
Analysis of Jai Dog's transect data.


This repo contains all data and scripts for the manuscript titled "The Impacts of Dog Sterilization Campaigns in Thailand: Understanding Changes in Dog Population Dynamics and Welfare". 

# Data
Contains all .csv and .rds files used throughout the analysis.

clinic_data.rds - Cleaned dataset of all sterilization surgeries conducted in the study area.

dog_density.rds - Cleaned dataset of transect data summarized by individual transect. For use in dog density models (model 1).

FULL_clinic_data.csv - Raw dataset of all sterilization surgeries conducted by Jai Dog up to Oct. 2025.

FULL_dog_density.csv - Raw dataset of all transect data summarized by individual transect up to Oct. 2025.

FULL_sightings.csv - Raw dataset of all individual dog observations during transects up to Oct. 2025.

sightings.rds - Cleaned dataset of individual dog observations during transects. For use in sterilization status (model 2), population structure (model 3), and health status (model 4) models.

# Plots
Contains all plot outputs of corresponding models.

# Scripts
Contains scripts for all models and creation of RDS files

density_RDS.R - Script cleaning FULL_dog_density.csv and FULL_clinic_data.csv, and saving as RDS file.

m1_dd_all.R - Model 1 Dog density, with all days of data included.

m1_dd_drop2.R - Model 1 Dog density, with day 2 data excluded.

m2_ster_all.R - Model 2 Sterilization status, with all days of data included.

m2_ster_drop2.R - Model 2 Sterilization status, with day 2 data excluded.

m3_pop_all.R - Model 3.1 Presence of lactating females and Model 3.2 Presence of puppies, with all days of data included.

m3_pop_drop2.R - Model 3.2 Presence of puppies, with day 2 data excluded.

m4_health_all.R - Model 4 Health status, with all days of data included.

sightings_RDS.R - Script cleaning FULL_sightings.csv, and saving as RDS file. 


