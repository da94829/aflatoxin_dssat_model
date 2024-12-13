# Load necessary libraries
library(dplyr)
library(readr)
library(purrr)
library(lubridate)

# Set the directory where output files are located
path <- "C:/Users/dayoung.kim/OneDrive - University of Florida/DSSAT/DSSAT output/AfConc/"


## Dothan soil
# Use list.files with pattern matching to directly read and bind the files
# Extract the latest AFCON by EXPERIMENT(year) and RUN(simulation option)
Dothan <- do.call(rbind, lapply(list.files(path = path, 
                                           pattern = "GAAP1620", 
                                           full.names = TRUE), read_csv)) %>% 
  group_by(EXPERIMENT, RUN) %>%
  filter(DATE == max(DATE)) %>%  
  mutate(soil = "Dothan Soil",
         year = year(DATE)) %>% 
  select(EXPERIMENT, RUN, year, soil, AFCON)

## Fuquay Soil
Fuquay <- do.call(rbind, lapply(list.files(path = path, 
                                           pattern = "GAAP1621", 
                                           full.names = TRUE), read_csv)) %>% 
  group_by(EXPERIMENT, RUN) %>%
  filter(DATE == max(DATE)) %>%  
  mutate(soil = "Fuquay Soil",
         year = year(DATE)) %>% 
  select(EXPERIMENT, RUN, year, soil, AFCON)


## Pelhem Soil
Pelhem <- do.call(rbind, lapply(list.files(path = path, 
                                           pattern = "GAAP1622", 
                                           full.names = TRUE), read_csv)) %>% 
  group_by(EXPERIMENT, RUN) %>%
  filter(DATE == max(DATE)) %>%  
  mutate(soil = "Pelhem Soil",
         year = year(DATE)) %>% 
  select(EXPERIMENT, RUN, year, soil, AFCON)

## Leefield Soil
Leefield <- do.call(rbind, lapply(list.files(path = path, 
                                             pattern = "GAAP1623", 
                                             full.names = TRUE), read_csv)) %>% 
  group_by(EXPERIMENT, RUN) %>%
  filter(DATE == max(DATE)) %>%  
  mutate(soil = "Leefield Soil",
         year = year(DATE)) %>% 
  select(EXPERIMENT, RUN, year, soil, AFCON)

## Tifton Soil
Tifton <- do.call(rbind, lapply(list.files(path = path, 
                                           pattern = "GAAP1614", 
                                           full.names = TRUE), read_csv)) %>% 
  group_by(EXPERIMENT, RUN) %>%
  filter(DATE == max(DATE)) %>%  
  mutate(soil = "Tifton Soil",
         year = year(DATE)) %>% 
  select(EXPERIMENT, RUN, year, soil, AFCON)

# combine all the soil type 

data <- rbind(Dothan, Fuquay, Pelhem, Leefield, Tifton)

write_csv(data, "Dssat_simulation_original_AfConc.csv")
