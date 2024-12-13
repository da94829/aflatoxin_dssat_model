# install necessary packages and load libraries
install.packages("devtools")
devtools::install_github("palderman/DSSAT")
install.packages("DSSAT")

library(DSSAT)
library(tidyverse)
library(lubridate)
library(purrr)

# Set options for DSSAT executable
options(DSSAT.CSM = 'C:\\DSSAT48\\dscsm048.exe')

##########################################################################
# Reading and modifying a single DSSAT file
file_x <- read_filex('C:/DSSAT48/Peanut/GAAP1660.PNX')

file_x$FIELDS <- file_x$FIELDS %>% 
  mutate_cond(L ==3, 
              WSTA = c("AAAA"))

tibble(FILEX = file_x,TRTNO = 1:40, RP = 1, SQ = 0, OP = 0, CO = 0,
       file_name = NULL) %>% 
  write_dssbatch()

run_dssat()

a <- read_output('AfConc.OUT')

##########################################################################

# Generate and run DSSAT simulations for multiple years, files (soil types), and weather stations

# Set working directory and output paths 
setwd("C:/DSSAT48/Peanut")
output_dir <- "C:/Users/da94_/OneDrive - University of Florida/DSSAT/DSSAT output/"


# Define simulation parameters
years <- 2016:2023
file_list <- c( "GAAP1620.PNX", "GAAP1621.PNX","GAAP1622.PNX","GAAP1623.PNX")
# weather_stations <- c("AAAA", "AAAB", "AAAC", "AAAD", "AAAE", "AAAF", "AAAG", 
#                      "AAAH", "AAAI", "AAAJ", "AAAK", "AAAL", "AAAM", "AAAN",
#                      "AAAO", "AAAP", "AAAR", "AAAS", "AAAT", "AAAU", "AAAV", 
#                      "AAAW", "AAAX", "AAAY", "AAAZ", "AABA", "AABB", "AABC", 
#                      "AABD", "AABE", "AABF", "AABG")

# Subset of weather stations
weather_stations <- c("AAAF", "AAAV", "AAAC", "AAAL", "AAAR", "AABB", "AABC",
                      "AAAI", "AAAK", "AAAM", "AAAT", "AAAA")

for (year in years) {
  
  # Iterate over each file
  for (file_name in file_list) {
    
    # Read the file
    file_x <- read_filex(paste0('C:/DSSAT48/Peanut/', file_name))
    
    # Iterate over each weather station
    for (weather_station in weather_stations) {
      
      # Modify the WSTA column in the FIELDS section
      file_x$FIELDS <- file_x$FIELDS %>%
        mutate_cond(L == 3, WSTA = weather_station)
      
      file_x$`INITIAL CONDITIONS` <- file_x$`INITIAL CONDITIONS` %>%
        mutate(ICDAT = if_else(C == 1, ymd(paste0(year, "-", month(ICDAT), "-", day(ICDAT))), ICDAT))
      
      file_x$`PLANTING DETAILS` <- file_x$`PLANTING DETAILS`  %>%
        mutate(PDATE = update(PDATE, year = year))
      
      file_x$`IRRIGATION AND WATER MANAGEMENT` <- file_x$`IRRIGATION AND WATER MANAGEMENT` %>%
        mutate(IDATE = update(IDATE, year = year))
      
      file_x$`HARVEST DETAILS` <- file_x$`HARVEST DETAILS` %>% 
        mutate(HDATE = update(HDATE, year = year))
        
      file_x$`SIMULATION CONTROLS` <- file_x$`SIMULATION CONTROLS` %>% 
        mutate(SDATE = update(SDATE, year = year),
               PFRST = update(PFRST, year = year),
               PLAST = update(PLAST, year = year),
               HLAST = update(HLAST, year = year),)
      
      # Save the modified file with a new name that reflects the year and weather station
      modified_file_name <- paste0('C:/DSSAT48/Peanut/modified_', tools::file_path_sans_ext(file_name), '_', year, '_', weather_station, '.PNX')
      write_filex(file_x, modified_file_name)
      
      # Write DSSAT batch
      tibble(FILEX = modified_file_name,
             TRTNO = 1:40,
             RP = 1,
             SQ = 0,
             OP = 0,
             CO = 0,
             file_name = NULL) %>%
        write_dssbatch()
      
      # Run DSSAT
      run_dssat()
      
      # Read the output
      a <- read_output('AfConc.OUT')
      b <- read_output('SoilWat.OUT')
      
      # Write the output with a different name 
      write_csv(a, paste0(output_dir, 'AfConc_', 
                          tools::file_path_sans_ext(file_name), '_', year, '_', weather_station, '.csv'))
      
      # Save `b` output with a descriptive name
      write_csv(b, paste0(output_dir, 'SoilWat_', 
                          tools::file_path_sans_ext(file_name), '_', year, '_', weather_station, '.csv'))
    }
  }
}
  