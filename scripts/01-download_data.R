#### Preamble ####
# Purpose: Downloads and saves the data input 5 file are present
# Author: Mohammed Yusuf Shaikh
# Date: 11 February 2024
# Contact: mohammedyusuf.shaikh@mail.utoronto.ca
# License: MIT
# Pre-requisites: Change Directory for running the code

#### Loading Workspace ####
# Load necessary libraries
library(dplyr)
library(readr)
library(haven)
library(tidyverse)
library(janitor)
library(knitr)

#### Downloading Data in Script ####

# Set working directory to where the data files are located
setwd("/cloud/project/inputs/data") # Replace with your actual path to the data files

# Read the data files

eurostat <- read_csv("Eurostat.csv")
market_share <- read_csv("market_share.csv")
specification_data <- read_dta("specification_data.dta")
temp_data <- read_dta("temp_data.dta")
bgd2 <- read_dta("bgd2.dta")
eu_countries <- read_dta("eu_countries.dta")

# Set the path to the input folder where the files will be saved
input_folder_path <- "/cloud/project/inputs/data"

# Write the files to the input folder
write_dta(eu_countries, file.path(input_folder_path, "eu_countries.dta"))
write_csv(eurostat, file.path(input_folder_path, "Eurostat.csv"))
write_csv(market_share, file.path(input_folder_path, "market_share.csv"))
write_dta(specification_data, file.path(input_folder_path, "specification_data.dta"))
write_dta(temp_data, file.path(input_folder_path, "temp_data.dta"))
write_dta(bgd2, file.path(input_folder_path, "bgd2.dta"))


         
