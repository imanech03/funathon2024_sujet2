library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(sf)
library(ggplot2)
library(plotly)
library(gt)
library(leaflet)
library(bslib)

source("R_bis/create_data_list.R")
source("R_bis/import_data.R")  
source("R_bis/clean_dataframe.R")
source("correction/R/divers_functions.R")
source("R_bis/tables.R")
source("R_bis/figures.R")

# Global variables ---------------------------

YEARS_LIST <- 2018:2022
MONTHS_LIST = 1:12

# Load data ----------------------------------
urls <- create_data_list("./sources.yml")


pax_apt_all <- import_airport_data(urls)
pax_cie_all <- import_compagnies_data(urls)
pax_lsn_all <- import_liaison_data(urls)

airports_location <- st_read(urls$geojson$airport)

liste_aeroports <- unique(pax_apt_all$apt)
default_airport <- liste_aeroports[1]

# OBJETS NECESSAIRES A L'APPLICATION ------------------------

trafic_aeroports <- pax_apt_all %>%
  mutate(trafic = apt_pax_dep + apt_pax_tr + apt_pax_arr) %>%
  filter(apt %in% default_airport) %>%
  mutate(
    date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")
  )

