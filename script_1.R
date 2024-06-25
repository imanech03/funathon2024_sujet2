renv::restore()
library(yaml)
read_yaml("sources.yml")
library(readr)
libary(dplyr)

# Fonction pour envoyer la liste des sources de données

create_data_list <- function(source_file){
  catalogue <- yaml::read_yaml(source_file)
  return(catalogue)
}

# Stockage des données
urls <- create_data_list("sources.yml")


# Importation des bases de données ----

## Fonction pour transformer la colonne ANMOIS en deux colonnes "annee" et "mois"

clean_dataframe <- function(data) {
  data$annee <- substring(data$ANMOIS, 1, 4)
  data$mois <- as.integer(ifelse(substring(data$ANMOIS, 5) == "0", # as.integer() nécessaire pour éliminer le "0"
                                 substring(data$ANMOIS, 6),
                                 substring(data$ANMOIS, 5, 6)))
  return(data)
}

## Airport ----

import_airport_data <- function(source_file) {
  col_types <- readr::cols(
    ANMOIS = readr::col_character(),
    APT = readr::col_character(),
    APT_NOM = readr::col_character(),
    APT_ZON = readr::col_character(),
    .default = readr::col_double()
  )
  
  data <- readr::read_csv2(unlist(source_file$airports), col_types = col_types)
  data <- clean_dataframe(data)
  
  return(data)
}

airport <- import_airport_data(urls)
## 