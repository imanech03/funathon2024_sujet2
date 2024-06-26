renv::restore()
library(yaml)
read_yaml("sources.yml")
library(readr)
libary(dplyr)
library(sf)
library(leaflet)
library(leaflet.providers)

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
  colnames(data) <- tolower(colnames(data))
  return(data)
}


## Airports ----

# Modification des types des colonnes


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

########## ESSAYER D'AUTOMATISER LA FONCTION ########## 

## Liaisons ----


import_liaison_data <- function(source_file) {
  
  col_types = cols(
    ANMOIS = col_character(),
    LSN = col_character(),
    LSN_DEP_NOM = col_character(),
    LSN_ARR_NOM = col_character(),
    LSN_SCT = col_character(),
    LSN_FSC = col_character(),
    .default = col_double()
  )
  
  data <- readr::read_csv2(unlist(source_file$liaisons), col_types = col_types)
  data <- clean_dataframe(data)
  
  return(data)
}

liaison <- import_liaison_data(urls)

## Compagnies ----


import_compagnies_data <- function(source_file) {
  
  col_types = cols(
    ANMOIS = col_character(),
    CIE = col_character(),
    CIE_NOM = col_character(),
    CIE_NAT = col_character(),
    CIE_PAYS = col_character(),
    .default = col_double()
  )
  
  data <- readr::read_csv2(unlist(source_file$compagnies), col_types = col_types)
  data <- clean_dataframe(data)
  
  return(data)
}

compagnies <- import_compagnies_data(urls)

# Localisation des aéroports ---- 

airports_location <- st_read(urls$geojson$airport)

# Carte

leaflet(airports_location) %>%
  addTiles() %>%
  addMarkers(popup = ~Nom)

source("R_bis/import_data.R")

# 4.3 Valorisation 1: Le trafic par aéroport ----

# On crée une nouvelle variable trafic

pax_apt_all$trafic <- pax_apt_all$apt_pax_dep + pax_apt_all$apt_pax_tr + pax_apt_all$apt_pax_arr

# On crée une variable date

pax_apt_all$date  <- as.Date(paste(pax_apt_all$annee, pax_apt_all$mois, "01", sep = "-"))

# Filtre sur un aéroport

aeroport_FMCZ <- pax_apt_all %>% filter(apt == 'FMCZ')

# Graphique simple statique

graph <- ggplot(aeroport_FMCZ, aes(date, trafic)) +
  geom_line()

# Graphique dynamique

graph_2 <- plot_ly(aeroport_FMCZ, x=~date, y = ~trafic, type = 'scatter', mode = 'lines+markers')

# On crée une fonction pour produire le graphique dynamique pour n'importe quel aéroport

plot_airport_line <- function(df, aeroport) {
  
  aeroport <- df %>% filter((apt == aeroport))
  
  graph <-plot_ly(aeroport, x=~date, y = ~trafic, type = 'scatter', mode = 'lines+markers')
  
  return(graph)
}

# Exemples d'utilisation

plot_airport_line(pax_apt_all, "FMCZ")

plot_airport_line(pax_apt_all, "LFBH")

# 4.4 Valorisation 2: Tableau HTML pour afficher des données ----

YEARS_LIST  <- as.character(2018:2022)
MONTHS_LIST <- 1:12

df <- pax_apt_all %>%
  filter(mois == "7",
         annee == "2019")

create_data_from_input <- function(df, year, month){
  df <- pax_apt_all %>%
    filter(mois == month,
           annee == year)
  return(df)
}


create_data_from_input(pax_apt_all, "7", "2019")

# Exercice 2

stats_aeroports <- pax_apt_all %>% 
  group_by(apt, apt_nom) %>% 
  mutate(nb_pax_dep = sum(apt_pax_dep),
         nb_pax_arr = sum(apt_pax_arr),
         nb_pax_tr = sum(apt_pax_tr),
         nb_pax_total = sum(trafic)) %>% 
  arrange(desc(nb_pax_total)) %>% 
  select(apt, apt_nom, nb_pax_dep, nb_pax_arr, nb_pax_tr, nb_pax_total) %>% 
  distinct()

summary_stat_airport  <- function(df) {
  
  df %>% 
    group_by(apt, apt_nom) %>% 
    mutate(nb_pax_dep = sum(apt_pax_dep),
           nb_pax_arr = sum(apt_pax_arr),
           nb_pax_tr = sum(apt_pax_tr),
           nb_pax_total = sum(trafic)) %>% 
    arrange(desc(nb_pax_total)) %>% 
    select(apt, apt_nom, nb_pax_dep, nb_pax_arr, nb_pax_tr, nb_pax_total) %>% 
    distinct() %>% 
    
    return(df)
}

summary_stat_airport(pax_apt_all)


# Nouvelle colonne 

YEARS_LIST  <- as.character(2018:2022)
MONTHS_LIST <- 1:12

year <- YEARS_LIST[1]
month <- MONTHS_LIST[1]

stats_aeroports_table <- stats_aeroports %>% mutate(name_clean = paste0(str_to_sentence(apt_nom), " _(", apt, ")_")
) %>%
  select(name_clean, everything())

# Création du tableau 

start_date <- "2018-01-01"
end_date <- "2022-12-01"

library(gt)

table_aeroports <- gt(stats_aeroports_table)

# Retirer les colonnes apt et apt_nom

table_aeroports <- table_aeroports %>% 
  cols_hide(columns = starts_with("apt")) 

# Formatter les colonnes numériques
table_aeroports <- table_aeroports %>% 
  fmt_number(columns = -1,
             sep_mark = ",") # Ligne de code facultative

# Mettre en forme la colonne "name_clean"

table_aeroports <- table_aeroports %>% 
  fmt_markdown(columns = "name_clean")

table_aeroports <- table_aeroports %>%
  cols_label(
    name_clean = md("**Aéroport**"),
    nb_pax_dep = md("**Départs**"),
    nb_pax_arr = md("**Arrivée**"),
    nb_pax_tr = md("**Transit**"),
    nb_pax_total = md("**Total**")
  ) %>%
  tab_header(
    title = md("**Statistiques de fréquentation**"),
    subtitle = md("_Classement des aéroports_")
    )%>%
  tab_source_note(source_note = md("_Source: DGAC, à partir des données sur data.gouv.fr_")) %>% 
  tab_options(heading.subtitle.font.size = px(18)) %>% 
  opt_interactive()


# 4.5 Valorisation 3 : Carte des aéroports ----


