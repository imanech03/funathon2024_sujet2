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