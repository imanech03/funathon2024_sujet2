clean_dataframe <- function(data) {
  data$annee <- substring(data$ANMOIS, 1, 4)
  data$mois <- as.integer(ifelse(substring(data$ANMOIS, 5) == "0", # as.integer() nécessaire pour éliminer le "0"
                                 substring(data$ANMOIS, 6),
                                 substring(data$ANMOIS, 5, 6)))
  colnames(data) <- tolower(colnames(data))
  return(data)
}
