create_data_from_input <- function(df, month, year){
  df <- pax_apt_all %>%
    filter(mois == month,
           annee == year)
  return(df)
}

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