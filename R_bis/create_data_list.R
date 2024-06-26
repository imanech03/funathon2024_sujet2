create_data_list <- function(source_file){
  catalogue <- yaml::read_yaml(source_file)
  return(catalogue)
}

