#' Get config data from a file
#' 
#' In test. Looks for a file in the working directory
#' @param file string: path to file. Readable formats are `.yml` and `.json`.
#' @importFrom rlang abort
#' @importFrom yaml read_yaml
#' @importFrom jsonlite read_json
#' @export
read_config_file <- function(file){
  if(grepl(".y(a?)ml$", file)){
    read_yaml(file = file, readLines.warn = FALSE)
  }else if(grepl(".json$", file)){
    read_json(file)
  }else{
    abort("file format not recognised")
  }
}