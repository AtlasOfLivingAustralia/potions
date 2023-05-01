#' Get configuration data from a file
#' 
#' This is primarily an internal function for importing configuration 
#' information from a file. It is called by `brew()`, and detects `.yml` or 
#' `.json` files by their file extentions; all the actual work is done by 
#' `yaml::read_yaml` and `jsonlite::read_json` respectively. It is available as 
#' an exported function so that users can check their data is being imported 
#' correctly, and for developers who may wish to intercept configuration files 
#' for checking purposes.
#' @param file string: path to file. Readable formats are `.yml` and `.json`.
#' @returns A `list` containing data from the specified file.
#' @importFrom rlang abort
#' @importFrom yaml read_yaml
#' @importFrom jsonlite read_json
#' @export
read_config <- function(file){
  if(grepl(".y(a?)ml$", file)){
    read_yaml(file = file, readLines.warn = FALSE)
  }else if(grepl(".json$", file)){
    read_json(file)
  }else{
    abort("file format not recognised")
  }
}