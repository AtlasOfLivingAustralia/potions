#' Set up potions for easy data retrieval
#' 
#' Start-up function to place a list into `options` with a specified 
#' slot name.
#' 
#' @param slot_name string: where should data be stored?
#' @param x a list containing data required at later stages
#' 
#' @export

brew <- function(slot_name, x){
  # check inputs
  if(missing(slot_name)){
    stop("argument `slot_name` is missing, with no default")
  }
  if(missing(x)){
    stop("argument `x` is missing, with no default")
  }
  if(!inherits(slot_name, "character")){
    stop("argument `slot_name` must be a character")
  }
  if(!inherits(x, "list")){
    stop("argument `x` must be a list")
  }
  
  # set options
  options("potions_slot_name" = slot_name)
  x_list <- list(x)
  names(x_list) <- slot_name
  options(x_list)
}