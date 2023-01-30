#' Clear package options
#' 
#' No arguments required
#' 
#' @export

drain <- function(){
  slot_name <- getOption("potions_slot_name")
  x <- list(x = NULL)
  names(x) <- slot_name
  options(x)
}