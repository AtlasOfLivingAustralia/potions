#' Clear package options
#' 
#' Clear options of previously specified content
#' @param slot_name (optional) slot to clear from options()
#' @export

drain <- function(slot_name){
  if(missing(slot_name)){
    slot_name <- getOption("potions_slot_name")
    if(is.null(slot_name)){
      stop("`slot_name` is missing, with no default")
    }
  }
  x <- list(x = NULL)
  names(x) <- slot_name
  options(x)
}