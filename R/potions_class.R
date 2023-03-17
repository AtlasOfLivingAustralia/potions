#' Methods for `potions` data
#' 
#' Data storage for information passed to `{potions}`.
#' @name potions-class
#' @export
create_potions <- function(){
  potions_default <- list(
    mapping = list(
      current_slot = c(),
      packages = c()),
    slots = list(),
    packages = list())
  class(potions_default) <- "potions"
  return(potions_default)
}

# NOTE: the object that stores user information has class `potions`
# it stores objects of class `list`