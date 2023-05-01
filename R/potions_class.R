#' Methods for `potions` data
#' 
#' This package stores data in a list-like format, named class `potions`. It 
#' contains three entries: `slots` contains data stored in 'interactive' mode;
#' `packages` contains data from packages built using `potions`; and `mapping`
#' stores data to understand the contents of the other two slots.
#' @name potions-class
#' @returns In the case of `create_potions()`, an empty `potions` object. 
#' `print.potions()` displays a `potions` object using `lobstr::tree()`.
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