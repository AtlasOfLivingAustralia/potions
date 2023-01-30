#' Retrieve a specified slot from options
#' 
#' UI based on here; i.e. uses strings separated by commas
#' 
#' @param ... strings: what slots should be returned
#' 
#' @export

pour <- function(...){
  # get and test inputs
  dots <- list(...)
  dots_check <- unlist(lapply(dots, function(a){inherits(a, "character")}))
  if(any(!dots_check)){
    stop("all arguments to `pour` must be characters")
  }
  
  # use to index list
  slot_name <- getOption("potions_slot_name")
  x <- getOption(slot_name)
  for(i in seq_along(dots)){
    x <- getElement(x, dots[[i]])
  }
  x
}