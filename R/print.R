#' @rdname potions-class
#' @export
print.potions <- function(x, ...){
  str(x, max.levels = 2)
}