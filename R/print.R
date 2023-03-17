#' @rdname potions-class
#' @param x An object of class `potions`
#' @param ... Any further arguments to `print()`
#' @importFrom utils str
#' @export
print.potions <- function(x, ...){
  str(x, max.levels = 2)
}