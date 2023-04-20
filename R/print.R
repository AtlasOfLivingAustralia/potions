#' @rdname potions-class
#' @param x An object of class `potions`
#' @param ... Any further arguments to `print()`
#' @importFrom lobstr tree
#' @export
print.potions <- function(x, ...){
  tree(x)
}