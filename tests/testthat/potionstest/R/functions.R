brewtest_nopkg <- function(...){
  brew(...)
}

brewtest_pkg <- function(...){
  brew(..., .pkg = "potionstest")
}

pourtest_nopkg <- function(...){
  pour(...)
}

pourtest_pkg <- function(...){
  pour(..., .pkg = "potionstest")
}

draintest_nopkg <- function(){
  drain()
}

draintest_pkg <- function(){
  drain(.pkg = "potionstest")
}