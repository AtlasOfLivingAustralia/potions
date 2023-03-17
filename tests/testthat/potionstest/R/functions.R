brewtest_nopkg <- function(.data){
  brew(.data)
}

brewtest_pkg <- function(.data){
  brew(.data, .pkg = "potionstest")
}

pourtest_nopkg <- function(){
  pour()
}

pourtest_pkg <- function(){
  pour(.pkg = "potionstest")
}

draintest_nopkg <- function(){
  drain()
}

draintest_pkg <- function(){
  drain(.pkg = "potionstest")
}