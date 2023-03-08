brewtest_nopkg <- function(.data){
  brew(.data)
}

brewtest_pkg <- function(.data){
  brew(.data, .pkg = "potionstest")
}

pourtest_nopkg <- function(){
  pour()
}