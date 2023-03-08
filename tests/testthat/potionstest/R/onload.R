.onLoad <- function(libname, pkgname) {
  if(pkgname == "potionstest") {
    potions::brew(.pkg = "potionstest")
  }
}