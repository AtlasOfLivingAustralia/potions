#' Set up potions for easy data retrieval
#' 
#' Start-up function to place a list into `options` with a specified 
#' slot name.
#' @param .data a list containing data to be stored via `options()`.
#' @param slot string: optional name to mandate where data is stored. Defaults 
#' to "default".
#' @param pkg string: if using {potions} within a package development process,
#' use this argument instead of `slot`.
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom rlang trace_back
#' @details 
#' The default method is to use `brew` and set either `pkg` or `slot`, but not
#' both. Alternatively you can use `brew_package()` or `brew_interactive()`.
#' 
#' If the user repeatly calls `brew()`, later list entries overwrite early 
#' entries. Whole lists are not overwritten unless all top-level entry names 
#' match. All data is stored in options("potions-pkg")
#' @export
brew <- function(.data, slot, pkg){
  has_slot <- !missing(slot)
  has_pkg <- !missing(pkg)
  if(has_slot){
    if(has_pkg){
      abort("Both `slot` and `pkg` have been set; please choose one")
    }
    brew_interactive(.data, slot)
  }else{
    if(has_pkg){
      brew_package(.data, pkg)
    }else{
      brew_interactive(.data, slot = "default")
    }
  }
}

#' @rdname brew
#' @export
brew_package <- function(.data, pkg){
  # check data
  check_potions_data(.data)
  check_is_character("pkg", pkg)
  
  # get potions object and update with pkg data
  current_list <- check_potions_storage() |>
    update_package_names(pkg) |>
    update_package_data(provided = .data, pkg = pkg)
  
  # save to `options()`
  options(list("potions-pkg" = current_list))
}

#' @rdname brew
#' @export
brew_interactive <- function(.data, slot){
  # check data
  check_potions_data(.data)
  check_is_character("slot", slot)
  
  # get potions object and update with pkg data
  current_list <- check_potions_storage() |>
    update_default_name(slot) |>
    update_slot_data(provided = .data, slot = slot)
  
  # save to `options()`
  options(list("potions-pkg" = current_list))
}