# check if `potions` data exists already, and if not create it
# returns an object of class `potions`
check_potions_storage <- function(){
  current_list <- getOption("potions-pkg")
  if(is.null(current_list)){
    current_list <- create_potions()
  }
  return(current_list)
}


# # check data provided to {potions}
# # Duplicates functionality of `check_potions_storage()`
check_potions_data <- function(.data){
  if(missing(.data)){
    abort("Argument `.data` is missing, with no default")
  }
}
#   # if(!inherits(.data, "list")){
#   #  abort("Argument `.data` must be a list") # should this be enforced?
#     # Could be useful to allow bespoke classes from different packages
#   # }
# }


#' simple character check
#' 
#' Also returns random slot name if one is not provided
#' @importFrom stringi stri_rand_strings
#' @keywords internal
#' @noRd
check_is_character <- function(x, fill = FALSE){
  if(missing(x)){
    if(fill){
      x <- stri_rand_strings(n = 1, length = 10)
      return(x)
    }else{
      abort("Argument `x` is missing, with no default")
    }
  }
  if(!inherits(x, "character")){
    abort("Non-character value supplied")
  }
}


# function used in `brew` to decide whether to use .pkg or .slot
# returns a `list` with up to two entries
check_existing_slots <- function(){
  lookup <- getOption("potions-pkg")
  if(is.null(lookup)){
    return(list(method = "all_empty"))
  }else{
    if(!is.null(lookup$current_slot)){
      return(list(method = ".slot", value = lookup$current_slot))
    }else if(!is.null(lookup$packages)){
      return(list(method = ".pkg", value = lookup$packages[[length(lookup$packages)]]))
    }else{
      return(list(method = "all_empty"))
    }
  }
}

#' Function to decide whether to default to choosing package-level options
#' 
#' This is in dev. It is an attempt to determine, at the point that `pour()`
#' is called, whether the  call is coming from within another package. If so, 
#' then this function will flag TRUE, and `pour()` will call from 
#' `getOption("potions-pkg")$packages[[.data$mapping$packages]]`,
#' rather than the default for the interactive sessions which is 
#' `getOption("potions-pkg")$slots[[.data$mapping$current_slot]]`.
#' @param pkg A package name (string)
#' @param trace result from `rlang::trace_back()`
#' @keywords internal
#' @noRd
check_within_pkg <- function(trace){
  .data <- getOption("potions-pkg")
  if(is.null(.data)){
    return(FALSE)
  }else{
    pkg <- .data
    pkg_info_stored <- any(.data$mapping$packages == pkg)
    pkg_loaded <- isNamespaceLoaded(pkg) # possibly redundant
    pkg_in_trace <- any(trace$namespace == pkg)
    return(pkg_info_stored & pkg_loaded & pkg_in_trace)
  }
}

## NOTES on above logic
# x <- rev(trace_back()$namespace) # returns tree going upwards
# you can then see whether x[-1] (or x[x != "potions"])includes any of the package names stored in 
# getOption("potions-package")[["mapping"]][["packages"]]
## AND sessionInfo(package = pkg_name) returns something sensible # NOTE: errors when missing
# if this is the case, then you are within a package and the default should be
# pour(pkg = "slot_name")
## note that in this case, pour() will default to package name, and index within it
# you will therefore need to add attr(x, "pkg") <- slot_name for clarity and debugging reasons


# Early test fun to get environment information
# 
# Purpose of this function is to develop methods to determine whether 
# potions is being called within another package
# @importFrom rlang trace_back
# env_check <- function(){
# list(
#   environment(),
#   parent.env(environment()),
#   parent.env(parent.env(environment())),
#   parent.env(parent.env(parent.env(environment()))),
#   ls.str(),
#  trace_back() # NOTE: this is probably the only bit we'll end up using
# )
# }