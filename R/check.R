#' check if `potions` data exists already, and if not create it
#' @return an object of class `potions`
#' @keywords internal
#' @noRd
check_potions_storage <- function(){
  current_list <- getOption("potions-pkg")
  if(is.null(current_list)){
    current_list <- create_potions()
  }
  return(current_list)
}

#' simple character check
#' 
#' Also returns random slot name if one is not provided
#' @importFrom stringi stri_rand_strings
#' @keywords internal
#' @noRd
check_is_character <- function(x, fill = FALSE){
  if(missing(x)){
    if(fill){
      result <- stri_rand_strings(n = 1, length = 10)
    }else{
      abort("Argument `x` is missing, with no default")
    }
  }else{
    result <- x
  }
  if(!inherits(result, "character")){
    abort("Non-character value supplied")
  }else{
    return(result)
  }
}

#' Function used in `brew` to decide whether to use .pkg or .slot
#' @return a `list` with up to two entries
#' @keywords internal
#' @noRd
check_existing_slots <- function(){
  lookup <- getOption("potions-pkg")
  if(is.null(lookup)){
    return(list(method = "all_empty"))
  }else{
    if(!is.null(lookup$mapping$current_slot)){
      return(list(method = ".slot", 
                  value = lookup$mapping$current_slot))
    }else if(!is.null(lookup$mapping$packages)){
      return(list(method = ".pkg", 
                  value = lookup$mapping$packages[[length(lookup$mapping$packages)]]))
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
  result <- list(within = FALSE)
  if(!is.null(.data)){
    if(!is.null(.data$mapping$packages)){
      next_pkg <- trace[trace != "potions"]
      if(length(next_pkg) > 0){
        if(any(.data$mapping$packages == rev(next_pkg)[1])){
          result <- list(within = TRUE, pkg = next_pkg)
        }
      }
    }
  }
  return(result)
}

## alternative or additional code:    
# pkg_info_stored <- any(.data$mapping$packages == trace[])
# pkg_loaded <- isNamespaceLoaded(pkg) # possibly redundant
# pkg_in_trace <- any(trace$namespace == pkg)
# return(pkg_info_stored & pkg_loaded & pkg_in_trace)

## NOTES on above logic
# x <- rev(trace_back()$namespace) # returns tree going upwards
# you can then see whether x[-1] (or x[x != "potions"])includes any of the package names stored in 
# getOption("potions-package")[["mapping"]][["packages"]]
## AND sessionInfo(package = pkg_name) returns something sensible # NOTE: errors when missing
# if this is the case, then you are within a package and the default should be
# pour(pkg = "slot_name")
## note that in this case, pour() will default to package name, and index within it
# you will therefore need to add attr(x, "pkg") <- slot_name for clarity and debugging reasons

#' internal checks for `pour_package`
#' @importFrom rlang abort
#' @keywords internal
#' @noRd
check_pour_package <- function(.pkg){
  # ensure a package name is given
  if(missing(.pkg)){
    abort("Argument `.pkg` is missing, with no default")
  }
  
  # check any data has been provided
  all_data <- getOption("potions-pkg")
  if(is.null(all_data)){
    bullets <- c("No data stored by `potions`",
                 i = "try using `brew()")
    abort(bullets)
  }else{
    # check package has been listed as having data
    if(!any(all_data$mapping$packages == .pkg)){
      abort(paste0("No data stored for package ", .pkg))
    }else{
      return(all_data$packages[[.pkg]])
    }
  }
}

#' Internal checks for `pour_interactive`
#' 
#' Note that - unlike packages - slots can be set to run on default only. This
#' means that .slot is not always required
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @keywords internal
#' @noRd
check_pour_interactive <- function(.slot){
  
  # check any data has been provided
  all_data <- getOption("potions-pkg")
  if(is.null(all_data)){
    bullets <- c("No data stored by `potions`",
                 i = "try using `brew()")
    abort(bullets)
  }else{
    if(missing(.slot)){
      if(length(all_data$slots) > 1){
        if(any(names(all_data$slots) == all_data$mapping$current_slot)){
          return(all_data$slots[[all_data$mapping$current_slot]])
        }else{
          bullets <- c("Multiple slots available, but `current_slot` is not named",
                       i = "please specify `.slot` to continue")
          abort(bullets)
        }
      }else if(length(all_data$slots) == 1L){
        return(all_data$slots[[1]])
      }else{
        bullets <- c("No data stored by `potions` using interactive mode",
                     i = "try using `brew()`")
        inform(bullets)
        return(NULL)
      }
    }else{ # i.e. `.slot` is given
      if(any(names(all_data$slots) == .slot)){
        return(all_data$slots[[.slot]])
      }else{
        bullets <- c("Named .slot is not stored by `potions`",
                     i = "please specify a valid `.slot` to continue")
        abort(bullets)
      }     
    }
  }
}

# # check data provided to {potions}
# # Duplicates functionality of `check_potions_storage()`
# Q: Is this used?
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
