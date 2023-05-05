#' Clear package options
#' 
#' Clear options of previously specified content. In most cases, calling `drain`
#' with no arguments will be sufficient, but the arguments `.slot` and `.pkg`,
#' and their corresponding functions `drain_interactive()` and `drain_package()`
#' are provided in case greater control is needed. This is rarely needed for 
#' packages, but it is possible to manually specify the use of multiple slots 
#' when using `potions::brew()` interactively.
#' @details Note that this function is not vectorized, so passing multiple
#' values to `.slot` or `.pkg` will fail (e.g. `drain(.slot = c("x", "y"))`).
#' Similarly, passing arguments to both `.slot` and `.pkg` will fail.
#' @param .slot (optional) slot to clear from `options()`
#' @param .pkg (optional) package to clear from `options()`
#' @returns This function never returns an object; it is called for its' side-
#' effect of removing data from `options()`.
#' @importFrom rlang trace_back
#' @importFrom purrr zap
#' @export
drain <- function(.slot, .pkg){
  
  if(!missing(.slot) & !missing(.pkg)){
    abort("Both `.slot` and `.pkg` have been set; please choose one")
  }
  
  if(missing(.slot) & missing(.pkg)){
    package_check <- trace_back()$namespace |> 
                     check_within_pkg()
    
    if(package_check$within){
      drain_package(.pkg = package_check$pkg)
    }else{
      drain_interactive()
    }
  }else{
    if(missing(.slot)){ # i.e. .pkg is given, but not .slot
      drain_package(.pkg = .pkg)
    }else{ # .slot is given, but not .pkg
      drain_interactive(.slot = .slot)
    }
  }
}

#' @rdname drain
#' @export
drain_package <- function(.pkg){
  check_is_character(.pkg)
  check_length_one(.pkg)
  current_list <- getOption("potions-pkg")
  if(!is.null(current_list)){
    if(any(names(current_list$packages) == .pkg)){
      # at this point, switch to using `zap` for removing list content
      current_list$packages[[.pkg]] <- list()
    }
    options(list("potions-pkg" = current_list))    
  }
}

#' @rdname drain
#' @export
drain_interactive <- function(.slot){
  current_list <- getOption("potions-pkg")
  if(!is.null(current_list)){
    if(missing(.slot) & !is.null(current_list$mapping$current_slot)){
      .slot <- current_list$mapping$current_slot
    }else{
      abort("no valid slot found")
    }
    # at this point, switch to using `zap` for removing list content
    current_list$slots[[.slot]] <- list()
    options(list("potions-pkg" = current_list))
  }
}