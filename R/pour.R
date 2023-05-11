#' Retrieve information stored using `potions::brew()`
#' 
#' This is the main function that most users will call on. It retrieves data 
#' from a `potions` object stored using `brew()`. The UI for this function is 
#' based on the `here` package, in that it uses list names separated by commas 
#' to navigate through nested content. It differs from `here` in not requiring
#' those names to be quoted.
#' 
#' @param ... string: what slots should be returned
#' @param .slot string: Optional manual override to default slot
#' @param .pkg string: Optional manual override to default package
#' @importFrom rlang trace_back
#' @details  Providing multiple arguments to `...` brings back nested values, 
#' i.e. `pour("x", "y")` is for the case of an object structured as 
#' `list(x = list(y = 1))`, rather than `list(x = 1, y = 2)`. For the latter 
#' case it would be necessary to call with either no arguments 
#' (`unlist(pour())`), or for greater control, call pour multiple times 
#' specifying different entries each time (e.g. `z <- c(pour("x"), pour("y"))`).
#' 
#' Additional functions are provided in case greater specificity is required. 
#' `pour_interactive(.slot = ...)` is synonymous with `pour(.slot = ...)`, while 
#' `pour_package(.pkg = ...)` is synonymous with `pour(.pkg = ...)`. 
#' `pour_all()` is a shortcut for `getOption("potions-pkg")`; i.e. to show all
#' data stored using `potions` by any package or slot, and does not accept any
#' arguments.
#' @returns If no arguments are passed to `...`, returns a `list` from the 
#' default slot. If `...` is supplied (correctly), then returns a `vector` of 
#' values matching those names.
#' @examples 
#' # first import some data
#' brew(x == 1, y == list(a = 2, b = 3))
#' 
#' # get all data
#' pour()
#' 
#' # get only data from slot x
#' pour(x)
#' 
#' # get nested data
#' pour(y, a)
#' 
#' # optional clean-up
#' drain()
#' @export

pour <- function(..., .slot, .pkg){

  if(missing(.slot) & missing(.pkg)){
    package_check <- trace_back()$namespace |> 
                     check_within_pkg()
    if(package_check$within){
      pour_package(..., .pkg = package_check$pkg)
    }else{
      pour_interactive(...)
    }
  }else{
    if(missing(.slot)){ # i.e. .pkg is given, but not .slot
      pour_package(..., .pkg = .pkg)
    }else{ # .slot is given, but not .pkg
      pour_interactive(..., .slot = .slot)
    }
  }
}

#' @rdname pour
#' @importFrom rlang enquos
#' @importFrom purrr pluck
#' @export
pour_package <- function(..., .pkg){
  dots <- list(...)
  data <- check_pour_package(.pkg)
  if(length(dots) > 0){
    check_is_character(dots)
    pluck(data, !!!dots)
  }else{
    return(data)
  }
}

#' @rdname pour
#' @importFrom rlang enquos
#' @importFrom purrr pluck
#' @export
pour_interactive <- function(..., .slot){
  dots <- list(...)
  data <- check_pour_interactive(.slot)
  if(length(dots) > 0){
    check_is_character(dots)
    pluck(data, !!!dots)
  }else{
    return(data)
  }
}

#' @rdname pour
#' @export
pour_all <- function(){
  # check any data has been provided
  all_data <- getOption("potions-pkg")
  if(is.null(all_data)){
    bullets <- c("No data stored by `potions`",
                 i = "try using `brew()`")
    abort(bullets)
  }else{
    class(all_data) <- "potions"
    return(all_data)
  }
}