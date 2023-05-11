#' Function to add new package name to `potions` object
#' @noRd
#' @keywords Internal
update_package_names <- function(data, .pkg){
  if(!any(data$mapping$packages == .pkg)){
    data$mapping$packages <- unique(c(data$mapping$packages, .pkg))
  }
  return(data)
}

#' Function to update the 'current' slot in a `potions` object
#' @noRd
#' @keywords Internal
update_default_name <- function(data, .slot){
  data$mapping$current_slot <- .slot
  return(data)
}

#' Function to update data in `potions`
#' 
#' This is basically the workhorse function underneath `brew()`
#' @importFrom rlang abort
#' @importFrom purrr pluck
#' @importFrom purrr pluck<-
#' @importFrom purrr list_modify
#' @noRd
#' @keywords Internal
update_data <- function(data, 
                        provided, 
                        .slot = NULL, 
                        .pkg = NULL, 
                        leaves = FALSE){
  
  # set error catching behavior for each type
  # in practice this shouldn't be needed, as `update_data()` is only ever called internally
  # but this should help debug in case of error
  if(is.null(.slot) && is.null(.pkg)){
    abort("Neither `.slot` nor `.pkg` provided to `update_data()`; please choose one")
  }
  if(!is.null(.slot) && !is.null(.pkg)){
    abort("Both `.slot` and `.pkg` provided to `update_data()`; please choose one")
  }else{
    # now set a vector for drilling down into lists using `search_down()`
    if(!is.null(.slot)){
      index_vector <- c("slots", .slot)
    }else{
      index_vector <- c("packages", .pkg)
    }
  }
  
  # if some information is missing, return the other
  # Note if both are missing, `brew()` should have errored by now
  if(is.null(provided)){
    return(data)
  }
  if(is.null(data)){
    return(provided)
  }
  
  # get currently stored data from `options()`
  current_list <- pluck(data, !!!index_vector)
  
  # update this data with user-supplied information
  if(leaves){
    browser()

    update_leaves(data)
  }else{
    if(is.null(current_list)){
      pluck(data, !!!index_vector) <- provided
    }else{
      pluck(data, !!!index_vector) <- list_modify(current_list, !!!provided)
    }
  }
  
  return(data)
}