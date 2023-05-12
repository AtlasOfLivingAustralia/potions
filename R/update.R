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
#' @importFrom purrr list_merge
#' @importFrom purrr list_modify
#' @noRd
#' @keywords Internal
update_data <- function(data, 
                        provided, 
                        .slot = NULL, 
                        .pkg = NULL, 
                        method){
  
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
  if(is.null(current_list)){
    method <- "overwrite"
  }
  final_list <- switch(method,
    "modify" = list_modify(current_list, !!!provided),
    "merge" = list_merge(current_list, !!!provided),
    "leaves" = leaf_modify(current_list, provided),
    "overwrite" = provided,
    {abort("Argument to `method` not recognised")}
  )
  pluck(data, !!!index_vector) <- final_list
  return(data)
}


#' Modify only the leaves of a list 
#' @importFrom rrapply rrapply
#' @noRd
#' @keywords Internal
leaf_modify <- function(old, new){
  names_df <- rrapply(old, how = "melt")
  names_df <- names_df[, grepl("^L[[:digit:]]{1,2}$", colnames(names_df)), 
                       drop = FALSE]
  # get vector of leaf names
  leaf_lookup <- data.frame(
    index = apply(names_df, 1, function(a){max(which(!is.na(a)))}),
    name = apply(names_df, 1, function(a){a[max(which(!is.na(a)))]}))
  
  for(i in seq_len(nrow(leaf_lookup))){
    old <- leaf_overwrite(names_df[i, ], leaf_lookup[i, ], old, new)
  }

   return(old)
}

#' Modify only the leaves of a list 
#' @noRd
#' @keywords Internal
leaf_overwrite <- function(df, lookup, old, new){
  if(any(names(new) == lookup$name)){
    address <- unlist(df[, seq_len(lookup$index)])
    pluck(old, !!!address) <- new[[lookup$name]]
  }
  return(old)
}