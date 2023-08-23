#' potions: simple options management
#' 
#' @description 
#' Often it is useful to set bespoke options for a single workflow, or within
#' a single package, without altering global options that influence other 
#' users or packages. This is possible `base::options()` and related functions, 
#' however doing so requires some bespoke knowledge. `potions` makes options 
#' management as easy as possible, by decreasing programmers' cognitive burden 
#' while storing and retrieving information. It does this by following three 
#' guiding principles:
#' 
#' \itemize{
#'   \item{minimalist: `potions` has only three core functions}
#'   \item{laconic: functions use as few characters as possible}
#'   \item{familiar: uses a UI for data retrieval based on the `{here}` package}
#' }
#' In combination, these features should make it easy for users and developers 
#' to manage options using `potions`.
#' @name potions
#' @section Functions:
#' \itemize{
#'    \item{[brew()]: store data in `options()`}
#'    \item{[pour()]: retrieve data from `options()`}
#'    \item{[drain()]: remove data stored using [brew()]}
#'  }
#' @keywords internal
"_PACKAGE"