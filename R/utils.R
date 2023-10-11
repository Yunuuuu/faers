`%||%` <- function(x, y) if (is.null(x)) y else x

#' Used by `is_installed` and `faers_meta_doc`
#' @noRd 
faers_cache <- new.env()
