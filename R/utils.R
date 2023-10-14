`%||%` <- function(x, y) if (is.null(x)) y else x

#' test whether from legacy aers, before 2012q3
#' @noRd 
is_from_laers <- function(years, quarters) {
    is_before_period(years, quarters, 2012L, "q3")
}

#' included the specified one
#' @noRd
is_before_period <- function(years, quarters, y, q) {
    years < y | (years == y & quarters <= q)
}

use_indices <- function(i, names, arg = rlang::caller_arg(i), call = rlang::caller_env()) {
    if (anyNA(i)) {
        cli::cli_abort(
            sprintf("%s cannot contain `NA`", style_arg(arg)),
            call = call
        )
    }
    if (is.character(i)) {
        outbounded_values <- setdiff(i, names)
        if (length(outbounded_values)) {
            cli::cli_abort(sprintf(
                "%s contains outbounded values: {outbounded_values}",
                style_arg(arg)
            ), call = call)
        }
    } else if (is.numeric(i)) {
        if (any(i < 1L) || any(i > length(names))) {
            cli::cli_abort(sprintf(
                "%s contains out-of-bounds indices", style_arg(arg)
            ), call = call)
        }
    } else {
        cli::cli_abort(sprintf(
            "%s must be an atomic numeic/character",
            style_arg(arg)
        ), call = call)
    }
    i
}

fda_url <- "https://fis.fda.gov"
faers_file_types <- c("ascii", "xml")
faers_file_quarters <- c("q1", "q2", "q3", "q4")
faers_ascii_file_fields <- c("demo", "drug", "indi", "ther", "reac", "rpsr", "outc")
