`%||%` <- function(x, y) if (is.null(x)) y else x

pkg_nm <- function() {
    utils::packageName(topenv(environment()))
}

# https://github.com/Rdatatable/data.table/issues/3214#issuecomment-462490046
dt_shallow <- function(x) {
    x[TRUE]
}

assert_internet <- function(call = rlang::caller_env()) {
    if (!curl::has_internet()) {
        cli::cli_abort("No internet", call = call)
    }
}

has_name <- function(x, name) {
    any(name == rlang::names2(x))
}

fda_host <- function(prefix = "www") {
    sprintf("https://%s.fda.gov", prefix)
}

faers_file_format <- c("ascii", "xml")
faers_file_quarters <- c("q1", "q2", "q3", "q4")
faers_ascii_file_fields <- c("demo", "drug", "indi", "reac", "ther", "rpsr", "outc")
