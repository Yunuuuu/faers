#' Combine a list FAERS objects into one
#'
#' Packed all [FAERSascii] or [FAERSxml] objects into a single [FAERSascii] or
#' [FAERSxml] object. Note: Unique reports will not be removed.
#' @param ... Multiple [FAERSxml] or [FAERSascii] objects or a list containing
#' [FAERSxml] or [FAERSascii] objects.
#' @return A [FAERSxml] or [FAERSascii] object.
#' @examples
#' # the files included in the package are sampled
#' data1 <- faers_parse(
#'     system.file("extdata", "aers_ascii_2004q1.zip", package = "faers"),
#'     compress_dir = tempdir()
#' )
#' data2 <- faers_parse(
#'     system.file("extdata", "faers_ascii_2017q2.zip", package = "faers"),
#'     compress_dir = tempdir()
#' )
#' faers_combine(data1, data2)
#' @export
faers_combine <- function(...) {
    x <- wrap_faers(...)
    l <- length(x)
    if (l == 0L) {
        cli::cli_abort("empty list")
    }
    if (l == 1L) {
        return(x[[1L]])
    }
    type <- check_faers_list_type(x)
    cli::cli_alert("Combining all {l} {.cls FAERS{type}} Data")
    # combine period data
    period <- data.table(
        year = unlist(lapply(x, function(obj) obj@year),
            recursive = FALSE, use.names = FALSE
        ),
        quarter = unlist(lapply(x, function(obj) obj@quarter),
            recursive = FALSE, use.names = FALSE
        )
    )
    if (anyDuplicated(period)) {
        cli::cli_abort("Duplicated periods combined")
    }
    data <- switch(type,
        ascii = combine_faers_ascii_data(x),
        xml = combine_faers_xml_data(x)
    )
    switch(type,
        ascii = methods::new("FAERSascii",
            data = data,
            deletedCases = unique(
                unlist(lapply(x, faers_deleted_cases), use.names = FALSE)
            ),
            year = period$year, quarter = period$quarter
        ),
        xml = methods::new("FAERSxml",
            data = data,
            year = period$year, quarter = period$quarter
        )
    )
}

combine_faers_ascii_data <- function(x) {
    data_list <- lapply(faers_ascii_file_fields, function(field) {
        data.table::rbindlist(
            lapply(x, function(obj) obj@data[[field]]),
            fill = TRUE, use.names = TRUE
        )
    })
    data.table::setattr(data_list, "names", faers_ascii_file_fields)
    data_list
}

combine_faers_xml_data <- function(x) {
    data.table::rbindlist(
        lapply(x, function(obj) obj@data),
        fill = TRUE, use.names = TRUE
    )
}

check_faers_list_type <- function(lst, call = rlang::caller_env()) {
    type <- NULL
    for (allowed_type in c("ascii", "xml")) {
        if (all(is_matched_faers(lst, allowed_type))) {
            type <- allowed_type
        }
    }
    if (is.null(type)) {
        cli::cli_abort(
            "All elements in {.arg ...} must be of the same class, {.cls FAERSascii} or {.cls FAERSxml}",
            call = call
        )
    }
    type
}

is_matched_faers <- function(x, type) {
    vapply(x, methods::is, logical(1L), class2 = paste0("FAERS", type))
}

wrap_faers <- function(..., call = rlang::caller_env()) {
    if (is.list(..1)) {
        if (...length() == 1L) {
            ..1
        } else {
            cli::cli_abort(
                "You can only supply a single list or multiple {.cls FAERS} objects into {.arg ...}",
                call = call
            )
        }
    } else {
        list(...)
    }
}
