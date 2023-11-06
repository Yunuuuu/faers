#' Combine a list FAERS objects into one
#'
#' Packed all [FAERSascii] or [FAERSxml] objects into a single [FAERSascii] or
#' [FAERSxml] object
#' @param x A list of [FAERSxml] or [FAERSascii] objects.
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
#' faers_combine(list(data1, data2))
#' @export
faers_combine <- function(x) {
    assert_(x, is.list, "a list")
    l <- length(x)
    if (l == 0L) {
        cli::cli_abort("empty list")
    }
    type <- NULL
    for (allowed_type in c("ascii", "xml")) {
        if (all(is_matched_faers(x, allowed_type))) {
            type <- allowed_type
        }
    }
    if (is.null(type)) {
        cli::cli_abort(
            "All elements of {.arg x} must be of the same class, {.cls FAERSascii} or {.cls FAERSxml}"
        )
    }
    if (l == 1L) {
        return(x[[1L]])
    }
    cli::cli_alert("Combining all {l} FAERS Quarterly {type} Data files")
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
        cli::cli_warn("Duplicated periods combined")
    }
    period <- unique(period)
    out <- switch(type,
        ascii = combine_faers_ascii(x),
        xml = combine_faers_xml(x)
    )
    out@year <- period$year
    out@quarter <- period$quarter
    methods::validObject(out)
    out
}

is_matched_faers <- function(x, type) {
    vapply(x, methods::is, logical(1L), class2 = paste0("FAERS", type))
}

combine_faers_ascii <- function(x) {
    data_list <- lapply(faers_ascii_file_fields, function(field) {
        data <- data.table::rbindlist(
            lapply(x, function(obj) obj@data[[field]]),
            fill = TRUE, use.names = TRUE
        )
        # In version < 1.9.8 default was key(x).
        unique(data, by = seq_along(data))
    })
    data.table::setattr(data_list, "names", faers_ascii_file_fields)
    methods::new("FAERSascii",
        data = data_list,
        deletedCases = unique(
            unlist(lapply(x, faers_deleted_cases), use.names = FALSE)
        ),
        year = 0L,
        quarter = "q1"
    )
}

combine_faers_xml <- function(x) {
    data <- data.table::rbindlist(
        lapply(x, function(obj) obj@data),
        fill = TRUE, use.names = TRUE
    )
    data <- unique(data, by = seq_along(data))
    methods::new("FAERSxml", data = data, year = 0L, quarter = "q1")
}
