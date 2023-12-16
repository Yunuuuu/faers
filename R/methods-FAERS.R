#' Methods for FAERS class
#'
#' Utils function for [FAERSascii] class.
#' @param object,.object A [FAERSascii] object.
#' @param ... Other arguments passed to specific methods. For `faers_filter`:
#' other arguments passed to `.fn`.
#' @details
#'  - `faers_get`: Extract a specific field
#'    [data.table][data.table::data.table]. For `reac` and `indi` field, meddra
#'    data will be automatically added if avaliable.
#'  - `faers_mget`: Extract a list of field
#'    [data.table][data.table::data.table]. For `reac` and `indi` field, meddra
#'    data will be automatically added if avaliable.
#'  - `[[`, `$`, and `[`: Extract a specific field
#'    [data.table][data.table::data.table] or a list of field
#'    [data.table][data.table::data.table] from [FAERS] object. Note: this just
#'    extract field data from `@data` slot directly. For usual usage, just use
#'    `faers_get` or `faers_mget`.
#'  - `faers_primaryid`: Extract the `primaryid` from `demo` field.
#'  - `faers_keep`: only keep data from specified `primaryid`. Note: `year`,
#'    `quarter`, `deletedCases` will be kept as the original. So make sure you
#'    didn't filter a whole period FAERS quarterly data, in this way, it's much
#'    better to run [faers].
#'  - `faers_filter`: apply a function to extract the wanted `primaryid`, then
#'    use `faers_keep` to keep data from these primaryids.
#' @return See details.
#' @examples
#' # you must change `dir`, as the file included in the package is sampled
#' data <- faers(2004, "q1",
#'     dir = system.file("extdata", package = "faers"),
#'     compress_dir = tempdir()
#' )
#' faers_get(data, "indi")
#' data[["indi"]]
#' data$indi
#' faers_get(data, "demo")
#' data[["demo"]]
#' data$demo
#' faers_mget(data, c("indi", "drug"))
#' faers_mget(data, c("indi", "demo"))
#' faers_primaryid(data)
#' faers_keep(data, primaryid = sample(faers_primaryid(data), 20L))
#' faers_filter(data, .fn = function(x) {
#'     sample(x$primaryid, 100L)
#' }, .field = "demo")
#' @export
#' @rdname FAERS-methods
methods::setGeneric("faers_get", function(object, ...) {
    methods::makeStandardGeneric("faers_get")
})

#' @param field A string indicates the FAERS fields to use. Only values
#' `r quote_strings(faers_ascii_file_fields)` can be used.
#' @export
#' @method faers_get FAERSascii
#' @rdname FAERS-methods
methods::setMethod("faers_get", "FAERSascii", function(object, field) {
    field <- match.arg(field, faers_ascii_file_fields)
    get_field(object, field)
})

get_field <- function(object, field) {
    out <- dt_shallow(object@data[[field]]) # make a shallow copy
    if (object@standardization && any(field == c("indi", "reac"))) {
        out <- faers_add_meddra(out, object@meddra@hierarchy)
    }
    out
}

faers_add_meddra <- function(data, hierarchy, remove_idx = TRUE) {
    .__idx__. <- data$meddra_hierarchy_idx
    data[, names(hierarchy) := hierarchy[.__idx__.]]
    if (remove_idx) data[, meddra_hierarchy_idx := NULL]
    data[] # in case of not printing
}

#######################################################
#' @export
#' @rdname FAERS-methods
methods::setGeneric("faers_mget", function(object, ...) {
    methods::makeStandardGeneric("faers_mget")
})

#' @param fields A character vector specifying the fields to use. Only values
#' "demo", "drug", "indi", "ther", "reac", "rpsr", and "outc" can be used.
#' @export
#' @method faers_mget FAERSascii
#' @rdname FAERS-methods
methods::setMethod("faers_mget", "FAERSascii", function(object, fields) {
    assert_inclusive(fields, faers_ascii_file_fields)
    out <- lapply(fields, get_field, object = object)
    data.table::setattr(out, "names", fields)
    out
})
utils::globalVariables(c("meddra_hierarchy_idx"))

#######################################################
#' @export
#' @aliases faers_primaryid
#' @rdname FAERS-methods
methods::setGeneric("faers_primaryid", function(object, ...) {
    methods::makeStandardGeneric("faers_primaryid")
})

#' @export
#' @method faers_primaryid FAERSascii
#' @rdname FAERS-methods
methods::setMethod("faers_primaryid", "FAERSascii", function(object) {
    object@data$demo$primaryid
})


#######################################################
#' @param x A [FAERSascii] object.
#' @param i,name Indices specifying elements to extract. For `i`, it will be
#' okay to use integer indices.
#' @export
#' @aliases [,FAERSascii-method
#' @rdname FAERS-methods
methods::setMethod("[", "FAERSascii", function(x, i) {
    x@data[i]
})

#' @export
#' @aliases [[,FAERSascii-method
#' @rdname FAERS-methods
methods::setMethod("[[", "FAERSascii", function(x, i) {
    x@data[[i]]
})

#' @export
#' @aliases $,FAERSascii-method
#' @rdname FAERS-methods
methods::setMethod("$", "FAERSascii", function(x, name) {
    eval(substitute(x@data$name, list(name = rlang::ensym(name))))
})

##############################################################
#' @export
#' @rdname FAERS-methods
methods::setGeneric("faers_keep", function(object, ...) {
    methods::makeStandardGeneric("faers_keep")
})

#' @export
#' @param primaryid An atomic character or integer specifies the reports to
#' keep. If `NULL`, will do nothing.
#' @param invert, A bool. If `TRUE`, will keep reports not in `primaryid`.
#' @method faers_keep FAERSascii
#' @rdname FAERS-methods
methods::setMethod("faers_keep", "FAERSascii", function(object, primaryid = NULL, invert = FALSE) {
    if (is.null(primaryid)) {
        return(object)
    }
    # as all data has a column primaryid, we just rename the variable to use it
    # in the data.table `i`
    .__primaryid__. <- unique(as.character(primaryid))
    if (isTRUE(invert)) {
        object@data <- lapply(object@data, function(x) {
            x[!.__primaryid__., on = "primaryid"]
        })
    } else {
        object@data <- lapply(object@data, function(x) {
            x[.__primaryid__., on = "primaryid", nomatch = NULL]
        })
    }
    object
})

##############################################################
#' @export
#' @rdname FAERS-methods
methods::setGeneric("faers_filter", function(.object, ...) {
    methods::makeStandardGeneric("faers_filter")
})

#' @param .fn A function or formula, accept the field data as the input and
#' return an atomic integer or character of `primaryid` you want to keep or
#' remove based on argument `.invert`.
#'
#'   If a **function**, it is used as is.
#'
#'   If a **formula**, e.g. `~ .x + 2`, it is converted to a function with up to
#'   two arguments: `.x` (single argument) or `.x` and `.y` (two arguments). The
#'   `.` placeholder can be used instead of `.x`.  This allows you to create
#'   very compact anonymous functions (lambdas) with up to two inputs.
#'
#'   If a **string**, the function is looked up in `globalenv()`.
#'
#' @param .field A string indicating the FAERS data to be used as input for the
#' `.fn` function to extract the primaryid or modify data. Only the following
#' values can be used: "demo", "drug", "indi", "ther", "reac", "rpsr", and
#' "outc".
#'   - `faers_filter`: Use `.fn` to extract primaryid. If `NULL`, `.object` will
#'     be passed directly to `.fn`. `.fn` should return an atomic integer or
#'     character of primaryid that you want to keep or remove based on the
#'     `.invert` argument.
#'   - `faers_modify`: Use `.fn` to modify the specified field data. You cannot
#'     use `NULL` here. `.fn` should always return a
#'     [data.table][data.table::data.table].
#' @param .invert A bool. If `TRUE`, will keep reports not returned by `.fn`.
#' @export
#' @method faers_filter FAERSascii
#' @rdname FAERS-methods
methods::setMethod(
    "faers_filter", "FAERSascii",
    function(.object, .fn, ..., .field = NULL, .invert = FALSE) {
        if (is.null(.field)) {
            data <- .object
        } else {
            data <- faers_get(.object, field = .field)
        }
        ids <- rlang::as_function(.fn)(data, ...)
        if (!(is.numeric(ids) || is.character(ids))) {
            cli::cli_abort("{.arg .fn} must return an atomic integer or character")
        }
        faers_keep(.object, primaryid = ids, invert = .invert)
    }
)

##############################################################
#' @export
#' @rdname FAERS-methods
methods::setGeneric("faers_modify", function(.object, ...) {
    methods::makeStandardGeneric("faers_modify")
})

#' @export
#' @method faers_modify FAERSascii
#' @rdname FAERS-methods
methods::setMethod("faers_modify", "FAERSascii", function(.object, .field, .fn, ...) {
    .field <- match.arg(.field, faers_ascii_file_fields)
    out <- dt_shallow(.object@data[[.field]])
    cannot_be_removed_cols <- c("year", "quarter", "primaryid")
    if (.object@standardization && any(.field == c("indi", "reac"))) {
        meddra_data <- .object@meddra@hierarchy
        out <- faers_add_meddra(out, meddra_data, remove_idx = FALSE)
        cannot_be_removed_cols <- c(
            cannot_be_removed_cols,
            "meddra_hierarchy_idx"
        )
    }
    .fn <- rlang::as_function(.fn)
    out <- .fn(out, ...)
    if (!data.table::is.data.table(out)) {
        cli::cli_abort("{.fn .fn} must return a {.cls data.table}")
    }
    # removing meddra columns before re-assign into original object
    if (exists("meddra_data", inherits = FALSE)) {
        meddra_cols <- intersect(names(out), names(meddra_data))
        data.table::set(out, j = meddra_cols, value = NULL)
    }
    missing_cols <- setdiff(cannot_be_removed_cols, names(out))
    if (length(missing_cols)) {
        cli::cli_abort("Cannot removing columns {.val {missing_cols}}")
    }
    .object@data[[.field]] <- out[] # in case of not printing
    invisible(.object)
})
