# Standalone file: do not edit by hand
# Source: <https://github.com/Yunuuuu/biomisc/blob/main/R/standalone-assert.R>
# ----------------------------------------------------------------------
#
# ---
# repo: Yunuuuu/biomisc
# file: standalone-assert.R
# last-updated: 2023-09-07
# license: https://unlicense.org
# dependencies: [standalone-obj-type.R, standalone-cli.R]
# imports: rlang (>= 1.1.0)
# ---

#' Report if an argument is a specific class
#'
#' @param x The object type which does not conform to `what`. Its
#'   `obj_type_friendly()` is taken and mentioned in the error message.
#' @param what The friendly expected type as a string. Can be a
#'   character vector of expected types, in which case the error
#'   message mentions all of them in an "or" enumeration.
#' @param show_value Passed to `value` argument of `obj_type_friendly()`.
#' @param show_length Passed to `length` argument of `obj_type_friendly()`.
#' @param ... Arguments passed to [rlang::abort()].
#' @noRd
assert_ <- function(
    x, assert_fn, what,
    null_ok = FALSE,
    show_value = TRUE,
    show_length = FALSE,
    ...,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()) {
    if (null_ok && is.null(x) || assert_fn(x)) {
        return(invisible(NULL))
    }
    stop_input_type(x, what,
        null_ok = null_ok,
        show_value = show_value,
        show_length = show_length,
        ..., arg = arg, call = call
    )
}

stop_input_type <- function(
    x, what,
    null_ok = FALSE,
    show_value = TRUE,
    show_length = FALSE,
    ...,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()) {
    if (null_ok) {
        what <- c(what, format_code("NULL"))
    }
    if (length(what)) {
        what <- oxford_comma(what, final = "or")
    }
    msg <- sprintf(
        "%s must be %s, not %s.",
        format_arg(arg),
        what,
        obj_type_friendly(x, value = show_value, length = show_length)
    )
    rlang::abort(msg, ..., call = call, arg = arg)
}

#' Report if an package installed
#' @noRd
assert_pkg <- function(pkg, version = NULL, fun = NULL, call = rlang::caller_env()) {
    if (!is_installed(pkg, version = version)) {
        if (is.null(fun)) {
            fun_call <- rlang::frame_call(frame = call)
            fun <- rlang::as_label(fun_call[[1L]])
        }
        pkg <- format_pkg(pkg)
        if (!is.null(version)) {
            pkg <- sprintf("%s (>=%s)", pkg, version)
        }
        rlang::abort(
            sprintf(
                "%s must be installed to use %s.",
                pkg, format_fn(fun)
            ),
            call = call
        )
    }
}

# scalar object ----------------------------------
assert_string <- function(
    x, empty_ok = TRUE, na_ok = FALSE, show_length = TRUE, ...,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()) {
    what <- "a string"
    if (!empty_ok) {
        what <- paste0(what, sprintf(
            "(empty %s is not allowed)", format_val("")
        ))
    }
    if (na_ok) {
        what <- c(what, format_code("NA"))
    }
    assert_(
        x = x,
        assert_fn = function(x) {
            .rlang_check_is_string(x, empty_ok = empty_ok, na_ok = na_ok)
        }, what = what,
        show_length = show_length,
        ...,
        arg = arg,
        call = call
    )
}

.rlang_check_is_string <- function(x, empty_ok, na_ok) {
    if (rlang::is_string(x)) {
        if (empty_ok || !rlang::is_string(x, "")) {
            return(TRUE)
        }
    }
    if (na_ok && (identical(x, NA) || identical(x, NA_character_))) {
        return(TRUE)
    }
    FALSE
}

assert_bool <- function(
    x, na_ok = FALSE, show_length = TRUE, ...,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()) {
    what <- c("`TRUE`", "`FALSE`")
    if (na_ok) {
        what <- c(what, format_code("NA"))
    }
    assert_(
        x = x,
        assert_fn = function(x) {
            .rlang_check_is_bool(x, na_ok = na_ok)
        }, what = what,
        show_length = show_length,
        ...,
        arg = arg,
        call = call
    )
}

.rlang_check_is_bool <- function(x, na_ok) {
    if (rlang::is_bool(x)) {
        return(TRUE)
    }
    if (na_ok && identical(x, NA)) {
        return(TRUE)
    }
    FALSE
}

# atomic vector ------------------------------------
is_character <- function(x, empty_ok = TRUE, na_ok = TRUE) {
    out <- is.character(x)
    if (!na_ok) {
        out <- out && !anyNA(x)
    }
    if (!empty_ok) {
        out <- out && !any(x == "")
    }
    out
}


# matrix -------------------------------------------
assert_matrix <- function(
    x, mode = NULL, any_na = TRUE, show_length = FALSE, ...,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()) {
    if (!is.null(mode)) {
        mode <- match.arg(mode, c(
            "integer", "numeric", "character", "logical"
        ))
        what <- switch(mode,
            integer = "an integer",
            paste("a", mode)
        )
        fn <- function(x) {
            all(match.fun(paste("is", mode, sep = "."))(x))
        }
    } else {
        what <- "a"
        fn <- NULL
    }
    what <- paste(what, format_cls("matrix"))
    if (!any_na) {
        what <- paste0(what, sprintf(
            " (%s is not allowed)", format_code("NA")
        ))
    }
    assert_(
        x = x,
        assert_fn = function(x) {
            .rlang_check_is_matrix(x, any_na = any_na, fn = fn)
        }, what = what,
        show_length = show_length,
        ...,
        arg = arg,
        call = call
    )
}

.rlang_check_is_matrix <- function(x, any_na, fn = NULL) {
    if (is.matrix(x)) {
        if (is.null(fn) || fn(x)) {
            if (any_na || !anyNA(x)) {
                return(TRUE)
            }
        }
    }
    FALSE
}

# S3 object ----------------------------------------
assert_s3_class <- function(
    x, is_class, what, ...,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()) {
    if (rlang::is_string(is_class)) {
        class <- is_class
        is_class <- function(x) {
            inherits(x, what = class)
        }
        if (missing(what)) {
            what <- class
        }
    }
    assert_(
        x = x, assert_fn = is_class, what = what,
        ..., arg = arg, call = call
    )
}

assert_data_frame <- function(x, ..., arg = rlang::caller_arg(), call = rlang::caller_env()) {
    assert_s3_class(
        x = x, is_class = "data.frame",
        what = "a data frame",
        ..., arg = arg, call = call
    )
}

assert_data_frame_columns <- function(x, columns, ..., args = rlang::caller_arg(x), call = rlang::caller_env()) {
    missing_cols <- setdiff(columns, names(x))
    if (length(missing_cols)) {
        args <- format_arg(args)
        if (length(args) == 1L) {
            msg <- args
        } else {
            msg <- sprintf("One of %s", oxford_comma(args, final = "or"))
        }
        rlang::abort(
            c(
                sprintf(
                    "%s must contain columns: %s", msg,
                    oxford_comma(columns)
                ),
                x = sprintf("missing columns: %s", oxford_comma(missing_cols))
            ),
            ...,
            call = call
        )
    }
}

#' Report if an argument has specific length
#' @keywords internal
#' @noRd
assert_length <- function(x, length, scalar_ok = FALSE, null_ok = FALSE, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    is_right_length <- length(x) == length
    if (scalar_ok) {
        is_right_length <- is_right_length || length(x) == 1L
    }
    if (null_ok) {
        is_right_length <- is_right_length || is.null(x)
    }
    if (!is_right_length) {
        stop_input_length(x, length,
            scalar_ok = scalar_ok, null_ok = null_ok,
            ..., arg = arg, call = call
        )
    }
}

stop_input_length <- function(
    x, length,
    scalar_ok = FALSE, null_ok = FALSE,
    ...,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()) {
    what <- NULL
    if (scalar_ok || length == 1L) {
        what <- c(what, sprintf("a %s", format_field("scalar")))
    }
    if (length != 1L) {
        what <- c(what, sprintf("of length %s", format_val(length)))
    }
    if (null_ok) {
        what <- c(what, format_code("NULL"))
    }
    if (length(what)) {
        what <- paste0(what, collapse = " or ")
    }
    msg <- sprintf(
        "%s must be %s, not of length %s.",
        format_arg(arg),
        what, format_val(length(x))
    )
    rlang::abort(msg, ..., call = call)
}

assert_inclusive <- function(x, y, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    missing_items <- setdiff(x, y)
    if (length(missing_items)) {
        rlang::abort(
            sprintf(
                "Only values (%s) are allowed in %s, not %s",
                oxford_comma(format_val(y)),
                format_arg(arg),
                oxford_comma(format_val(missing_items))
            ),
            call = call
        )
    }
}
