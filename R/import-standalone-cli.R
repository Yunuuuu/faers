# Standalone file: do not edit by hand
# Source: <https://github.com/Yunuuuu/biomisc/blob/main/R/standalone-cli.R>
# ----------------------------------------------------------------------
#
# ---
# repo: Yunuuuu/biomisc
# file: standalone-cli.R
# last-updated: 2023-05-01
# license: https://unlicense.org
# ---
#
# Provides a minimal shim API to format message elements consistently
# with cli in packages that can't depend on it. If available, cli is
# used to format the elements. Otherwise a fallback format is used.

#' The `style_` functions are helper to work with cli message. 
#' @noRd
style_val <- function(x) .rlang_cli_style_inline(x, "val", NULL)
style_emph <- function(x) .rlang_cli_style_inline(x, "emph", NULL)
style_strong <- function(x) .rlang_cli_style_inline(x, "strong", NULL)

style_code <- function(x) .rlang_cli_style_inline(x, "code", "`%s`")
style_q <- function(x) .rlang_cli_style_inline(x, "q", NULL)
style_pkg <- function(x) .rlang_cli_style_inline(x, "pkg", NULL)
style_fn <- function(x) .rlang_cli_style_inline(x, "fn", "`%s()`")
style_arg <- function(x) .rlang_cli_style_inline(x, "arg", "`%s`")
style_kbd <- function(x) .rlang_cli_style_inline(x, "kbd", "[%s]")
style_key <- function(x) .rlang_cli_style_inline(x, "key", "[%s]")
style_file <- function(x) .rlang_cli_style_inline(x, "file", NULL)
style_path <- function(x) .rlang_cli_style_inline(x, "path", NULL)
style_email <- function(x) .rlang_cli_style_inline(x, "email", NULL)
style_url <- function(x) .rlang_cli_style_inline(x, "url", "<%s>")
style_var <- function(x) .rlang_cli_style_inline(x, "var", "`%s`")
style_envvar <- function(x) {
    .rlang_cli_style_inline(x, "envvar", "`%s`")
}
style_field <- function(x) .rlang_cli_style_inline(x, "field", NULL)
style_cls <- function(x) {
    fallback <- function(x) sprintf("<%s>", paste0(x, collapse = "/"))
    .rlang_cli_style_inline(x, "cls", fallback)
}
style_href <- function(x, target = NULL) {
    .rlang_cli_style_inline_link(x, target, "href", "<%s>")
}
style_run <- function(x, target = NULL) {
    .rlang_cli_style_inline_link(x, target, "run", "`%s`")
}

.rlang_cli_style_inline <- function(x, span, fallback = "`%s`") {
    if (.rlang_cli_has_cli()) {
        paste0("{.", span, " {\"", encodeString(x), "\"}}")
    } else if (is.null(fallback)) {
        x
    } else if (is.function(fallback)) {
        fallback(x)
    } else {
        sprintf(fallback, x)
    }
}

.rlang_cli_style_inline_link <- function(x, target, span, fallback = "`%s`") {
    if (!is.null(target) && .rlang_cli_has_cli()) {
        sprintf("{.%s [%s](%s)}", span, x, target)
    } else {
        .rlang_cli_style_inline(x, span, fallback = fallback)
    }
}

.rlang_cli_has_cli <- function(version = "3.0.0") {
    is_installed("cli", version = version)
}

is_installed <- local({
    cache <- new.env()
    function(pkg, version = NULL) {
        id <- if (is.null(version)) pkg else paste(pkg, version, sep = ":")
        out <- cache[[id]]
        if (is.null(out)) {
            if (is.null(version)) {
                out <- requireNamespace(pkg, quietly = TRUE)
            } else {
                out <- requireNamespace(pkg, quietly = TRUE) &&
                    utils::packageVersion(pkg) >= version
            }
            cache[[id]] <<- out
        }
        out
    }
})

# utils function to collapse characters ---------------------------
oxford_comma <- function(chr, sep = ", ", final = "and") {
    n <- length(chr)

    if (n < 2L) {
        return(chr)
    }

    head <- chr[seq_len(n - 1L)]
    last <- chr[n]

    head <- paste(head, collapse = sep)

    # Write a or b. But a, b, or c.
    if (n > 2L) {
        paste0(head, sep, final, " ", last)
    } else {
        paste0(head, " ", final, " ", last)
    }
}
