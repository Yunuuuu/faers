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

#' The `format_` functions are easier to work with because they format the style
#' eagerly. However they produce slightly incorrect style in corner cases
#' because the formatting doesn't take into account the message type. In
#' principle, cli themes can create different stylings depending on the message
#' type.
#' @noRd
format_val <- function(x) .rlang_cli_format_inline(x, "val", NULL)
format_emph <- function(x) .rlang_cli_format_inline(x, "emph", NULL)
format_strong <- function(x) .rlang_cli_format_inline(x, "strong", NULL)

format_code <- function(x) .rlang_cli_format_inline(x, "code", "`%s`")
format_q <- function(x) .rlang_cli_format_inline(x, "q", NULL)
format_pkg <- function(x) .rlang_cli_format_inline(x, "pkg", NULL)
format_fn <- function(x) .rlang_cli_format_inline(x, "fn", "`%s()`")
format_arg <- function(x) .rlang_cli_format_inline(x, "arg", "`%s`")
format_kbd <- function(x) .rlang_cli_format_inline(x, "kbd", "[%s]")
format_key <- function(x) .rlang_cli_format_inline(x, "key", "[%s]")
format_file <- function(x) .rlang_cli_format_inline(x, "file", NULL)
format_path <- function(x) .rlang_cli_format_inline(x, "path", NULL)
format_email <- function(x) .rlang_cli_format_inline(x, "email", NULL)
format_url <- function(x) .rlang_cli_format_inline(x, "url", "<%s>")
format_var <- function(x) .rlang_cli_format_inline(x, "var", "`%s`")
format_envvar <- function(x) {
    .rlang_cli_format_inline(x, "envvar", "`%s`")
}
format_field <- function(x) .rlang_cli_format_inline(x, "field", NULL)
format_cls <- function(x) {
    fallback <- function(x) sprintf("<%s>", paste0(x, collapse = "/"))
    .rlang_cli_format_inline(x, "cls", fallback)
}
format_href <- function(x, target = NULL) {
    .rlang_cli_format_inline_link(x, target, "href", "<%s>")
}
format_run <- function(x, target = NULL) {
    .rlang_cli_format_inline_link(x, target, "run", "`%s`")
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

.rlang_cli_format_inline <- function(x, span, fallback = "`%s`") {
    if (.rlang_cli_has_cli()) {
        cli::format_inline(paste0("{.", span, " {x}}"))
    } else {
        .rlang_cli_style_inline(x, span, fallback = fallback)
    }
}

.rlang_cli_format_inline_link <- function(x, target, span, fallback = "`%s`") {
    if (.rlang_cli_has_cli()) {
        if (is.null(target)) {
            cli::format_inline(paste0("{.", span, " {x}}"))
        } else {
            cli::format_inline(paste0("{.", span, " [{x}]({target})}"))
        }
    } else {
        .rlang_cli_style_inline(x, span, fallback = fallback)
    }
}

.rlang_cli_has_cli <- function(version = "3.0.0") {
    is_installed("cli", version = version)
}

is_installed <- function(pkg, version = NULL) {
    id <- if (is.null(version)) pkg else paste(pkg, version, sep = ":")
    out <- faers_cache[[id]]
    if (is.null(out)) {
        if (is.null(version)) {
            out <- requireNamespace(pkg, quietly = TRUE)
        } else {
            out <- requireNamespace(pkg, quietly = TRUE) &&
                utils::packageVersion(pkg) >= version
        }
        faers_cache[[id]] <<- out
    }
    out
}

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
