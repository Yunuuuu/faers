# stingr from base R ---------------------------------
str_which <- function(string, pattern, ..., fixed = FALSE) {
    grep(
        pattern = pattern, x = string, ...,
        perl = !fixed, value = FALSE,
        fixed = fixed
    )
}

str_detect <- function(string, pattern, ..., fixed = FALSE) {
    grepl(pattern = pattern, x = string, ..., perl = !fixed, fixed = fixed)
}

str_subset <- function(string, pattern, ..., fixed = FALSE) {
    grep(
        pattern = pattern, x = string, ...,
        perl = !fixed, value = TRUE,
        fixed = fixed
    )
}

str_replace <- function(string, pattern, replacement, ..., fixed = FALSE) {
    sub(pattern = pattern, replacement = replacement, x = string, 
        perl = !fixed, fixed = fixed, ...
    )
}

str_replace_all <- function(string, pattern, replacement, ..., fixed = FALSE) {
    gsub(pattern = pattern, replacement = replacement, x = string, 
        perl = !fixed, fixed = fixed, ...
    )
}

str_extract <- function(string, pattern, ..., fixed = FALSE) {
    matches <- regexpr(pattern, string, perl = !fixed, ..., fixed = fixed)
    start <- as.vector(matches)
    end <- start + attr(matches, "match.length") - 1L
    start[start == -1L] <- NA_integer_
    substr(string, start, end)
}
str_extract_all <- function(string, pattern, ..., fixed = FALSE) {
    regmatches(
        string,
        m = gregexpr(
            pattern = pattern, text = string,
            perl = !fixed, ..., fixed = fixed
        )
    )
}

# split string based on pattern, Only split once, Return a list of character,
# the length of every element is two
str_split_fixed <- function(string, pattern, ..., fixed = FALSE) {
    regmatches(
        string,
        regexpr(
            pattern = pattern, text = string,
            perl = !fixed, ..., fixed = fixed
        ),
        invert = TRUE
    )
}

str_split <- function(string, pattern, fixed = FALSE) {
    strsplit(x = string, split = pattern, fixed = fixed, perl = !fixed)
}

str_match <- function(string, pattern, ..., fixed = FALSE) {
    out <- regmatches(
        string,
        regexec(
            pattern = pattern, text = string,
            perl = !fixed, ..., fixed = fixed
        ),
        invert = FALSE
    )
    out <- lapply(out, function(x) {
        if (!length(x)) "" else x
    })
    out <- do.call("rbind", out)
    out[out == ""] <- NA_character_
    out
}

str_match_all <- function(string, pattern, ..., fixed = FALSE) {
    regmatches(
        string,
        gregexec(
            pattern = pattern, text = string,
            perl = !fixed, ..., fixed = fixed
        ),
        invert = FALSE
    )
}
