# create sample files by sampling primaryids
faers_sample <- function(year, quarter, size = 100L, dir = getwd()) {
    raw_file <- faers_download(year, quarter, dir = tempdir())
    # read data
    path <- unzip2(raw_file, tempdir())
    file_dir <- locate_dir(path, "^ascii$")
    ascii_files <- locate_files(file_dir, "\\.txt$")
    names(ascii_files) <- tolower(str_remove(
        basename(ascii_files), "\\d+q\\d(_new)?\\.txt$",
        ignore.case = TRUE
    ))
    ascii_files <- ascii_files[faers_ascii_file_fields]
    data_lst <- lapply(ascii_files, function(file) {
        out <- data.table::fread(
            file = file,
            sep = "$", quote = "", fill = TRUE,
            blank.lines.skip = TRUE,
            na.strings = na_string,
            keepLeadingZeros = TRUE
        )
        data.table::setnames(out, tolower)
    })
    # sample primaryids
    if (is_from_laers(year, quarter)) {
        id <- "isr"
    } else {
        id <- "primaryid"
    }
    smps <- sample(unique(data_lst$demo[[id]]), size) # nolint

    # prepare output directory
    output_dir <- file.path(
        dir, sub("\\.zip$", "", basename(raw_file), ignore.case = TRUE)
    )
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    ascii_data_path <- file.path(output_dir, basename(file_dir))
    if (!dir.exists(ascii_data_path)) {
        dir.create(ascii_data_path)
    }
    # output data
    .mapply(function(data, file) {
        data <- eval(substitute(data[id %in% smps], list(id = rlang::sym(id))))
        data.table::fwrite(data,
            file.path(ascii_data_path, basename(file)),
            sep = "$"
        )
    }, list(data = data_lst, file = ascii_files), NULL)
    # compress
    output_file <- sprintf("%s.zip", basename(output_dir))
    zip2(output_file, basename(ascii_data_path), root = output_dir)
    out <- file.path(dir, output_file)
    file.rename(file.path(output_dir, output_file), out)
    unlink(output_dir, recursive = TRUE)
    out
}
