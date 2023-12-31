% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combine.R
\name{faers_combine}
\alias{faers_combine}
\title{Combine FAERS objects from different Quarterly files.}
\usage{
faers_combine(...)
}
\arguments{
\item{...}{Multiple \code{FAERSxml} or \code{FAERSascii} objects or a list containing
\code{FAERSxml} or \code{FAERSascii} objects. Objects can be standardized by
\link{faers_standardize} but cannot be de-duplicated by \link{faers_dedup}. If we
combine deduplicated objects from different quarterly data files, duplicate
reports will be introduced again.}
}
\value{
A \link{FAERSxml} or \link{FAERSascii} object.
}
\description{
Packed all \code{FAERSascii} or \code{FAERSxml} objects into a single \code{FAERSascii} or
\code{FAERSxml} object. It is important to note that all data passed to these
functions via the \code{...} argument must belong to the different \link{FAERS}
objects, indicating that they have the different period data (as defined by
\link{faers_period}).
}
\examples{
# the files included in the package are sampled
data1 <- faers_parse(
    system.file("extdata", "aers_ascii_2004q1.zip", package = "faers"),
    compress_dir = tempdir()
)
data2 <- faers_parse(
    system.file("extdata", "faers_ascii_2017q2.zip", package = "faers"),
    compress_dir = tempdir()
)
faers_combine(data1, data2)
}
