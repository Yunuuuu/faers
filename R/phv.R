#+-------------------+-----------------+------------+-------+
#|                   | ADR of interest | Other ADRs | Total |
#+-------------------+-----------------+------------+-------|
#| Drug of interest  |      a=n11      |    b=n10   | a + b |
#+-------------------+-----------------+------------+-------|
#| Other drugs       |      c=n01      |    d=n00   | c + d |
#+-------------------+-----------------+------------+-------|
#| Total             |       a+c       |    b+d     |a+b+c+d|
#+-------------------+-----------------+------------+-------+
#' Pharmacovigilance Analysis
#'
#' In the context of pharmacovigilance studies, disproportionality analysis
#' primarily served as a tool to evaluate possible association between a
#' specific adverse event and a particular drug which can then be investigated
#' through clinical assessment of individual case reports. Pharmacovigilance,
#' also known as drug safety. Following function can be used to Signal mining
#' @param a also referred to as `n11` as this is the count of event of interest
#' under exposure of interest.
#' @param b also referred to as `n10` as this is the count of \emph{not} event
#' of interest under exposure of interest.
#' @param c also referred to as `n01` as this is the count of event of interest
#' under \emph{not} exposure of interest.
#' @param d also referred to as `n00` as this is the count of \emph{not} event
#' of interest under \emph{not} exposure of interest.
#' @param alpha Level of significance, for construction of the confidence
#'  intervals.
#' @param n_mcmc number of MCMC simulations per \code{(a,b,c,d)}-tuple.
#' @name phv_signal
NULL

# modified from https://github.com/tystan/pharmsignal
#' @export
#' @rdname phv_signal
phv_ror <- function(a, b, c, d, alpha = 0.05) {
    # ugly way to do ad/bc but this way avoids integer overflow
    ror <- (a / b) * (d / c)
    log_ror <- log(ror)
    var_log_ror <- sqrt(1L / a + 1L / b + 1L / c + 1L / d)
    half <- alpha / 2L
    ci_low <- exp(stats::qnorm(half, log_ror, var_log_ror))
    ci_high <- exp(stats::qnorm(1L - half, log_ror, var_log_ror))

    data.table::data.table(
        # expected = (a + b) / (a + b + c + d) * (a + c),
        ror = ror,
        ci_low = ci_low,
        ci_high = ci_high
    )
}

#' @export
#' @rdname phv_signal
phv_prr <- function(a, b, c, d, alpha = 0.05) {
    prr <- (a / (a + b)) / (c / (c + d))
    log_prr <- log(prr)
    sd_log_prr <- sqrt(1L / a - 1L / (a + b) + 1L / c - 1L / (c + d))
    half <- alpha / 2L
    ci_low <- exp(stats::qnorm(half, log_prr, sd_log_prr))
    ci_high <- exp(stats::qnorm(1 - half, log_prr, sd_log_prr))
    data.table::data.table(prr = prr, ci_low = ci_low, ci_high = ci_high)
}

#' @export
#' @rdname phv_signal
phv_bcpnn_norm <- function(a, b, c, d, alpha = 0.05) {
    n1. <- a + b
    n.1 <- a + c
    n <- a + b + c + d

    p1 <- 1L + n1.
    p2 <- 1L + n - n1.
    q1 <- 1L + n.1
    q2 <- 1L + n - n.1
    r1 <- 1L + a
    r2b <- n - a - 1L + (2L + n)^2L / (q1 * p1)

    IC <- (digamma(r1) - digamma(r1 + r2b) -
        (digamma(p1) - digamma(p1 + p2) + digamma(q1) - digamma(q1 + q2))) /
        log(2L)
    var_ic <- (trigamma(r1) - trigamma(r1 + r2b) +
        (trigamma(p1) - trigamma(p1 + p2) + trigamma(q1) -
            trigamma(q1 + q2))) / log(2L)^2L
    half <- alpha / 2L
    ci_low <- stats::qnorm(half, IC, sqrt(var_ic))
    ci_high <- stats::qnorm(1 - half, IC, sqrt(var_ic))
    data.table::data.table(
        IC = IC,
        ci_low = ci_low,
        ci_high = ci_high
    )
}

#' @export
#' @rdname phv_signal
phv_bcpnn_mcmc <- function(a, b, c, d, alpha = 0.05, n_mcmc = 1e5L) {
    # NOTE: we could speed up the code by combining some of the expressions
    # here. We decided to keep it like this for readability's sake.

    # determine the marginals and the total
    n1. <- a + b
    n.1 <- a + c
    n <- a + b + c + d

    # determine the q-values (see eq. (5) in the paper by Noren et al.)
    q1. <- (n1. + 0.5) / (n + 1)
    q.1 <- (n.1 + 0.5) / (n + 1)
    q.0 <- (n - n.1 + 0.5) / (n + 1)
    q0. <- (n - n1. + 0.5) / (n + 1)

    # determine the prior parameters (see eq. (3) & (4) in the paper by Noren et al.)
    alpha.. <- 0.5 / (q1. * q.1)
    alpha11 <- q1. * q.1 * alpha..
    alpha10 <- q1. * q.0 * alpha..
    alpha01 <- q0. * q.1 * alpha..
    alpha00 <- q0. * q.0 * alpha..

    # determine the gammas on the basis of the alphas and the observed counts
    # see eq. (2) in the paper by Noren et al.
    gamma11 <- alpha11 + a
    gamma10 <- alpha10 + b
    gamma01 <- alpha01 + c
    gamma00 <- alpha00 + d

    IC <- log2(
        gamma11 * (gamma11 + gamma10 + gamma01 + gamma00) /
            ((gamma11 + gamma10) * (gamma11 + gamma01))
    )
    out <- .mapply(
        function(g11, g10, g01, g00) {
            # sample from the posterior distribution
            p <- MCMCpack::rdirichlet(n_mcmc, c(g11, g10, g01, g00))

            # compute the corresponding ICs for the samples from the posterior distribution
            p11 <- p[, 1]
            p1. <- p11 + p[, 2]
            p.1 <- p11 + p[, 3]

            ic_monte <- log2(p11 / (p1. * p.1))
            # posterior distribution quantiles:
            # (0.025, 0.975) for alpha = 0.05
            stats::quantile(ic_monte, c(alpha / 2, 1 - alpha / 2))
        },
        list(g11 = gamma11, g10 = gamma10, g01 = gamma01, g00 = gamma00), NULL
    )
    out <- data.table::as.data.table(do.call("rbind", out))
    data.table::setnames(out, c("ci_low", "ci_high"))
    out[, IC := IC]
    data.table::setcolorder(out, "IC", before = 1L)
    out[]
}
