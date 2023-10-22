#' Pharmacovigilance Analysis
#'
#' Pharmacovigilance, also known as drug safety. In the context of
#' pharmacovigilance studies, disproportionality analysis primarily served as a
#' tool to evaluate possible association between a specific adverse event and a
#' particular drug which can then be investigated through clinical assessment of
#' individual case reports.
#'
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
#' @details
#' Note that the `a`, `b`, `c`, `d` inputs can be an atomic vectors of equal
#' length, for which the function will perform the calculations for each
#' individual `(a,b,c,d)`-tuple moving across the vectors.
#'
#' It is assumed that the contingency table under consideration has
#' drugs/exposures in the rows and outcomes/events in the columns. See
#' contingency table section.
#' @section Contingency table:
#' |                   | ADR of interest | Other ADRs | Total |
#' |-------------------|-----------------|------------|-------|
#' | Drug of interest  |      a=n11      |    b=n10   | a + b |
#' | Other drugs       |      c=n01      |    d=n00   | c + d |
#' | Total             |       a+c       |    b+d     |a+b+c+d|
#' @return A [data.table][data.table::data.table] with columns of estimated
#' value and it's confidence interval (`ci_low` and `ci_high`). Estimated column
#' are as follows:
#' - `phv_ror`: reporting odds ratio (`ror`).
#' - `phv_prr`: proportional reporting ratio (`prr`).
#' - `phv_bcpnn_norm`: information component (`ic`).
#' - `phv_bcpnn_mcmc`: information component (`ic`).
#' - `phv_obsexp_shrink`: observed to expected ratio (`oe`).
#' @name phv_signal
NULL

# modified from https://github.com/tystan/pharmsignal
#' @export
#' @rdname phv_signal
phv_ror <- function(a, b, c, d, alpha = 0.05) {
    assert_phv_table(a, b, c, d)

    # ugly way to do ad/bc but this way avoids integer overflow
    ror <- (a / b) * (d / c)
    log_ror <- log(ror)
    var_log_ror <- sqrt(1L / a + 1L / b + 1L / c + 1L / d)
    half <- alpha / 2L
    ci_low <- exp(stats::qnorm(half, log_ror, var_log_ror))
    ci_high <- exp(stats::qnorm(1L - half, log_ror, var_log_ror))

    data.table::data.table(
        ror = ror,
        ci_low = ci_low,
        ci_high = ci_high
    )
}

#' @export
#' @rdname phv_signal
phv_prr <- function(a, b, c, d, alpha = 0.05) {
    assert_phv_table(a, b, c, d)

    # run prr analysis
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
    assert_phv_table(a, b, c, d)

    # run bcpnn analysis
    n1. <- a + b
    n.1 <- a + c
    n <- a + b + c + d

    p1 <- 1L + n1.
    p2 <- 1L + n - n1.
    q1 <- 1L + n.1
    q2 <- 1L + n - n.1
    r1 <- 1L + a
    r2b <- n - a - 1L + (2L + n)^2L / (q1 * p1)

    ic <- (digamma(r1) - digamma(r1 + r2b) -
        (digamma(p1) - digamma(p1 + p2) + digamma(q1) - digamma(q1 + q2))) /
        log(2L)
    var_ic <- (trigamma(r1) - trigamma(r1 + r2b) +
        (trigamma(p1) - trigamma(p1 + p2) + trigamma(q1) -
            trigamma(q1 + q2))) / log(2L)^2L
    half <- alpha / 2L
    ci_low <- stats::qnorm(half, ic, sqrt(var_ic))
    ci_high <- stats::qnorm(1 - half, ic, sqrt(var_ic))
    data.table::data.table(
        ic = ic,
        ci_low = ci_low,
        ci_high = ci_high
    )
}

#' @param n_mcmc number of MCMC simulations per `(a,b,c,d)`-tuple.
#' @export
#' @rdname phv_signal
phv_bcpnn_mcmc <- function(a, b, c, d, alpha = 0.05, n_mcmc = 1e5L) {
    assert_phv_table(a, b, c, d)
    # run bcpnn analysis
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

    ic <- log2(
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
    out[, ic := ic]
    data.table::setcolorder(out, "ic", before = 1L)
    out[]
}

#' @param alpha1 Numerator shrinkage parameter `>=0`, default `0.5`.
#' See section phv_obsexp_shrink.
#' @param alpha2 Denominator shrinkage parameter `>=0`, default `0.5`.
#' See section phv_obsexp_shrink.
#' @section phv_obsexp_shrink:
#'
#' The observed to expected (OE) ratio with approximate confidence intervals are
#' constructed on the log2 scale as outlined in Norén et al. (2013).
#'
#' Expected value was estimated by `(a + b) / (a + b + c + d) * (a + c)`.
#'
#' The OE ratio with shrinkage estimates is calculated as `(O + alpha1) / (E +
#' alpha2)`.
#'
#' If `(O + alpha1) < 1`, then the exact uncertainty limits should be used. That
#' is the confidence intervals as implemented in `phv_bcpnn_mcmc` (Norén et al.,
#' 2013).
#'
#' `log2(OE)` approximates the Bayesian confidence propagation neural
#' network information component (IC) with reasonable accuracy when `alpha1
#' = alpha2 = 0.5` (Norén et al., 2013).
#' @export
#' @rdname phv_signal
phv_obsexp_shrink <- function(a, b, c, d, alpha = 0.05, alpha1 = 0.5, alpha2 = 0.5) {
    assert_phv_table(a, b, c, d)
    # run bcpnn analysis
    n <- a + b + c + d
    n1. <- a + b
    n.1 <- a + c
    expected <- (n1. / n) * n.1

    oe <- log2((a + alpha1) / (expected + alpha2))
    ci_low <- oe - 3.3 * (a + alpha1)^(-1L / 2L) -
        2L * (a + alpha1)^(-3L / 2L)
    ci_high <- oe + 2.4 * (a + alpha1)^(-1L / 2L) -
        0.5 * (a + alpha1)^(-3L / 2L)

    need_exact_lims <- (a + alpha1) < 1L
    if (any(need_exact_lims)) {
        cli::cli_warn(c(
            `!` = "{sum(need_exact_lims)} case{?s} of (O + alpha1) < 1",
            i = "will use the CIs from the BCPNN IC (using MCMC estimation)"
        ))

        # use default mcmc draws
        a_star <- a[need_exact_lims]
        b_star <- b[need_exact_lims]
        c_star <- c[need_exact_lims]
        d_star <- d[need_exact_lims]
        ic <- phv_bcpnn_mcmc(a_star, b_star, c_star, d_star, alpha = alpha)
        # replace the CIs for those with (O + alpha1) < 1
        ci_low[need_exact_lims] <- ic$ci_low
        ci_high[need_exact_lims] <- ic$ci_high
    }
    data.table::data.table(oe = oe, ci_low = ci_low, ci_high = ci_high)
}

assert_phv_table <- function(a, b, c, d, call = rlang::caller_env()) {
    lst <- list(a, b, c, d)
    right_cls <- vapply(lst, function(x) {
        is.numeric(x) && all(x >= 0L)
    }, logical(1L))
    if (!all(right_cls)) {
        fail_cls <- c("a", "b", "c", "d")[!right_cls] # nolint
        cli::cli_abort(
            "{.arg {fail_cls}} must be non-negative numeric vector",
            call = call
        )
    }
    lens <- lengths(lst)
    if (!all(lens == lens[1L])) {
        cli::cli_abort(
            c("all {.arg a}, {.arg b}, {.arg c}, {.arg d} must be numeric vector and all have the same length",
                i = "length of {.arg a}, {.arg b}, {.arg c}, {.arg d}: {lens}"
            ),
            call = call
        )
    }
}
