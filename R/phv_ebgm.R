# Following codes are modified from openEBGM, since all of these functions
# will prevent zeros in `theta_hat`.
phv_ebgm_qn <- function(theta_hat, N, E) {
    # openEBGM::Qn
    prob_f1 <- theta_hat[2] / (theta_hat[2] + E)
    prob_f2 <- theta_hat[4] / (theta_hat[4] + E)
    f1_NB <- stats::dnbinom(N, size = theta_hat[1], prob = prob_f1)
    f2_NB <- stats::dnbinom(N, size = theta_hat[3], prob = prob_f2)
    P_hat <- theta_hat[5]
    Qn_num <- P_hat * f1_NB
    Qn_den <- (P_hat * f1_NB) + ((1 - P_hat) * f2_NB)
    Qn_num / Qn_den
}

phv_ebgm_score <- function(theta_hat, N, E, qn) {
    # openEBGM::ebgm
    expectation1 <- digamma(theta_hat[1] + N) -
        log(theta_hat[2] + E)
    expectation2 <- digamma(theta_hat[3] + N) -
        log(theta_hat[4] + E)
    exp_log_lamda <- (qn * expectation1) + ((1 - qn) * expectation2)
    EBlog2 <- exp_log_lamda / log(2)
    2^EBlog2
}

phv_ebgm_quant_bisect <- function(
    cut_point, theta_hat, N, E, qn, digits = 2,
    limits = c(-1e+05, 1e+05), max_iter = 2000L) {
    # openEBGM::quantBisect
    cdf_error <- function(guess, cut_point, qn, theta_hat, N,
                          E) {
        cdf1 <- stats::pgamma(guess,
            shape = theta_hat[1] + N,
            rate = theta_hat[2] + E
        )
        cdf2 <- stats::pgamma(guess,
            shape = theta_hat[3] + N,
            rate = theta_hat[4] + E
        )
        post_cdf <- (qn * cdf1) + ((1 - qn) * cdf2)
        post_cdf - cut_point
    }
    tol <- 0.5 * (10^(-digits))
    num_pts <- length(N)
    guess_init <- rep(1, num_pts)
    error_init <- cdf_error(
        guess_init, cut_point, qn, theta_hat,
        N, E
    )
    is_too_big <- error_init > 0
    left <- rep(limits[1], num_pts)
    right <- rep(limits[2], num_pts)
    left <- ifelse(is_too_big, left, guess_init)
    right <- ifelse(is_too_big, guess_init, right)
    iter_count <- 1L
    while (iter_count <= max_iter) {
        mid_pt <- (left + right) / 2
        error <- cdf_error(
            mid_pt, cut_point, qn, theta_hat,
            N, E
        )
        if (max((right - left) / 2) < tol) {
            quantiles <- round(mid_pt, digits)
            if (max(quantiles) == limits[2]) {
                stop("increase maximum for 'limits'")
            }
            return(quantiles)
        } else {
            iter_count <- iter_count + 1
            error_left <- cdf_error(
                left, cut_point, qn, theta_hat,
                N, E
            )
            left <- ifelse(sign(error_left) == sign(error), mid_pt,
                left
            )
            right <- ifelse(sign(error_left) == sign(error),
                right, mid_pt
            )
        }
    }
    stop("failed to converge -- try adjusting 'limits' or 'max_iter'")
}

phvid_objective <- function(theta, N, E) {
    # https://github.com/cran/PhViD/blob/master/R/GPS.R#L29
    out <- dbinbinom(N,
        size1 = theta[1L],
        prob1 = theta[2L] / (theta[2L] + E),
        size2 = theta[3L],
        prob2 = theta[4L] / (theta[4L] + E),
        w = theta[5L]
    )
    sum(-log(out))
}

dbinbinom <- function(x, size1, prob1, size2, prob2, w) {
    # https://github.com/bips-hb/pvm/blob/master/R/dbinbinom.R
    w * stats::dnbinom(x, size = size1, prob = prob1) +
        (1 - w) * stats::dnbinom(x, size = size2, prob = prob2)
}
