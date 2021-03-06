% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datagen3.R
\name{generate_sturmer_data}
\alias{generate_sturmer_data}
\title{Generate data by the extended version of Sturmer et al 2010}
\usage{
generate_sturmer_data(n, alphas, prev_params, contraindication, beta0,
  betaA, betaX, betaXA, outcome_fun)
}
\arguments{
\item{n}{Sample size}

\item{alphas}{True coefficients for the first and second treatment linear predictors. This vector should contain the intercept. alphas = c(alpha01, alphaX1, alpha02, alphaX2)}

\item{prev_params}{Prevalence parameters. A vector of length 3.}

\item{contraindication}{Logical to determine whether to parametrize based on contraindication. If \code{TRUE}, the unmeasured binary variables are more common in the high PS regions (corners in the PS ternary plot). If \code{FALSE}, the unmeasured binary variables are more common in the low PS regions (edges in the PS ternary plot).}

\item{beta0}{Outcome model intercept coefficient}

\item{betaA}{Outcome model coefficient for I(A_i = 1) and I(A_i = 2)}

\item{betaX}{Outcome model coefficient vector for covariates X_i}

\item{outcome_fun}{Outcome generating function. Either one of \code{\link{generate_bin_outcome_log_tri_treatment}} or \code{\link{generate_count_outcome_log_tri_treatment}}}

\item{betaXA1}{Outcome model interaction coefficients for covariates. betaXA = c(betaXA1, betaXA2)}
}
\value{
a complete simulated data_frame
}
\description{
.. content for details ..
}
\author{
Kazuki Yoshida
}
