###
### Tests for the corresponding script
################################################################################


library(testthat)
library(tidyverse)

context("Data Generation")

set.seed(2017092615)


test_that("rmultinom works", {

    pA_mat <- matrix(c(1,0,0,
                       0,1,0,
                       0,0,1,
                       1/3,1/3,1/3),
                     ncol = 3,
                     byrow = TRUE)
    A_mat <- pA_mat_to_A_mat(pA_mat)

    ## Expectations
    expect_equal(dim(pA_mat), dim(A_mat))

    A <- A_indicator_mat_to_multinom_A_vec(A_mat)

    expect_equal(length(A), 4)
    expect_equal(A[-4],c(0,1,2))

})


test_that("Data generation steps work for three groups", {

    Sigma <- matrix(c(1, 0.7,
                      0.7, 1),
                    byrow = TRUE,
                    ncol = 2)
    Sigma

    ## Generate binary covariates
    data_bin_cov <- generate_mvn_covariates(n = 1000, mu = rep(0,ncol(Sigma)), Sigma = Sigma) %>%
        generate_covariates(prob = c(0.01, 0.2))

    summary(data_bin_cov)

    ## All are {0,1} binary
    expect_true(all(data_bin_cov$X1 %in% c(0,1)))
    expect_true(all(data_bin_cov$X2 %in% c(0,1)))

    ## Add three-valued treatment
    data_tri_treat <- data_bin_cov %>%
        generate_tri_treatment(alphas1 = c(-1.5, +0.3 , -0.5),
                               alphas2 = c(-1.0, -0.3 , +0.5))
    data_tri_treat
    expect_true(all(data_tri_treat$A %in% c(0,1,2)))

    ## Add binary outcome via log-linear model
    data_bin_outcome_null_tx <- data_tri_treat %>%
        generate_bin_outcome_log_tri_treatment(beta0 = -1,
                                               ## No treatment effect at all
                                               betaA1 = 0,
                                               betaA2 = 0,
                                               betaX = c(0.1, 0.2),
                                               betaXA1 = c(0, 0),
                                               betaXA2 = c(0, 0))
    data_bin_outcome_null_tx
    expect_true(all(data_bin_outcome_null_tx$Y %in% c(0,1)))
    ## Under no treatment effect counterfactual are the same
    expect_equal(data_bin_outcome_null_tx$pY0,
                 data_bin_outcome_null_tx$pY1)
    expect_equal(data_bin_outcome_null_tx$pY1,
                 data_bin_outcome_null_tx$pY2)

    ## Protective A = 1
    data_bin_outcome_protect_A1 <- data_tri_treat %>%
        generate_bin_outcome_log_tri_treatment(beta0 = -1,
                                               ## No treatment effect at all
                                               betaA1 = -1,
                                               betaA2 = 0,
                                               betaX = c(0.1, 0.2),
                                               betaXA1 = c(0, 0),
                                               betaXA2 = c(0, 0))
    data_bin_outcome_protect_A1
    ## Counterfactual under treatment 1 is lower.
    expect_equal(data_bin_outcome_protect_A1$pY0,
                 data_bin_outcome_protect_A1$pY2)
    expect_true(all(data_bin_outcome_protect_A1$pY1 < data_bin_outcome_protect_A1$pY0))
    expect_true(all(data_bin_outcome_protect_A1$pY1 < data_bin_outcome_protect_A1$pY2))


    ## Protective A = 1 in X2 = 1
    data_bin_outcome_protect_A1_in_X2 <- data_tri_treat %>%
        generate_bin_outcome_log_tri_treatment(beta0 = -1,
                                               ## No treatment effect at all
                                               betaA1 = 0,
                                               betaA2 = 0,
                                               betaX = c(0.1, 0.2),
                                               betaXA1 = c(0, 0),
                                               betaXA2 = c(0, -1))
    data_bin_outcome_protect_A1_in_X2
    ## Same in X2 = 0 stratum
    expect_equal(filter(data_bin_outcome_protect_A1_in_X2, X2 == 0)$pY0,
                 filter(data_bin_outcome_protect_A1_in_X2, X2 == 0)$pY1)
    expect_equal(filter(data_bin_outcome_protect_A1_in_X2, X2 == 0)$pY0,
                 filter(data_bin_outcome_protect_A1_in_X2, X2 == 0)$pY2)
    ## Protective in X2 = 1 stratum
    expect_true(all(filter(data_bin_outcome_protect_A1_in_X2, X2 == 1)$pY1 < filter(data_bin_outcome_protect_A1_in_X2, X2 == 1)$pY0))
    expect_true(all(filter(data_bin_outcome_protect_A1_in_X2, X2 == 1)$pY1 < filter(data_bin_outcome_protect_A1_in_X2, X2 == 1)$pY2))

})
