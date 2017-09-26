###
### Tests for the corresponding script
################################################################################


library(testthat)

context("Data Generation")

set.seed(2017092615)


test_that("rmultinom works", {

    pA_mat <- matrix(c(1,0,0,
                       0,1,0,
                       0,0,1,
                       1/3,1/3,1/3),
                     byrow = TRUE)
    A_mat <- pA_mat_to_A_mat(pA_mat)

    ## Expectations
    expect_equal(dim(pA_mat), dim(A_mat))

    expect_true(FALSE)
})
