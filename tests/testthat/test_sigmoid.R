context("sigmoid")


testthat::test_that("sigmoid", {
    expect_equal(length(0), 1)
    expect_equal(str_length(c(0,1)), 2)
    expect_equal(str_length(c(0,1,10)), 3)
})
