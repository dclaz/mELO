#' Add noise to match outcomes
#'
#' This function can be used to flip a set proportion of match outcomes. Useful
#' for testing models.
#'
#' @param outcome Original match outcome. Must be between 0 and 1 inclusive.
#' @param error_prob The probability an outcome switches from a 0 to 1 or
#' vice-versa
#'
#' @return a numeric vector.
#' @export
#'
#' @examples
#' # Create some sample outcomes
#' set.seed(1)
#' test_df <- data.frame(
#'    true_outcome = sample(c(0,1), 1000, replace=TRUE)
#' )
#'
#' # No noise
#' test_df$noisy_outcome = add_noise_to_outcomes(test_df$true_outcome, error_prob = 0)
#' table(test_df)
#'
#' # Test different noise levels
#' test_df$noisy_outcome = add_noise_to_outcomes(test_df$true_outcome, error_prob = 0.1)
#' table(test_df)
#'
#' test_df$noisy_outcome = add_noise_to_outcomes(test_df$true_outcome, error_prob = 0.5)
#' table(test_df)
#'
#' test_df$noisy_outcome = add_noise_to_outcomes(test_df$true_outcome, error_prob = 1)
#' table(test_df)
add_noise_to_outcomes <- function(
    outcome,
    error_prob = 0.1
){

    noisy_outcome <- ((runif(length(outcome), 0, 1) > (1-error_prob)) + outcome) %% 2

    return(noisy_outcome)

}