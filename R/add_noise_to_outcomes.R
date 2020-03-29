#' Add noise to match outcomes
#'
#' This function can be used to flip a set proportion of match outcomes. Useful
#' for testing models under varying levels of signal to noise ratios.
#'
#' Input outcomes of 0.5 will be randomy rounded to either 0 or 1.
#'
#' @param outcome Original match outcome. Must be one of 0, 0.5, or 1.
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
#'
add_noise_to_outcomes <- function(
    outcome,
    error_prob = 0.1
){

    if( !all(outcome %in% c(0, 0.5, 1))){
        stop("Outcome must be one of 0, 0.5, or 1")
    }

    outcome <- round(jitter(outcome))

    noise <- (runif(length(outcome), 0, 1))
    noisy_outcome <- (outcome + (noise <= error_prob) ) %% 2

    return(noisy_outcome)

}