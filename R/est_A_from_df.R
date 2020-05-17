#' Get very rough estimates of the advantage/disadvantage matrix from match data
#'
#' Combined with est_C_given_A(), this might be useful for warm starting mELO models.
#'
#' @param data a data.frame of matches. Four columns, time, agent 1, agent 2 and the outcome.
#'
#' @return a matrix.
#' @export
#'
#' @examples
#' est_A_from_data(rps_df)
#' est_A_from_data(rpssl_df)
#' est_A_from_data(rpsfw_df)
#' noisy_rpsfw_df <- rpsfw_df
#' noisy_rpsfw_df$outcome <- add_noise_to_outcomes(noisy_rpsfw_df$outcome, error_prob = 0.5)
#' est_A_from_data(noisy_rpsfw_df)
est_A_from_data <- function(
    data
){

    alpha <- log(10)/400

    names(data) <- c("time", "agent_1", "agent_2", "outcome")

    mat_temp <- data %>%
        #filter(throw_1 != throw_2) %>%
        group_by(agent_1, agent_2) %>%
        summarise(outcome = sum(outcome)) %>%
        ungroup() %>%
        spread(throw_2, outcome) %>%
        select(-throw_1) %>%
        as.matrix()

    mat <- mat_temp / (mat_temp + t(mat_temp))
    rownames(mat) <- colnames(mat)
    mat[is.na(mat)] <- 0

    A_mat <- -1*log(1/mat - 1)/alpha
    A_mat <- A_mat %>% pmin(1000) %>% pmax(-1000)

    return(A_mat)

}







