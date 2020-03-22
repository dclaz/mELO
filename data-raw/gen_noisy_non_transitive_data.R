# TODO Generate data with noisy outcomes.

# Add some noise to the rps outcomes
set.seed(1337)
noisy_rps_df <- rps_df
noisy_rps_df$outcome_no_noise <- noisy_rps_df$outcome
noisy_rps_df$outcome <- (
    (runif(nrow(noisy_rps_df), 0, 1) > 0.50) +
        noisy_rps_df$outcome
) %% 2


noisy_rps_df <- noisy_rps_df[
    sample(1:nrow(noisy_rps_df), nrow(noisy_rps_df), replace=FALSE),
    ]
noisy_rps_df$time_index <- 1:(nrow(noisy_rps_df))

# CHECK totals
noisy_rps_df %>%
    group_by(player_1, player_2) %>%
    summarise(N=n(), sum(outcome_no_noise) ,sum(outcome)) %>%
    ungroup()


