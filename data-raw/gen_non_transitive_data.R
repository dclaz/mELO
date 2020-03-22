################################################################################
# TEST DATA
################################################################################

library(dplyr)
rm(list=ls())
gc()

set.seed(1337)

################################################################################
# Simple non-transitive data
################################################################################

# Rock Paper Scissors

# Each possible match outcome
rps_df <- data.frame(
    throw_1 = c("PAPER", "ROCK", "SCISSORS", "ROCK", "SCISSORS", "PAPER"),
    throw_2 = c("ROCK", "SCISSORS", "PAPER", "PAPER", "ROCK", "SCISSORS"),
    outcome = c(1, 1, 1, 0, 0, 0),
    stringsAsFactors = FALSE
)

# Repeat matches
rps_df <- do.call(
    "rbind",
    replicate(20, rps_df, simplify = FALSE)
)

# clean up and prepare the dataframe
rps_df$time_index <- 1:(nrow(rps_df))
rps_df <- rps_df[,c("time_index", "throw_1", "throw_2", "outcome")]

rps_df <- rps_df %>% as_tibble()

# Create a randomly ordered fixture
unordered_rps_df <- rps_df %>%
    sample_frac(
        1,
        replace = FALSE
    )
unordered_rps_df$time_index <- 1:(nrow(unordered_rps_df))

unordered_rps_df <- unordered_rps_df %>% as_tibble()

# save datas to correct location
usethis::use_data(rps_df, overwrite = TRUE)
usethis::use_data(unordered_rps_df, overwrite = TRUE)



# TODO Matches happening at same time point
# # Matches happening at same time point
# non_transitive_data_2 <- non_transitive_data
# non_transitive_data_2$time_index <- rep(1:(nrow(non_transitive_data)/3), each=3)
#
# non_transitive_data_2b <- non_transitive_data
# non_transitive_data_2b$time_index <- rep(1:(nrow(non_transitive_data)/6), each=6)


################################################################################
# Extended non-transitive data
################################################################################

# Rock Paper Scissors Spock Lizard
rpssl_vec <- c("ROCK", "PAPER", "SCISSORS", "SPOCK", "LIZARD")

rpssl_df <- expand.grid(
    rpssl_vec,
    rpssl_vec,
    stringsAsFactors = FALSE
) %>%
    as_tibble()

rpssl_df <- rpssl_df %>%
    arrange(Var1, Var2) %>%
    mutate(
        outcome = NA,
        outcome = ifelse(
            (Var1 == "PAPER") & (Var2 %in% c("SCISSORS", "LIZARD")),
            0,
            outcome
        ),
        outcome = ifelse(
            (Var1 == "SCISSORS") & (Var2 %in% c("ROCK", "SPOCK")),
            0,
            outcome
        ),
        outcome = ifelse(
            (Var1 == "ROCK") & (Var2 %in% c("PAPER", "SPOCK")),
            0,
            outcome
        ),
        outcome = ifelse(
            (Var1 == "SPOCK") & (Var2 %in% c("PAPER", "LIZARD")),
            0,
            outcome
        ),
        outcome = ifelse(
            (Var1 == "LIZARD") & (Var2 %in% c("SCISSORS", "ROCK")),
            0,
            outcome
        ),
        outcome = ifelse(
            Var1 == Var2,
            0.5,
            outcome
        ),
        outcome = ifelse(
            is.na(outcome),
            1,
            outcome
        )
    )

# Repeat matches
rpssl_df <- do.call(
    "rbind",
    replicate(10, rpssl_df, simplify = FALSE)
)

rpssl_df <- rpssl_df %>%
    sample_frac(1) %>%
    mutate(
        time_index = 1:n()
    ) %>%
    select(
        time_index,
        throw_1 = Var1,
        throw_2 = Var2,
        outcome
    ) %>% as_tibble()


usethis::use_data(rpssl_df, overwrite = TRUE)


################################################################################
# More complex extended non-transitive data
################################################################################

rpsfw_vec <- c("ROCK", "PAPER", "SCISSORS", "FIRE", "WATER")

rpsfw_df <- expand.grid(
    rpsfw_vec,
    rpsfw_vec,
    stringsAsFactors = FALSE
) %>%
    as_tibble()



rpsfw_df <- rpsfw_df %>%
    arrange(Var1, Var2) %>%
    mutate(
        outcome = NA,
        outcome = ifelse(
            (Var1 == "PAPER") & (Var2 %in% c("SCISSORS", "FIRE")),
            0,
            outcome
        ),
        outcome = ifelse(
            (Var1 == "SCISSORS") & (Var2 %in% c("ROCK", "FIRE")),
            0,
            outcome
        ),
        outcome = ifelse(
            (Var1 == "ROCK") & (Var2 %in% c("PAPER", "FIRE")),
            0,
            outcome
        ),
        outcome = ifelse(
            (Var1 == "FIRE") & (Var2 %in% c("WATER")),
            0,
            outcome
        ),
        outcome = ifelse(
            (Var1 == "WATER") & (Var2 != "FIRE"),
            0,
            outcome
        ),
        outcome = ifelse(
            Var1 == Var2,
            0.5,
            outcome
        ),
        outcome = ifelse(
            is.na(outcome),
            1,
            outcome
        )
    )

# Repeat matches
rpsfw_df <- do.call(
    "rbind",
    replicate(15, rpsfw_df, simplify = FALSE)
)

rpsfw_df <- rpsfw_df %>%
    sample_frac(1) %>%
    mutate(
        time_index = 1:n()
    ) %>%
    select(
        time_index,
        throw_1 = Var1,
        throw_2 = Var2,
        outcome
    # ) %>%
    # filter(
    #     throw_1 != throw_2
    ) %>% as_tibble()



usethis::use_data(rpsfw_df, overwrite = TRUE)


################################################################################

