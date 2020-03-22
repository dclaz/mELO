################################################################################
# Generate clean AFL DATA
################################################################################

library(fitzRoy)
library(dplyr)


# Prepare the data
afl_df_raw <- fitzRoy:::get_match_results()

match_outcome <- function(margin){
    (margin + 0.5) %>%
        pmin(1) %>%
        pmax(0)
}

afl_df <- afl_df_raw %>%
    filter(Date > as.Date("20000101", format="%Y%m%d")) %>%
    arrange(Date) %>%
    mutate(
        match_index = 1:n(),
        #time_index = as.numeric(Date - min(Date)),
        outcome = match_outcome(Margin)
    ) %>%
    select(
        match_index,
        home_team = Home.Team,
        away_team = Away.Team,
        outcome,
        date = Date
    )


usethis::use_data(afl_df, overwrite = TRUE)
