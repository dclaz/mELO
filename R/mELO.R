#' mELO Ratings
#'
#' This function implements the multidimensional ELO (mELO) rating system for
#' performing pairwise comparisons. The mELO rating system has the desirable
#' properties of being able to handle cyclic interactions and is better behaved
#' in the presence of redundant copies of players/agents or tasks.
#'
#' @param match_data A data frame containing four columns: (1) a numeric
#' vector denoting the time period in which the game took place (2) a numeric
#' or character identifier for player one (3) a numeric or character identifier
#' for player two and (4) the result of the game expressed as a number,
#' typically equal to one for a player one win, zero for a player two win and
#' one half for a draw.
#' @param init_data Initialise a rating model with \code{ratings} component of the
#' returned list of the output from a previous \code{mELO()} call, or a data.frame with
#' columns Player and Rating.
#' @param init_c_mat Initialise a rating model with \code{c_mat} component of the
#' returned list of the output from a previous \code{mELO()} call, or a matrix with
#' with dimensions (#agents, k). Recall the C matrix encodes the non-transitive
#' interactions of the agents.
#' @param init_rating The initial rating for players not appearing in
#' \code{init_data}.
#' @param k Integer defining the complexity of non-transitive interactions to model.
#' @param eta_1 Learning rate for the ratings vector.
#' @param eta_2 Learning rate for elements of the C matrix.
#' @param p1_advantage Player 1 advtange parameter. Either a single value or a
#' vector equal to the number of rows in \code{match_data}.
#' @param save_history If \code{TRUE} return the rating history for each player.
#' @param sort If \code{TRUE}, sort the output ratings from highest to lowest.
#'
#' @return A list object of class "\code{mELO_rating}" with the following
#' components:
#' \describe{
#'   \item{ratings}{A data frame of the results at the end of the final time period.
#'   The variables are self explanatory except for Lag, which represents the
#'   number of time periods since the player last played a game. This is equal
#'   to zero for players who played in the latest time period, and is also zero
#'   for players who have not yet played any games.}
#'   \item{history}{An array containing the ratings history for all
#'   players.}
#'   \item{c_mat}{An estimate of the C matrix.}
#'   \item{c_mat_history}{An array containing the history of the C matrix.}
#'   \item{p1_advantage}{A single value or a vector values for the
#'   advantage Player 1 had.}
#'   \item{k}{Integer defining the complexity of non-transitive interactions to model.}
#'   \item{eta_1}{Learning rate for the ratings vector.}
#'   \item{eta_2}{Learning rate for elements of the C matrix.}
#'   \item{type}{The type of model. In this case, "mELO".}
#'   \item{preds}{The player 1 success probabilities predicted prior to adjusting to the
#'   outcome of the match.}
#'   \item{outcomes}{The outcome for player 1 for each match.}
#'   \item{preds_logloss}{The mean logloss error for all predictions.}
#'
#' }
#' @references
#' Balduzzi, David, et al. "Re-evaluating Evaluation." arXiv preprint arXiv:1806.02643 (2018).
#' @export
#'
#' @examples
#' # Rock paper scissors
#' head(rps_df)
#'
#' # Note that ELO doesn't perform well
#' rps_ELO <- ELO(rps_df)
#' rps_ELO
#' ELO_preds <- predict(
#'     rps_ELO,
#'     head(rps_df)
#' )
#' cbind(
#'     head(rps_df),
#'     ELO_preds
#' )
#'     # Predictions are all ~0.5
#'
#' # Fit a mELO model that can handle these types of interactions.
#' rps_mELO <- mELO(rps_df, k=1)
#' rps_mELO
#' # Inspect advantage matrix
#' get_adv_mat(rps_mELO)
#' # Get predictioncs
#' mELO_preds <- predict(
#'     rps_mELO,
#'     head(rps_df)
#' )
#' cbind(
#'     head(rps_df),
#'     mELO_preds
#' )
#'     # Much better predictions!
mELO <- function(
    match_data,
    init_data = NULL,
    init_c_mat = NULL,
    init_rating = 2200,
    k = ifelse(
        !is.null(init_c_mat),
        ncol(as.matrix(init_c_mat)),
        1
    ),
    eta_1 = 27,
    eta_2 = 1,
    p1_advantage = 0,
    save_history = TRUE,
    sort = TRUE
){
    # Stops
    if (!is.data.frame(match_data)){
        stop("match_data must be a data.frame")
    }
    if (!is.data.frame(init_data) && !is.null(init_data)){
        stop("init_data must be a data.frame")
    }
    # if (ncol(match_data) != 4)
    #     stop("match_data must have four variables")
    if (nrow(match_data) == 0) {
        stop("match_data is empty and 'status' is NULL")
    }
    if (length(init_rating) != 1){
        stop("There is no matches in match_data")
    }
    if (
        !(
            (length(p1_advantage) == 1) |
            (length(p1_advantage) == nrow(match_data))
        )
    ){
        stop("the length of 'p1_advantage' must be one or nrow(match_data)")
    }

    if (!is.null(init_c_mat)){
        if(!is.matrix(init_c_mat)){
            stop("init_c_mat must be a matrix")
        }
    }

    if (is.matrix(init_c_mat)){
        if (ncol(init_c_mat) != 2*k){
            stop("init_c_mat must have 2*k columns")
        }
    }

    if (k < 1){
        stop("k must be greater than 0")
    }
    if (k %% 1 != 0){
        stop("k must be an integer")
    }

    # Fix names, and then do more stops
    names(match_data) <- c("time_index", "player_1", "player_2", "outcome")

    if (!is.numeric(match_data$time_index)){
        stop("Time period must be numeric or date")
    }
    if (!is.numeric(match_data$player_1) &&
        !is.character(match_data$player_1)){
        stop("Player identifiers must be numeric or character")
    }
    if (!is.numeric(match_data$player_2) &&
        !is.character(match_data$player_2)){
        stop("Player identifiers must be numeric or character")
    }
    if (
        !is.numeric(match_data$outcome)
        || any(match_data$outcome > 1)
        || any(match_data$outcome < 0)
    ){
        stop("outcomes must be in the interval [0,1]")
    }


    # vector of players
    players <- sort(unique(c(match_data$player_1, match_data$player_2)))

    # counts of players and periods
    n_players <- length(players)
    n_times <- length(unique(match_data$time_index))

    # Fix player for easy tabulations
    match_data$player_1 <- match(match_data$player_1, players)
    match_data$player_2 <- match(match_data$player_2, players)

    # If we have data to initialise the model
    # Note we don't need to initialise all playwers
    if (!is.null(init_data)) {
        new_players_not_in_match_data <- players[!(players %in% init_data$Player)]
        np_zero_vec <- rep(0, length(new_players_not_in_match_data))
        new_player_status <- data.frame(
            Player = new_players_not_in_match_data,
            Rating = rep(init_rating, length(new_players_not_in_match_data)),
            Games = np_zero_vec,
            Win = np_zero_vec,
            Draw = np_zero_vec,
            Loss = np_zero_vec,
            Lag = np_zero_vec
        )

        if (!("Games" %in% names(init_data)))
            init_data <- cbind(init_data, Games = 0)
        if (!("Win" %in% names(init_data)))
            init_data <- cbind(init_data, Win = 0)
        if (!("Draw" %in% names(init_data)))
            init_data <- cbind(init_data, Draw = 0)
        if (!("Loss" %in% names(init_data)))
            init_data <- cbind(init_data, Loss = 0)
        if (!("Lag" %in% names(init_data)))
            init_data <- cbind(init_data, Lag = 0)
        init_data <- rbind(
            init_data[,
                      c("Player",
                        "Rating",
                        "Games",
                        "Win",
                        "Draw",
                        "Loss",
                        "Lag")
                      ],
            new_player_status
        )

        ratings_init <- init_data[[2]]
        n_games <- init_data[[3]]
        n_win <- init_data[[4]]
        n_draw <- init_data[[5]]
        n_loss <- init_data[[6]]
        n_lag <- init_data[[7]]
        names(ratings_init) <- names(n_games) <- init_data$Player

        # c matrix initialise
        c_mat_init <- init_c_mat

    } else {

        # In the case we do not initialise ANY of the players with previous ratings
        # We only need to care about players in the match data!
        ratings_init <- rep(init_rating, length.out = n_players)
        n_games <- n_win <- n_draw <- n_loss <- n_lag <- rep(0, length.out = n_players)
        names(ratings_init) <- names(n_games) <- names(n_lag) <- players

        # initialise matrix of C vectors
        c_mat_init <- matrix(
            #1, doesn't work
            rnorm(2*k*n_players, 10, 2),
            ncol = 2*k,
            nrow = n_players
        )

        # Think of a way to decompose an observed WIN/LOSS matrix in to a C
        # matrix for better initial params?

        # Fix names
        rownames(c_mat_init) <- players
        colnames(c_mat_init) <- paste0("c_", seq(1,2*k, length.out = 2*k))

    }

    # Make sure everything is still okay
    if (!all(names(ratings_init) == names(n_games)))
        stop("names of ratings and ngames are different")
    if (!all(players %in% names(ratings_init)))
        stop("Payers in data are not within current status")

    # Initialise all the other parameters
    current_players <- match(players, names(ratings_init))

    # stats for players NOT in the match data
    other_ratings <- ratings_init[-current_players]
    other_n_games <- n_games[-current_players]
    other_n_win <- n_win[-current_players]
    other_n_draw <- n_draw[-current_players]
    other_n_loss <- n_loss[-current_players]
    other_n_lag <- n_lag[-current_players]
    other_n_lag[other_n_games != 0] <- other_n_lag[other_n_games != 0] + n_times


    # stats for players IN the match data
    n_games <- n_games[current_players]
    n_win <- n_win[current_players]
    n_draw <- n_draw[current_players]
    n_loss <- n_loss[current_players]
    n_lag <- n_lag[current_players]

    # Initialise ratings of players who will play
    current_ratings <- ratings_init[current_players]
    current_c_mat <- c_mat_init[current_players,]

    # Slit data for each training period
    matches <- split(match_data, match_data$time_index)

    # Prepare p1_advantage vector
    if (length(p1_advantage) == 1){
        p1_advantage <- p1_advantage_vec <- rep(p1_advantage, nrow(match_data))
    }
    p1_advantage <- split(p1_advantage, match_data$time_index)

    # Construct omega matrix
    omega_mat <- construct_omega(k)

    # Record history of ratings?
    if (save_history){
        # Create array to store the history in
        history_array <- array(
            NA,
            dim = c(n_players, n_times, 3),
            dimnames = list(
                players,
                1:n_times,
                c("rating", "games", "lag")
            )
        )

        # How will we store c vectors, an array
        # 1:n_player x 1:n_times x 2k
        c_mat_array <- array(
            NA,
            dim = c(n_players, n_times, 2*k),
            dimnames = list(
                players,
                1:n_times,
                paste0("c_", 1:(2*k))
            )
        )
    }

    # constants used in calculation
    alpha <- log(10)/400

    # vector to store predictions and outcomes in
    # these are used to calculate prediction logloss
    p1_all_preds <- numeric(0)
    p1_all_outcomes <- numeric(0)

    # Begin loop through time points
    for (i in 1:n_times){

        # Current matchs
        current_matches <- matches[[i]]
        # Players in the current matches
        current_match_players <- c(current_matches$player_1, current_matches$player_2)

        # Extract current ratings for players
        ratings_1 <- current_ratings[current_matches$player_1 ]
        ratings_2 <- current_ratings[current_matches$player_2 ]


        # Calculate adjustment for all matchups
        adjustment_mat <- (current_c_mat) %*% omega_mat %*% t(current_c_mat)

        # Get adjustment values for each of the matches in terms of p1
        p_1_adjustment <- adjustment_mat[
            cbind(
                current_matches$player_1,
                current_matches$player_2
            )
        ]

        # Expected win probabilities
        p_1 <- sigmoid(
            alpha*(
                ratings_1 +
                    p1_advantage[[i]] -
                    ratings_2 +
                    p_1_adjustment
            )
        )

        p_2 <- 1 - p_1
        names(p_2) <- names(ratings_2)

        # Actual outcomes for all players
        outcomes <- c(
            current_matches$outcome,
            abs(current_matches$outcome - 1)
        )


        # delta between outcome and predictions
        deltas <- outcomes - c(p_1, p_2)

        # get all the matches played by each team together
        deltas_list <- split(deltas, current_match_players)
        # pop them in to a matrix
        delta_mat <- matrix(
            unlist(deltas_list),
            nrow = length(unique(current_match_players)),
            byrow = TRUE
        )

        current_players_in_list <- as.numeric(names(deltas_list))
        delta_vec <- rep(0, n_players)
        delta_vec[current_players_in_list] <-  rowSums(delta_mat)


        # Update the ratings (gradient descent step!)
        # K factor (the learning rate)
        eta_1_vec <- eta_1*tabulate(unique(current_match_players), n_players)

        # ratings
        current_ratings <- current_ratings + eta_1_vec * delta_vec

        # Update the C matrix
        match_mat <- as.matrix(current_matches[,c(2,3)])

        # Matrix to store temp updates in
        current_c_mat_temp <- current_c_mat

        for (j in 1:nrow(match_mat)){
            current_combo <- match_mat[j,]
            x <- current_combo[1]
            y <- current_combo[2]

            current_c_mat_temp[x,] <- current_c_mat[x,] + eta_2*deltas[j] * t(omega_mat%*%current_c_mat[y,])
            current_c_mat_temp[y,] <- current_c_mat[y,] - eta_2*deltas[j] * t(omega_mat%*%current_c_mat[x,])

            # Update entries before moving to next match
            current_c_mat <- current_c_mat_temp

        }

        # # Update entries before moving to next match
        # current_c_mat <- current_c_mat_temp


        # Acculate p1 preds and outcomes for logloss calcs
        p1_all_preds <- c(p1_all_preds, p_1)
        p1_all_outcomes <- c(p1_all_outcomes, current_matches$outcome)


        # Collect the stats
        current_wins <- c(
            current_matches$player_1[current_matches$outcome == 1],
            current_matches$player_2[current_matches$outcome == 0]
        )
        current_draws <- c(
            current_matches$player_1[current_matches$outcome == 0.5],
            current_matches$player_2[current_matches$outcome == 0.5]
        )
        current_losses <- c(
            current_matches$player_1[current_matches$outcome == 0],
            current_matches$player_2[current_matches$outcome == 1]
        )

        # #Update the cumulative stats
        n_games <- n_games + tabulate(current_match_players, n_players)
        n_win <- n_win + tabulate(current_wins, n_players)
        n_draw <- n_draw + tabulate(current_draws, n_players)
        n_loss <- n_loss + tabulate(current_losses, n_players)
        unique_current_players <- unique(current_match_players)
        n_lag[n_games != 0] <- n_lag[n_games != 0] + 1
        n_lag[unique_current_players] <- 0

        # Update history array
        if (save_history){
            history_array[, i, 1] <- current_ratings
            history_array[, i, 2] <- n_games
            history_array[, i, 3] <- n_lag

            c_mat_array[, i, ] <- current_c_mat

        }


    }
    # End loop through time points


    # Calculate logloss for predictions
    preds_logloss <- logloss(
        p1_all_preds,
        p1_all_outcomes
    )

    # Create objects for things we didnt chose to store
    if (!save_history){
        history_array <- NULL
        c_mat_array <- NULL
    }

    # Prepare primary output data.frame
    players <- suppressWarnings(as.numeric(names(c(current_ratings, other_ratings))))

    if (any(is.na(players))){
        players <- names(c(current_ratings, other_ratings))
    }

    output_df <- data.frame(
        Player = players,
        Rating = c(current_ratings, other_ratings),
        Games = c(n_games, other_n_games),
        Win = c(n_win, other_n_win),
        Draw = c(n_draw, other_n_draw),
        Loss = c(n_loss, other_n_loss),
        Lag = c(n_lag, other_n_lag),
        stringsAsFactors = FALSE
    )

    # Sort if desired
    if (sort){
        output_df <- output_df[order(output_df$Rating, decreasing = TRUE), ]
    } else {
        output_df <- output_df[order(output_df$Player), ]
    }

    # Fix rownames
    row.names(output_df) <- NULL

    # Prepare output list
    output_list <- list(
        ratings = output_df,
        history = history_array,
        c_mat = current_c_mat,
        c_mat_history = c_mat_array,
        p1_advantage = p1_advantage_vec,
        k = k,
        eta_1 = eta_1,
        eta_2 = eta_2,
        type = "mELO",
        preds = p1_all_preds,
        outcomes = p1_all_outcomes,
        preds_logloss = preds_logloss
    )

    # Set class of output
    class(output_list) <- "mELO_rating"

    # And finally return all the output
    return(output_list)

}

