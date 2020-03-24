
#' Predict results of a game from a ELO or mELO model
#'
#' This function gives predictions of success probabilities for agent or
#' player 1 from a fitted ELO or mELO model (a \code{mELO_rating} oject).
#'
#' @param object An object of class \code{mELO_rating}.
#' @param new_match_data A data frame containing four columns: (1) a numeric
#' vector denoting the time period in which the game took place (2) a numeric
#' or character identifier for player one (3) a numeric or character identifier
#' for player two.
#' @param min_games A single value. If the number of games of either player is
#' below this value, the prediction will be based on the \code{default_ratings}
#' parameter.
#' @param default_ratings The rating to be used for agents or players who have
#' not yet played \code{min_games}.
#' @param p1_advantage Player 1 advantage parameter. A single value or numeric
#' vector with length equal to the number of rows in \code{new_match_data}. Can
#' be though of as representing first move or home ground advantage.
#' @param thresh A single value. If given, a binary vector is returned
#' indicating whether the prediction is greater than this value.
#'
#' @return A numeric vector of predictions, which may contain missing values.
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
predict.mELO_rating <- function(
    object,
    new_match_data,
    min_games = 15,           # min number of games before actual rating is used
    default_ratings = NULL,   # before tng games, ratings will be based on this value
    p1_advantage = 0,
    thresh
){

    # Stops
    if (class(object) != "mELO_rating"){
        stop("Model object must be of class mELO_rating")
    }
    if (!(object$type %in% c("ELO", "mELO"))){
        stop("model type must be ELO or mELO")
    }

    if (
        missing(new_match_data) ||
        (nrow(new_match_data) == 0)
    ){
        stop("'newdata' must be non-missing and have non-zero rows")
    }

    if (!is.null(default_ratings)) {
        if (length(default_ratings) != 1){
            stop("'default_ratings' must be vector of length one")
        }
    }

    if (!is.null(default_ratings)){
        object$Rating[object$Games < min_games] <- default_ratings[1]
    } else {
        is.na(object$Rating[object$Games < min_games]) <- TRUE
    }

    # Prepare objects
    ratings <- object$ratings
    players <- ratings$Player
    player_1 <- match(new_match_data[[2]], players)
    player_2 <- match(new_match_data[[3]], players)
    alpha <- log(10)/400

    # Fix player for easy tabulations
    new_match_data$player_1 <- match(new_match_data[[2]], players)
    new_match_data$player_2 <- match(new_match_data[[3]], players)

    p1_rating <- ratings$Rating[player_1]
    p2_rating <- ratings$Rating[player_2]

    if (!is.null(default_ratings)){
        p1_rating[is.na(p1_rating)] <- default_ratings[1]
    }
    if (!is.null(default_ratings)){
        p2_rating[is.na(p2_rating)] <- default_ratings[1]
    }


    # Calculate predictions for player 1
    # ELO
    if (object$type == "ELO"){
        preds <- sigmoid(
            alpha*(p1_rating + p1_advantage - p2_rating)
        )
    }

    # mELO
    if (object$type == "mELO"){
        # set up data
        current_c_mat <- object$c_mat

        # rearrange C matrix to match the new play indexes
        current_c_mat <- current_c_mat[match(rownames(current_c_mat), players),]

        # COnstruct Omega matrix
        omega_mat <- construct_omega(k = object$k)

        # Calculate adjustment for all matchups
        adjustment_mat <- (current_c_mat) %*% omega_mat %*% t(current_c_mat)

        # Get adjustment values for each of the matches in terms of p1
        p_1_adjustment <- adjustment_mat[
            cbind(
                new_match_data$player_1,
                new_match_data$player_2
            )
        ]

        # Expected win probabilities
        preds <- sigmoid(
            alpha*(
                p1_rating +
                    p1_advantage -
                    p2_rating +
                    p_1_adjustment
            )
        )
    }

    # Threshold predictions if required
    if (!missing(thresh))
        preds <- as.numeric(preds >= thresh)

    # Return the predictions
    return(preds)
}



