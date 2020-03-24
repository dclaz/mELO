#

#' Return pairwise predictions for specified agents
#'
#' Given a mELO_rating model and a vector of agents, this function gives the
#' predicted success probabilities for each combination. This is useful for mELO
#' models as the ratings alone is not useful for predicting outcomes.
#'
#' @param model An object of class \code{mELO_rating}.
#' @param agents a vector of agents/players/teams you would like to pairwise
#' predictions for.
#' @param round Round results to how many figures. Default is 4.
#' @param actual_matrix Should result be an actual matrix? Default is FALSE and
#' the output is a data.frame.
#'
#' @return a data.frame.
#' @export
#'
#' @examples
#' # Fit a mELO model that can handle these types of interactions.
#' rps_mELO <- mELO(rps_df, k=1)
#' rps_mELO
#'
#' # Get pairwise predictions
#' model_pred_mat(
#'    rps_mELO,
#'    unique(rps_df[[2]])
#' )
model_pred_mat <- function(
    model,
    agents,
    round = 4
){
    # Stops
    if (!(model$type %in% c("ELO", "mELO"))){
        stop("model type must be ELO or mELO")
    }

    if (class(model) != "mELO_rating"){
        stop("Model object must be of class mELO_rating")
    }

    # vector of agents
    agents <- agents %>%
        unique() %>%
        sort()

    # data.frame giving all combinations
    pred_mat_df <- data.frame(
        index = 1:(length(agents)^2),
        expand.grid(
            agent_1 = agents,
            agent_2 = agents
        ),
        stringsAsFactors = FALSE
    )


    # bind predictions to match data.frame
    pred_combos_df <- cbind(
        pred_mat_df,
        pred = predict(
            model,
            pred_mat_df
        ) %>%
            round(round)
    )[,-1]

    # spread
    pred_combos_mat <- pred_combos_df %>%
        tidyr::spread(agent_2, pred)

    # fix matrix
    row_names_vec <- pred_combos_mat[,1]
    pred_combos_mat <- as.matrix(pred_combos_mat[,-1])
    rownames(pred_combos_mat) <- row_names_vec

    return(pred_combos_mat)

}
