#' Get advantage matrix from a mELO model
#'
#' Returns the player to player advantage/disadvantage matrix for a mELO model.
#' the matrix is calculated as \eqn{C \Omega C'}.
#'
#' @param model a \code{mELO_rating} model created with a \code{mELO()} function.
#'
#' @return a matrix
#' @export
#'
#' @examples
#' # rock paper scissors model
#' rps_model <- mELO(rps_df, k=1)
#' get_adv_mat(rps_model)
#'
#' # rock paper scissors fire water model
#' rpsfw_model <- mELO(rpsfw_df, k=2)
#' get_adv_mat(rpsfw_model)
get_adv_mat <- function(model){
    if (model$type != "mELO"){
        stop("Must be a mELO rating model to obtain an advantage matrix")
    }

    omega_mat <- construct_omega(model$k)
    c_mat <- model$c_mat
    adv_mat <- (c_mat) %*% omega_mat %*% t(c_mat)

    return(adv_mat)
}