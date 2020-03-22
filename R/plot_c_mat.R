#' Plot the evolution of the C matrix for a mELO model
#'
#' This function plots the evolution of the elements of the C matrix for a mELO
#' model. The C matrix has a row vector for each player, and k columns. Where k
#' determines the complexity of non-transitive interactions being modelled.
#'
#' @param mELO_model a \code{mELO_rating} object with \code{type = mELO}. The
#' output of a \code{mELO()} function call.
#'
#' @return a plot.
#' @export
#'
#' @examples
#' head(rpsfw_df)
#'
#' # mELO model with k=1
#' rpsfw_mELO_1 <- mELO(rpsfw_df, k=1)
#' plot_c_mat(rpsfw_mELO_1)
#'     # not stable, mELO with k=1 can't accurately model the dynamics
#'
#' # mELO model with k=2
#' rpsfw_mELO_2 <- mELO(rpsfw_df, k=2)
#' plot_c_mat(rpsfw_mELO_2)
#'     # stable, good estimates
plot_c_mat <- function(mELO_model){

    tidy_c_mat_df <- tidyr::gather(
        c_array_as_df(mELO_model$c_mat_history),
        key = "C", value = "value",
        -Player, -time
    )

    ggplot2::ggplot(
        tidy_c_mat_df,
        ggplot2::aes(
            x = time,
            y = value,
            colour = C
        )
    ) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~Player)

}