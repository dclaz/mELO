#' Convert C matrix array to data.frame
#'
#' Converts the C history array from the mELO output to a data.frame. Useful for
#' plotting diagnostics.
#'
#' @param c_mat_array The history of C matrix values for a mELO rating model.
#'
#' @return A data.frame.
#' @export
#'
#' @examples
#' # rock paper scissors fire water model
#' rpsfw_model <- mELO(rpsfw_df, k=2)
#' c_mat_df <- c_array_as_df(rpsfw_model$c_mat_history)
#' head(c_mat_df)
#'
#' # Plot the evolution of C vectors for each throw
#' # Make data a bit 'tidyer'
#' tidy_c_mat_df <- tidyr::gather(
#'     c_mat_df,
#'     key = "C",
#'     value = "value",
#'     -time, -Player
#'     )
#' ggplot2::ggplot(tidy_c_mat_df, ggplot2::aes(x=time,y=value,colour=C)) +
#'     ggplot2::geom_line(size=1.25) +
#'     ggplot2::facet_wrap(~Player)
c_array_as_df <- function(c_mat_array){

    player_names <- rownames(c_mat_array)
    time_idx <- as.numeric(colnames(c_mat_array))

    # Helper function
    c_to_df_helper_fn <- function(x){
        current_df <- as.data.frame(c_mat_array[x,,])
        current_df <- data.frame(
            Player = player_names[x],
            time = time_idx,
            current_df
        )

    }

    c_mat_df_list <- lapply(
        1:dim(c_mat_array)[1],
        c_to_df_helper_fn
    )

    c_mat_df <- do.call(rbind, c_mat_df_list)

    return(c_mat_df)
}
