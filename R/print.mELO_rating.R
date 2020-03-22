
#' Print simple output of mELO_rating object to screen
#'
#' @param modelled_ratings A mELO_rating object.
#' @param digits Number of significant digits in the ratings column to be output.
#'
#' @return text to screen
#' @export
#'
#' @examples
#' # rock paper scissors model
#' rps_model <- mELO(rps_df, k=1)
#' # print.mELO_rating is automatically called:
#' rps_model
print.mELO_rating <- function (
    modelled_ratings,
    digits = 1
){

    output_df <- modelled_ratings$ratings
    output_df$Rating <- round(output_df$Rating, digits)
    n_players <- nrow(output_df)
    n_games <- round(sum(output_df$Games)/2)

    cat(
        paste("\n", modelled_ratings$type, " Ratings For ", n_players,
              " Players Playing ", n_games, " Games\n\n",
              sep = ""
        )
    )
    if (modelled_ratings$type == "mELO"){
        cat(
            paste(
                "k = ", modelled_ratings$k, ".\n\n",
                sep = ""
            )
        )
    }


    print(output_df[1:(min(1000, n_players)), 1:ncol(output_df), drop = FALSE])
    if (n_players > 1000)
        cat("\nOutput Tructated To First 1000 Players \n")
    cat("\n")
    invisible()

}


