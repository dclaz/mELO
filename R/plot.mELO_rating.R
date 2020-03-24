#' Plot historical player ratings for a mELO_rating object
#'
#' Plot historical player ratings for a \code{mELO_rating} object fit with the
#' \code{ELO()} or \code{mELO()} functions.
#'
#' @param modelled_ratings An object of class \code{mELO_rating}.
#' @param x_labels Custom labels for the x axis.
#' @param xlab Title for x axis.
#' @param ylab Title for y axis.
#' @param main Title for the plot.
#'
#' @return a plot
#' @export
#'
#' @examples
#' # History of AFL ratings
#' head(afl_df)
#'
#' afl_ELO <- ELO(afl_df[,1:4])
#' plot(afl_ELO)
plot.mELO_rating <- function(
    modelled_ratings,
    xlab = "Time Period",
    ylab = paste(modelled_ratings$type, "Ratings"),
    main = NULL
){

    # Stops
    if (class(modelled_ratings) != "mELO_rating"){
        stop("Model object must be of class mELO_rating")
    }
    if (!(modelled_ratings$type %in% c("ELO", "mELO"))){
        stop("model type must be ELO or mELO")
    }

    if (is.null(modelled_ratings$history)){
        stop("Need Full History For Plotting")
    }

    # plot title
    if(is.null(main)){
        if (modelled_ratings$type == "mELO"){
            main <- paste0(
                modelled_ratings$type,
                " Ratings System. k = ", modelled_ratings$k, "."
            )
        }

        if (modelled_ratings$type == "ELO"){
            main <- paste0(
                modelled_ratings$type,
                " Ratings System."
            )
        }
    }

    dim_history <- dim(modelled_ratings$history)
    n_players <- dim_history[1]

    t_0 <- 1
    t_n <- dim_history[2]
    x_labels <- t_0:t_n

    ratings <- modelled_ratings$history[, t_0:t_n, "rating"]
    games <- modelled_ratings$history[, t_0:t_n, "games"]

    players <- order(games[, 1], decreasing = TRUE)[1:n_players]

    ratings <- t(as.matrix(ratings[players, ]))
    matplot(
        x_labels,
        ratings,
        type = "l",
        xlab = xlab,
        ylab = ylab,
        main = main
    )

    invisible(0)

}



