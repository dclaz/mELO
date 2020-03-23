#' AFL match outcomes
#'
#' A data.frame giving a history of AFL match outcomes since the start of the
#' 2000 season.
#'
#' @format A data frame with four variables:
#' \describe{
#' \item{\code{match_index}}{Unique index for the matches.}
#' \item{\code{home_team}}{Home team.}
#' \item{\code{away_team}}{Away team.}
#' \item{\code{outcome}}{Outcome of the match.}
#' \item{\code{date}}{Date of match.}
#' \item{\code{home_team}}{Score for home team.}
#' \item{\code{away_team}}{Score for away team.}
#' }
#'
"afl_df"



#' Rock Paper Scissors data
#'
#' A data.frame of rock-paper-scissors-matches.
#'
#' @format A data frame with four variables:
#' \describe{
#' \item{\code{time_index}}{Time point match was played.}
#' \item{\code{throw_1}}{Throw 1.}
#' \item{\code{throw_2}}{Throw 2.}
#' \item{\code{outcome}}{Outcome of the match.}
#' }
#'
"rps_df"



#' Rock Paper Scissors Spock Lizard data
#'
#' A data.frame of Rock-Paper-Scissors-Spock-Lizard matches. This is a variant
#' of rock-paper-scissors that accommodates 5 different moves with similar
#' non-transitive behaviours.
#'
#' @format A data frame with four variables:
#' \describe{
#' \item{\code{time_index}}{Time point match was played.}
#' \item{\code{throw_1}}{Throw 1.}
#' \item{\code{throw_2}}{Throw 2.}
#' \item{\code{outcome}}{Outcome of the match.}
#' }
#'
"rpssl_df"


#' Rock Paper Scissors Fire Water data
#'
#' A data.frame of Rock-Paper-Scissors-Fire-Water matches. This is a variant
#' of rock-paper-scissors that accommodates 5 different moves but with a more
#' complex non-transitive behaviour.
#'
#' @format A data frame with four variables:
#' \describe{
#' \item{\code{time_index}}{Time point match was played.}
#' \item{\code{throw_1}}{Throw 1.}
#' \item{\code{throw_2}}{Throw 2.}
#' \item{\code{outcome}}{Outcome of the match.}
#' }
#'
"rpsfw_df"



