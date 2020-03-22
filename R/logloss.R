#' Calculate logloss for evaluating predictions
#'
#' Calculate logloss or cross-entropy for a set of predictions.
#'
#' @param prediction A vector of estimated probabilities.
#' @param outcome A vector of observed outcomes.
#' @param tol Numerical tolerance. Can also be used to threshold errors
#' for really bad predictions, or when you don't want a model to be penalized
#' too strongly in the presence of high dispersion. Default is
#' .Machine$double.neg.eps.
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' preds <- c(0.9, 0.1, 0.8, 0.5)
#' outcomes <- c(1, 0, 1, 0)
#' logloss(preds, outcomes)
#'
#' # Thresholding large errors for bad predictions
#' preds <- c(0.000001)
#' outcomes <- c(1)
#' logloss(preds, outcomes)
#' logloss(preds, outcomes, 0.01)
logloss <- function(
    prediction,
    outcome,
    tol = .Machine$double.neg.eps
) {
    if (length(outcome) !=  length(prediction)){
        stop("Observed outcome and predicted outcome need to be equal lengths!")
    }

    #pred_capped <- prediction
    pred_capped <- pmin(1-tol, prediction)
    pred_capped <- pmax(tol, pred_capped)

    # calculate logloss
    -sum(outcome*log(pred_capped) + (1 - outcome)*log(1 - pred_capped)) / length(outcome)
}
