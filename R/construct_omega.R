#' onstruct Omega matrix for given k
#'
#' This function constructs the 2k * 2k Omega matrix necessary for calulating
#' and updating mELO ratings.
#'
#' @param k Integer defining the complexity of non-transitive interactions to
#' model.
#'
#' @return a matrix
#' @export
#'
#' @examples
#' construct_omega(1)
#' construct_omega(3)
construct_omega <- function(k){

    E <- diag(2*k)
    omega <- matrix(0, ncol=2*k, nrow=2*k)

    for (i in 1:k){
        omega <- omega +
            E[,2*i-1] %*% t(E[,2*i]) -
            E[,2*i] %*% t(E[,2*i-1])
    }

    return(omega)

}