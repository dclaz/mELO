#' Estimate C matrix from an Advantage/Disadvantage matrix
#'
#' Use this function to estimate a possible C matrix from a skew-symmetric A
#' matrix. Recall that \eqn{A = C \Omega C'}. Combined with est_A_from_data(),
#' this might be useful for warm starting mELO models.
#'
#' @param A_mat An advantage/disadvantage matrix.
#' @param k The number of columns of the C matrix. More complex
#' non-transitive interactions require higher values of k.
#'
#' @return A list with three components:
#' \describe{
#'   \item{C_mat}{The estimated C matrix}
#'   \item{error}{The squared reconstruction error}
#'   \item{est_req_k}{The estimated required value for k to minimise the
#'   reconstruction error.}
#' }
#' @export
#'
#' @examples
#' # Generate C mat
#' set.seed(112123)
#' m <- 4
#' k <- 1
#' C_mat <- matrix(
#'     runif(m*2*k, 0, 10),
#'   ncol = 2*k
#' )
#'
#'
#' Omega_mat <- construct_omega(k)
#' A_mat <- C_mat %*% Omega_mat %*% t(C_mat)
#' A_mat
#'
#' C_mat_list <- find_C_given_A(A_mat)
#' C_mat_list
est_C_given_A <- function(
    A_mat,
    k = 1
){

    Omega_mat <- construct_omega(k)

    error_fn <- function(C_vec){
        C_mat <- matrix(C_vec, ncol=2*k, nrow=ncol(A_mat))
        error <- sum((A_mat - C_mat %*% Omega_mat %*% t(C_mat))^2)
    }

    optim_C <- optim(
        rnorm(ncol(A_mat)*2*k),
        error_fn,
        method = "BFGS"
    )

    C_mat <- matrix(optim_C$par, ncol=2*k, nrow=ncol(A_mat))

    A_svd_d <- round(svd(A_mat)$d, 2)
    est_req_k <- length(unique(A_svd_d[A_svd_d > 0]))

    results <- list()
    results$C_mat <- C_mat
    results$error <- optim_C$value
    results$est_req_k <- est_req_k

    return(results)
}


