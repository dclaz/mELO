find_C_given_A <- function(A_mat, k=1){

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

# Generate C mat
set.seed(112123)

m <- 40
k <- 2
C_mat <- matrix(
    runif(m*2*k, 0, 10),
    ncol = 2*k
)


Omega_mat <- construct_omega(k)
A_mat <- C_mat %*% Omega_mat %*% t(C_mat)

# Now lets decompose A and see if we can get back some reconstruction of C
svd_A <- svd(A_mat) #, nu = 2*k, nv = 2*k)
svd_A_2 <- svd(A_mat, nu = 2*k, nv = 2*k)

# yes this equals A
svd_A$u %*% diag(svd_A$d) %*% t(svd_A$v)
svd_A_2$u %*% diag(svd_A_2$d[1:(2*k)]) %*% t(svd_A_2$v)

# The components
D <- diag(svd_A_2$d[1:(2*k)])
U <- svd_A_2$u
V <- svd_A_2$v
Vt <- t(svd_A_2$v)

svd_A$d %>% round(3)


find_C_given_A(A_mat, k=1)
find_C_given_A(A_mat, k=2)
find_C_given_A(A_mat, k=3)

################################################################################


temp_A_from_data <- function(data){
    mat_temp <- data %>%
        group_by(throw_1, throw_2) %>%
        summarise(outcome = sum(outcome)) %>%
        ungroup() %>%
        spread(throw_2, outcome) %>%
        select(-throw_1) %>%
        as.matrix()

    mat <- mat_temp - t(mat_temp)
    mat <- mat %>% pmin(1) %>% pmax(-1)
    rownames(mat) <- colnames(mat) <- colnames(mat_temp)

    return(mat)
}

A_rps <- temp_A_from_data(rps_df)
A_rps[is.na(A_rps)] <- 0
A_rpssl <- temp_A_from_data(rpssl_df)
A_rpsfw <- temp_A_from_data(rpsfw_df)

find_C_given_A(A_rps)
find_C_given_A(A_rpssl, k=1)
find_C_given_A(A_rpsfw, k=2)

svd(A_rps)$d %>% round(3)
svd(A_rpssl)$d %>% round(3)
svd(A_rpsfw)$d %>% round(3)


rpssl_mELO <- mELO(rpssl_df, k=1)


estimatge_A_from_data <- function(
    data,
    baseline = 2200
){



}




