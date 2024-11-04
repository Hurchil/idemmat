#' This function approximates a matrix "mat" by an idempotent matrix "result"
#' using singular value decomposition (SVD)
#' @param mat the matrix
#' @return the idempotent matrix approximating mat
#' @examples
#' as_approximate_svd(matrix(c(1, 2, 2, 4), nrow = 2))
#' @export

as_approximate_svd <- function(mat){
    if(!is.matrix(mat)) stop(cout_error("Please provide a matrix"))

    # Singular value decomposition
    svd_decomp <- svd(mat)
    U <- svd_decomp$u

    approx_mat <- U %*% t(U)

    return(approx_mat)
}



# ideminfinite <- function(){
#     i <- 100
#     while(i > 0){
#         matif <- matrix(runif(100), ncol = 10)
#         matnorm <- matrix(rnorm(100), ncol = 10)
#         matsample <- matrix(sample(-100:0, 100, replace = TRUE), ncol = 10)

#         s1 <- svd(matif)
#         s2 <- svd(matnorm)
#         s3 <- svd(matsample)

#         print(is_idempotent(s1$u %*% t(s1$u)))
#         print(is_idempotent(s2$u %*% t(s2$u)))
#         print(is_idempotent(s3$u %*% t(s3$u)))

#         i <- i - 1
#     }
# }


# ideminfinite()
