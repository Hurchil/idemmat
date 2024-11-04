#' This function returns the Frobenius norm distance between two matrices
#' It will be much more used to evaluate the validity of idempotent matrices approximated from an initial matrix
#' @param A the first matrix
#' @param B the second matrix
#' @return returns the Frobenius norm distance
#' @examples
#' frob_norm(matrix(c(1, 2, 2, 4), nrow = 2), t(matrix(c(1, 2, 2, 4), nrow = 2)))
#' frob_norm(matrix(c(1, 2, 2, 4), nrow = 2))
#' @export

frob_norm <- function(A, B = A * 0){

    # In the case where two matrices are provided, check if they have the same dimensions
    if(any(dim(A) != dim(B))) stop(cout_error("The matrices do not have the same dimensions"))

    # Calculate the Frobenius norm distance
    # Will give the norm of A simply with B = 0 for the case where only one matrix is provided
    distance <- sqrt(sum((A - B)^ 2))
    return(distance)
}
