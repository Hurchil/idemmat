#' This function transforms a matrix into a projection matrix
#' @param mat the matrix must have linearly independent rows and the number of columns must be less than or equal to the number of rows #nolint
#' @return the projection matrix
#' @examples
#' as_projection(matrix(c(1, 0, 0, 1, 0, 1)))
#' @export


as_projection <- function(mat) {
    # Checking the type of data retrieved
    if (!is.matrix(mat)) stop(cout_error("The input must be a matrix."))

    # Checking the number of rows and columns
    if (ncol(mat) > nrow(mat)) stop(cout_error("The matrix must have at least as many rows as columns."))
    
    # Checking the invertibility of t(mat) %*% mat
    if (qr(mat)$rank < ncol(mat)) stop(cout_error("The columns of the matrix are not linearly independent."))
    
    # Calculating the projection matrix
    idem_mat <- mat %*% solve(t(mat) %*% mat) %*% t(mat)
    return(idem_mat)
}
