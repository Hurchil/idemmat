#' Checks if a matrix is idempotent.
#' A matrix A is said to be idempotent if (A^2 = A)
#'
#' @param mat A square matrix.
#' @param tol The tolerance for numerical approximation
#' @return TRUE if mat is idempotent, FALSE otherwise.
#' @examples
#' is_idempotent(matrix(c(1, 0, 0, 0), 2, 2))
#' @export

is_idempotent <- function(mat, tol = 1e-10) {
    # Checking the matrix
    if(!is.matrix(mat) || nrow(mat) != ncol(mat)) stop(cout_error("Please provide a square matrix")) #nolint
    
    # Returns the result with tolerance
    result <- isTRUE(all.equal(mat %*% mat, mat, tolerance = tol))
    return(result)
}
