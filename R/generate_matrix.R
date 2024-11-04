#' This function generates an idempotent matrix from the given dimensions
#' @param n defines the dimensions of the matrix n*n
#' @return returns an idempotent matrix of dimensions n*n
#' @examples
#' gen_matrix(8)
#' @export

gen_matrix <- function(n) {
    if(!is.numeric(n) || length(n) != 1 || n <= 0) stop(cout_error("Please enter a positive integer")) #nolint
    mat <- matrix(rnorm(n * n), nrow = n)
    return(as_projection(mat))
}