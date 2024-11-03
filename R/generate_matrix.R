#' Cette fonction génère une matrice idempotente à partir des dimensions données
#' @param n définis les dimensions de la matrice n*n
#' @return retourne une matrice idempotentes de dimensions n*n
#' @examples
#' gen_matrix(8)
#' @export

gen_matrix <- function(n) {
    if(!is.numeric(n) || length(n) != 1 || n <= 0) stop(cout_error("Veuillez entrer un nombre entier positif")) #nolint
    mat <- matrix(rnorm(n * n), nrow = n)
    return(as_projection(mat))
}