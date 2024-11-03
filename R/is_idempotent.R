#' Vérifie si une matrice est idempotente.
#' Une A est dite idempotente si (A^2 = A)
#'
#' @param mat Une matrice carrée.
#' @param tol La tolérance pour les approximation numérique
#' @return TRUE si mat est idempotente , FALSE sinon.
#' @examples
#' is_idempotent(matrix(c(1, 0, 0, 0), 2, 2))
#' @export




is_idempotent <- function(mat, tol = 1e-10) {
    # Vérification de matrice
    if(!is.matrix(mat) || nrow(mat) != ncol(mat)) stop(cout_error("Veuillez Fournir une matrice carrée")) #nolint
    
    # Renvoie la réponse avec la tolérance
    result <- isTRUE(all.equal(mat %*% mat, mat, tolerance = tol))
    return(result)
}
