#' Cette fonction transforme une matrice en matrice de projection
#' @param mat la matrice doit avoir les lignes linéairement indépendantes et que le nombre de colonnes soit inférieur ou égale au nombre de ligne #nolint
#' @return la matrice de projection
#' @examples
#' as_projection(matrix(c(1, 0, 0, 1, 0, 1)))
#' @export


as_projection <- function(mat) {
    # Vérification du type de donnée récupérer
    if (!is.matrix(mat)) stop(cout_error("L'entrée doit être une matrice."))

    # Vérification du nombre de lignes et colonnes
    if (ncol(mat) > nrow(mat)) stop(cout_error("La matrice doit avoir au moins autant de lignes que de colonnes."))
    
    # Vérification de l'inversibilité de t(mat) %*% mat
    if (qr(mat)$rank < ncol(mat)) stop(cout_error("Les colonnes de la matrice ne sont pas linéairement indépendantes."))
    
    # Calcul de la matrice de projection
    idem_mat <- mat %*% solve(t(mat) %*% mat) %*% t(mat)
    return(idem_mat)
}
