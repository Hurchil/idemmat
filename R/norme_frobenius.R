#' Cette fonction renvoie la distance en norme de frobenius entre deux matrices
#' Il sera beaucoup plus utilisé pour évaluer la validité des matrices idempotentes approché par à partir d'une matrice initial
#' @param A la première matrice
#' @param B la deuxième matrice
#' @return retourne la distance en norme de Frobenius
#' @examples
#' frob_norm(matrix(c(1, 2, 2, 4), nrow = 2), t(matrix(c(1, 2, 2, 4), nrow = 2)))
#' frob_norm(matrix(c(1, 2, 2, 4), nrow = 2))
#' @export

frob_norm <- function(A, B = A * 0){

    # Dans le cas où deux matrice sont fournis, on vérifie si les deux ont les même dimensions
    if(any(dim(A) != dim(B))) stop(cout_error("Les matrices n'ont pas les mêmes dimensions"))

    # On calcul la distance en norme de Frobenius
    # Donnera la norme de A simplement avec B = 0 pour le cas où une seule matrice est fournie
    distance <- sqrt(sum((A - B)^ 2))
    return(distance)
}