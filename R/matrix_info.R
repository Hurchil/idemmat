#' Cette fonction retourne une liste qui contient plusieurs information à partir d'une matrice
#' Notamment: son rang, ses valeurs propres, sa trace, si c'est idempotent ou pas
#' @param mat la matrice d'entrée
#' @return une liste contenant les informations de la matrices
#' @examples
#' idem(matrix(c(1, 2, 2, 4), nrow = 2))
#' matrix_info(matrix(c(1, 2, 2, 4), nrow = 2))
#' matrix_info(matrix(1:6, nrow = 2)) # Matrice non carrée
#' matrix_info(diag(1, 3)) # Matrice idempotente
#' @export

matrix_info <- function(mat){
    # Déterminer le rang
    rang <- qr(mat)$rank

    # Determiner la décomposition spectrale (vecteurs et valeurs propres)
    decomposition_spectral <- eigen(mat)
    valeur_propres <- decomposition_spectral$values
    vecteur_propres <- decomposition_spectral$vectors
    
    # Déterminer si la matrice est idempotente
    idem <- is_idempotent(mat)

    # Initialiser les variables trace et symetrique pour les determiner si la matrice est carré
    trace <- NULL
    symetrique <- FALSE
    if(nrow(mat) == ncol(mat)){
        trace <- sum(diag(mat))
        symetrique <- all(mat == t(mat))
    }

    # Retourne toutes les informations de la matrice
    return(list(rang = rang,
                idem = idem,
                trace = trace,
                valeur_propres = valeur_propres,
                vecteur_propres = vecteur_propres,
                symetrique = symetrique
                ))
}