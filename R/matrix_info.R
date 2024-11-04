#' This function returns a list containing several pieces of information from a matrix
#' Notably: its rank, its eigenvalues, its trace, and whether it is idempotent or not
#' @param mat the input matrix
#' @return a list containing the information of the matrix
#' @examples
#' matrix_info(matrix(c(1, 2, 2, 4), nrow = 2))
#' matrix_info(matrix(1:6, nrow = 2)) # Non-square matrix
#' matrix_info(diag(1, 3)) # Idempotent matrix
#' @export

matrix_info <- function(mat){
    # Determine the rank
    rang <- qr(mat)$rank

    # Determine if the matrix is idempotent

    # Initialize trace and symmetric variables to determine if the matrix is square
    trace <- NULL
    symetrique <- FALSE
    if(nrow(mat) == ncol(mat)){
        trace <- sum(diag(mat))
        symetrique <- all(mat == t(mat))

        decomposition_spectral <- eigen(mat)
        valeur_propres <- decomposition_spectral$values
        vecteur_propres <- decomposition_spectral$vectors
        idem <- is_idempotent(mat)
    } else {
        valeur_propres <- NULL
        vecteur_propres <- NULL

        idem <- FALSE
    }
    
    # Return all the information of the matrix
    return(list(rang = rang,
                idem = idem,
                trace = trace,
                valeur_propres = valeur_propres,
                vecteur_propres = vecteur_propres,
                symetrique = symetrique
                ))
}
