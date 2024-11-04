#' Cette fonction calcul le vecteur coefficient d'une regression linéaire multiple par la méthode des moindres carrés
#' @param Y le vecteur de la variable à prédire
#' @param X la matrice des variables explicatives
#' @return un vecteur de coefficient de regression
#' @examples 
#'
#' @export

coef_lm <- function(Y, X){
    # On vérifie si les deux variables entrée
    if(!is.vector(Y) || !is.matrix(X)) stop(cout_error("Y doit être un vecteur et X une matrice numérique"))
    if(nrow(X) != length(Y)) stop(cout_error("Y et X doivent avoir la même taille"))

    # retourne les coefficient en applicant la formule
    coefs <- solve(t(X) %*% X) %*% t(X) %*% Y
    return(as.vector(coefs))
}