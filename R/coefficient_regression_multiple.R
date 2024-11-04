#' This function calculates the coefficient vector of a multiple linear regression using the least squares method
#' @param Y the vector of the variable to predict
#' @param X the matrix of explanatory variables
#' @return a vector of regression coefficients
#' @examples 
#' Y <- c(2, 3, 5, 7, 11)  # Variable to predict
#' X <- matrix(c(1, 1, 1, 1, 1,   # Constant for the intercept
#'                1, 2, 3, 4, 5), # Explanatory variable
#'              nrow = 5,
#'              byrow = FALSE)
#' coefficients <- coef_lm(Y, X)
#' @export

coef_lm <- function(Y, X){
    # Check if both input variables are correct
    if(!is.vector(Y) || !is.matrix(X)) stop(cout_error("Y must be a vector and X a numeric matrix"))
    if(nrow(X) != length(Y)) stop(cout_error("Y and X must have the same size"))

    # Return the coefficients by applying the formula
    coefs <- solve(t(X) %*% X) %*% t(X) %*% Y
    return(as.vector(coefs))
}
