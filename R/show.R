#' Affiche un message sous forme d'erreur, en coloriant en rouge encadré par cet émojis ❌
#' 
#' @param message le message à afficher
#' @return le message formater
#' @examples
#' cout_error("Une erreur s'est produite")
cout_error <- function(message){
    paste("\n❌\033[31m ", " \033[0m❌", sep = message)
}

#' Affiche un message sous forme de succès, en coloriant en rouge encadré par cet émojis ✅
#'
#' @param message le message à afficher
#' @return le message formater
#' @examples
#' cout_success("Opération réussie")
cout_success <- function(message){
    paste("\n✅\033[32m ", " \033[0m✅", sep = message)
}

#' Affiche un message sous forme d'info, en coloriant en rouge encadré par cet émojis ℹ️
#' @param message le message à afficher
#' @return le message formater
#' @examples
#' cout_info("Une information importante")
cout_info <- function(message){
    paste("\nℹ️\033[33m  ", " \033[0m", sep = message)
}