#' Displays a message as an error, coloring it in red framed by this emoji ❌
#'
#' @param message the message to display
#' @return the formatted message

cout_error <- function(message) {
    return(paste("\n❌\033[31m", message, "\033[0m❌"))
}

#' Displays a message as a success, coloring it in green framed by this emoji ✅
#'
#' @param message the message to display
#' @return the formatted message

cout_success <- function(message) {
    return(paste("\n✅\033[32m", message, "\033[0m✅"))
}

#' Displays a message as information, coloring it in yellow framed by this emoji ℹ️
#'
#' @param message the message to display
#' @return the formatted message

cout_info <- function(message) {
    return(paste("\nℹ️\033[33m", message, "\033[0m"))
}
