#' Name That Tune Application
#'
#' @description Opens the Name That Tune game.
#'
#' @importFrom shiny runApp
#'
#' @param client_id character string of length 32. Spotify `Client ID`. See
#'     details for more information.
#' @param client_secret character string of length 32. Spotify `Client Secret`.
#'     See details for more information.
#' @param port The TCP port. See \code{\link[shiny]{runApp}}.
#'
#' @details To get Client ID and Secret you need to have the account
#' on \href{https://developer.spotify.com/}{Spotify for Developers}.
#'
#' @section Warning : Any ad-blocking software may cause malfunctions.
#'
#' @export run_name_that_tune
#'


run_name_that_tune <- function(client_id,
                               client_secret,
                               port = getOption("shiny.port")) {

    Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
    Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)

    runApp(system.file("app", package = "NameThatTune"), port = port, launch.browser = TRUE)
}

