if (interactive()) {
    # testing url
    options(shiny.port = 1410)
    APP_URL <- "http://localhost:1410/"
} else {
    # deployed URL
    APP_URL <-  "https://krystynagrzesiak.shinyapps.io/NTT-test/"
}

app <- oauth_app("NameThatTune!",
                 key = Sys.getenv("SPOTIFY_CLIENT_ID"),
                 secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                 redirect_uri = APP_URL
)

api <- oauth_endpoint(authorize = "https://accounts.spotify.com/authorize",
                      access = "https://accounts.spotify.com/api/token")

get_spotify_authorization_code()

uri_p <- get_my_playlists(limit = 20)$id

tracks <- lapply(uri_p, function(id) {
    get_playlist_tracks(id)
}) %>% bind_rows()


playlist_tracks <- tracks

albums <- playlist_tracks$track.artists

albums_data <- lapply(1:length(albums), function(i) {
    author <- paste0(albums[[i]]$name, collapse = ", ")
    img <- playlist_tracks$track.album.images[[i]][["url"]][1]
    if(is.null(img)) {
        img <- ""
    }
    data.frame(author = author,
               img = img)
}) %>% bind_rows()


data_tracks <- albums_data %>%
    mutate(title = playlist_tracks$track.name,
           album = playlist_tracks$track.album.name,
           uris = playlist_tracks$track.uri)


saveRDS(data_tracks, "tracks_djtm.RDS")

