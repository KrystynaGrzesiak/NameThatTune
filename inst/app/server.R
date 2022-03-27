library(shiny)
library(spotifyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(shinycssloaders)
library(shinyjs)

server <- function(input, output, session) {

    ###### SETTINGS ############################################################

    token <- get_spotify_authorization_code()

    ###### DEVICES #############################################################

    observe({
        devices <- get_my_devices(authorization = token)

        active <- devices[["name"]][devices[["is_active"]] == TRUE]

        if(length(active) == 0) {
            shinyjs::disable(selector = '.navbar-nav a[data-value="Players"')
            shinyjs::disable(selector = '.navbar-nav a[data-value="Game"')
            shinyjs::disable(selector = '.navbar-nav a[data-value="Playlist"')
            shinyjs::disable(selector = '.navbar-nav a[data-value="Summary"')
            showNotification(id = "device",
                             "ERROR! There are no available active devices!!!
                                 Please start Spotify session on the device you
                                 want to play music on and refresh.",
                             type = "error",
                             closeButton = FALSE,
                             duration = NULL)
        } else {
            updateSelectInput(session,
                              "devices",
                              choices = devices[["name"]],
                              selected = devices[["name"]][1])
            removeNotification("device")
        }
    })

    observeEvent(input[["refresh"]], {
        devices <- get_my_devices(authorization = token)

        active <- devices[["name"]][devices[["is_active"]] == TRUE]

        if(length(active) != 0) {
            shinyjs::enable(selector = '.navbar-nav a[data-value="Players"')
            shinyjs::enable(selector = '.navbar-nav a[data-value="Game"')
            shinyjs::enable(selector = '.navbar-nav a[data-value="Playlist"')
            shinyjs::enable(selector = '.navbar-nav a[data-value="Summary"')

            updateSelectInput(session,
                              "devices",
                              choices = devices[["name"]],
                              selected = devices[["name"]][1])
            removeNotification("device")
        }
    })


    ###### PLAYERS #############################################################

    players <- reactiveValues()

    observeEvent(input[["n_players"]], {
        if(is.null(input[["n_players"]])) {
            number_of_text_inputs <- 1
        } else {
            number_of_text_inputs <- input[["n_players"]]
        }
        players[["n_texts"]] <- number_of_text_inputs
    })


    output[["text_windows"]] <- renderUI({
        lapply(1L:players[["n_texts"]], function(ith_slider) {
            textInput(inputId = paste0("input_text", ith_slider),
                      label = paste0("Player", ith_slider),
                      value = paste0("Player", ith_slider))
        })
    })


    output[["choice"]] <- renderText({
        players[["chosen_players"]] <- sapply(1:players[["n_texts"]],
                                              function(ith_player) {
                                                  input[[paste0("input_text",
                                                                ith_player)]]
                                              })

        text <- paste0("Players: \n\n", paste0("* ",
                                               players[["chosen_players"]],
                                               "\n",
                                               collapse = ""))
        text
    })

    observeEvent(players[["chosen_players"]], {

        if(!any(sapply(players[["chosen_players"]], is.null))) {
            updateCheckboxGroupInput(session, "author_point",
                                     choices = players[["chosen_players"]])

            updateCheckboxGroupInput(session, "title_point",
                                     choices = players[["chosen_players"]])

            updateCheckboxGroupInput(session, "extra_point",
                                     choices = players[["chosen_players"]])

            updateCheckboxGroupInput(session, "players_plot",
                                     choices = players[["chosen_players"]],
                                     selected = players[["chosen_players"]])
        }
    })


    ###### PLAYLIST ############################################################

    playlist <- reactiveValues()
    table_of_scores <- reactiveValues()
    song_index <- reactiveValues()

    observe({
        if(is.null(playlist[["all_names"]])) {
            playlist_info <- get_my_playlists(limit = 30,
                                              authorization = token)
            names <- playlist_info[["name"]]

            playlist[["all_names"]] <- playlist_info[["name"]]
            playlist[["all_uris"]] <- playlist_info[["uri"]]
            playlist[["all_ids"]] <- playlist_info[["id"]]

            updateSelectInput(session,
                              inputId = "playlists",
                              choices = playlist[["all_names"]],
                              selected = playlist[["all_names"]][1])
        }
    })

    output[["playlist_content"]] <- renderTable({

        playlist_id <- playlist[["all_ids"]][playlist[["all_names"]] == input[["playlists"]]]
        playlist_info <- get_playlist(playlist_id)
        playlist_tracks <- get_playlist_tracks(playlist_id)
        albums <- playlist_tracks$track.album.artists

        albums_data <- lapply(1:length(albums), function(i) {
            author <- paste0(albums[[i]]$name, collapse = ", ")
            img <- playlist_tracks$track.album.images[[i]][["url"]][1]

            if(is.null(img)) {
                img <- ""
            }
            data.frame(author = author,
                       img = img)
        }) %>% bind_rows()

        playlist[["uri"]] <- playlist[["all_uris"]][playlist[["all_names"]] == input[["playlists"]]]
        playlist[["image"]] <- get_playlist_cover_image(playlist_id, authorization = token)$url[1]
        playlist[["owner"]] <- playlist_info$owner$display_name

        tracks_names <- playlist_tracks$track.name
        playlist[["n_tracks"]] <- length(tracks_names)
        playlist[["tracks"]] <- data.frame(author = albums_data[["author"]],
                                           img = albums_data[["img"]],
                                           title =  tracks_names,
                                           album = playlist_tracks$track.album.name,
                                           uris = playlist_tracks$track.uri) %>%
            slice(sample(1:n())) %>%
            mutate(id = 1:n())

        playlist[["tracks"]] %>%
            select(-uris, -img, -id)
    })

    output[["playlist_image"]] <- renderText({
        url <- playlist[["image"]]
        c('<img src="', url, '" width="200">')
    })


    observeEvent(input[["start"]], {

        tracks_table <- playlist[["tracks"]]

        toggle_my_shuffle(state = FALSE,
                          authorization = token)
        set_my_repeat_mode(state = "context",
                           authorization = token)
        updateTabsetPanel(session = session,
                          inputId = "general_tab",
                          selected = "Game")

        n_players <- length(players[["chosen_players"]])

        grid <- expand.grid(category = c("author", "title", "fun fact"),
                            player = players[["chosen_players"]])

        table_of_scores[["table"]] <- playlist[["tracks"]] %>%
            slice(rep(1:n(), each = n_players * 3)) %>%
            group_by(uris) %>%
            mutate(category = grid[["category"]],
                   player = grid[["player"]],
                   score = 0)

        start_my_playback(uris = tracks_table[["uris"]],
                          authorization = token)

        song_index[["i"]] <- 1
    })

    ###### GAME ################################################################


    output[["album"]] <- renderText({

        song <- playlist[["tracks"]] %>%
            filter(id == song_index[["i"]])
        if(song_index[["i"]] > playlist[["n_tracks"]]) {
            " "
        } else {
            url <- song$img
            c('<img src="', url, '" width="200">')
        }
    })


    output[["current_song"]] <- renderText({

        song <- playlist[["tracks"]] %>%
            filter(id == song_index[["i"]])

        author <- paste0(song$author, sep = ", ", collapse = "")
        title <- song$title
        album <- song$album

        text <- paste0("Author: ", author, "\n", "\n",
                       "Song: ", title, "\n", "\n",
                       "Album: ", album, "\n", "\n",
                       "Playlist: ", input[["playlists"]], " by ",
                       playlist[["owner"]], "\n", "\n",
                       song_index[["i"]], "/", playlist[["n_tracks"]])

        if(song_index[["i"]] > playlist[["n_tracks"]])
            text <- "The end. Go check your results in Summary."

        text
    })


    # pause / start song
    observeEvent(input[["pause_button"]], {
        tryCatch(pause_my_playback(authorization = token),
                 warning = function(cond) {
                     return(NULL)
                 },
                 error = function(cond) {
                     return(NULL)
                 })
    })

    observeEvent(input[["play_button"]], {
        tryCatch(start_my_playback(authorization = token),
                 warning = function(cond) {
                     return(NULL)
                 },
                 error = function(cond) {
                     return(NULL)
                 })
    })


    # scores
    output[["print_scores"]] <- renderTable({
        table_of_scores[["table"]] %>%
            group_by(player) %>%
            summarise(score = round(sum(score), 2)) %>%
            arrange(desc(score))
    })


    # next song
    observeEvent(input[["next_button"]], {

        i <- song_index[["i"]]

        score_title <- as.numeric(players[["chosen_players"]] %in% input[["title_point"]])
        score_author <- as.numeric(players[["chosen_players"]] %in% input[["author_point"]])
        score_extra <- as.numeric(players[["chosen_players"]] %in% input[["extra_point"]])

        tmp_scores <- expand_grid(category = c("author", "title", "fun fact"),
                                  player = players[["chosen_players"]]) %>%
            cbind(score = c(score_author, score_title, score_extra)) %>%
            group_by(category) %>%
            mutate(score = score/sum(score))

        tmp_scores[is.na(tmp_scores)] <- 0

        updateCheckboxGroupInput(session, "author_point",
                                 choices = players[["chosen_players"]])
        updateCheckboxGroupInput(session, "title_point",
                                 choices = players[["chosen_players"]])
        updateCheckboxGroupInput(session, "extra_point",
                                 choices = players[["chosen_players"]])

        if(i <= playlist[["n_tracks"]]) {

            tbl <- table_of_scores[["table"]]
            tbl[tbl[["id"]] == i, c("category", "player", "score")] <- tmp_scores
            table_of_scores[["table"]] <- tbl

            skip_my_playback(authorization = token)
        }
        song_index[["i"]] <- i + 1
    })


    # previous song
    observeEvent(input[["prev_button"]], {

        i <- song_index[["i"]]

        if(i > 1) {
            i <- i - 1
            song_index[["i"]] <- i
            skip_my_playback_previous(authorization = token)

            prev_scores <- table_of_scores[["table"]] %>%
                filter(id == i)

            Title <- prev_scores %>%
                filter(category == "title") %>%
                mutate(score = as.numeric(score > 0)) %>%
                pull(score)
            Author <- prev_scores %>%
                filter(category == "author") %>%
                mutate(score = as.numeric(score > 0)) %>%
                pull(score)
            Fun_fact <- prev_scores %>%
                filter(category == "fun fact") %>%
                mutate(score = as.numeric(score > 0)) %>%
                pull(score)

            prev_scores[prev_scores[["id"]] == i, "score"] <- 0
            tbl <- table_of_scores[["table"]]
            tbl[tbl[["id"]] == i, ] <- prev_scores
            table_of_scores[["table"]] <- tbl

            updateCheckboxGroupInput(session, "author_point",
                                     choices = players[["chosen_players"]],
                                     selected = players[["chosen_players"]][as.logical(Author)])
            updateCheckboxGroupInput(session, "title_point",
                                     choices = players[["chosen_players"]],
                                     selected = players[["chosen_players"]][as.logical(Title)])
            updateCheckboxGroupInput(session, "extra_point",
                                     choices = players[["chosen_players"]],
                                     selected = players[["chosen_players"]][as.logical(Fun_fact)])
        }
    })


    ###### SUMMARY #############################################################

    output[["overall_results"]] <- renderTable({
        scores <- table_of_scores[["table"]]
        results <- scores %>%
            select(id, category, player, score) %>%
            group_by(player, category) %>%
            summarise(points = sum(score)) %>%
            spread(player, points) %>%
            mutate_if(is.numeric, round, digits = 2)

        csums <- cbind(category = "SUM", t(colSums(results[, -1])))
        results <- rbind(results, csums) %>%
            mutate_if(is.character, as.numeric)

        results %>%
            cbind(SUM = rowSums(results[ , -1]))
    })

    output[["results_vis"]] <- renderPlot({

        scores <- table_of_scores[["table"]]
        players <- input[["players_plot"]]
        cat <- input[["category"]]

        dat <- scores %>%
            filter(category %in% cat,
                   player %in% players) %>%
            group_by(id, player) %>%
            mutate(points = sum(score)) %>%
            select(-category) %>%
            unique() %>%
            group_by(player) %>%
            arrange(id) %>%
            mutate(csum = cumsum(score)) %>%
            filter(id <= song_index[["i"]])

        dat[["id"]] <- as.factor(dat[["id"]])

        dat %>%
            ggplot() +
            geom_line(aes(x = id, y = csum, col = player, group = player)) +
            theme_minimal() +
            xlab("Song") +
            ylab("Cumulative points")
    })

    output[["download_scores"]] <-
        downloadHandler(filename = "scores.csv",
                        content = function(file) {
                            write.csv(table_of_scores[["table"]], file)
                        })

}

