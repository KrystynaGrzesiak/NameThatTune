library(shiny)
library(spotifyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Define UI for application that draws a histogram
ui <- navbarPage("Name That Tune!",
                 tabPanel("Settings",
                          textInput("client_id",
                                    value = "2034823feb56418483aa481676131fbc",
                                    label = "Client ID:"),
                          textInput("secret",
                                    value = "9e80f7bd19444dc0956a4038f3f1dce9",
                                    label = "Secret:"),
                          textInput("user_id",
                                    value = "spotrodzina1",
                                    label = "User ID:"),
                 ),
                 tabPanel("Players and playlist",
                          column(3,
                                 checkboxGroupInput("players",
                                                    "Select players",
                                                    choices = c("Piotrek", "Wojtek",
                                                                "Jozek","Maciek",
                                                                "Krysia", "Gabor",
                                                                "Krzychu", "Miki"))
                          ),
                          verbatimTextOutput("choice"),
                 ),
                 tabPanel("Game",
                          h3("Currently playing:"),
                          br(),
                          column(2, htmlOutput("album")),
                          column(10, verbatimTextOutput("current_song")),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          h3("Scores:"),
                          column(3, tableOutput("print_scores")),
                          column(3,
                                 checkboxGroupInput("title_point",
                                                    "Point for the title goes to:",
                                                    choices = c(""))
                          ),
                          column(3,
                                 checkboxGroupInput("author_point",
                                                    "Point for the author goes to:",
                                                    choices = c(""))
                          ),
                          column(3,
                                 checkboxGroupInput("extra_point",
                                                    "Point for a fun fact goes to:",
                                                    choices = c(""))
                          ),
                          column(10),
                          column(2, actionButton("next_button", "Next song"))
                 ),
                 tabPanel("Summary",
                          h3("Results:"),
                          tableOutput("overall_results"),
                          downloadButton("download_scores"),
                          column(3,
                                 checkboxGroupInput("category",
                                                    "Category:",
                                                    choices = c("Author",
                                                                "Title",
                                                                "Fun fact"),
                                                    selected = c("Author",
                                                                 "Title",
                                                                 "Fun fact")),
                                 checkboxGroupInput("players_plot",
                                                    "Players:",
                                                    choices = c(""))),
                          column(9, plotOutput("results_vis"))
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    #Settings

    client_id <- NULL
    secret <- NULL
    user_id <- NULL

    observe({
        client_id <- reactiveVal(input[["client_id"]])
        secret <- reactiveVal(input[["secret"]])
        user_id <- reactiveVal(input[["user_id"]])

        if(!is.null(secret()) & !is.null(client_id()) & !is.null(user_id())) {
            Sys.setenv(SPOTIFY_CLIENT_ID = client_id())
            Sys.setenv(SPOTIFY_CLIENT_SECRET = secret())
            access_token <- get_spotify_access_token()

            playlist <<- reactiveVal(value = {
                song <- get_my_currently_playing()
                playlist_id <- str_remove(song$context$uri, "spotify:playlist:")
                playlist <- get_playlist(playlist_id)
                playlist
            })
        }
    })

    # Playlists and players


    output[["choice"]] <- renderText({

        players <- input[["players"]]

        if(is.null(players)) {
            players <- "Choose at least one player!"
        }

        text <- paste0("Players: \n\n", paste0("* ",
                                               players,
                                               "\n",
                                               collapse = ""))
        text
    })

    observe({
        updateCheckboxGroupInput(session, "author_point",
                                 choices = input[["players"]])

        updateCheckboxGroupInput(session, "title_point",
                                 choices = input[["players"]])

        updateCheckboxGroupInput(session, "extra_point",
                                 choices = input[["players"]])
    })


    #Game
    i <- reactiveVal(3)

    current_song <- reactive({
        song <- get_my_currently_playing()
        reactiveVal(song)
    })

    output[["current_song"]] <- renderText({

        song <- current_song()()
        aut <- paste0(song$item$artists$name, sep = ", ", collapse = "")
        author <- substr(aut, 1, nchar(aut) - 2)
        title <- song$item$name
        album <- song$item$album$name

        playlist_data <- playlist()

        text <- paste0("Author: ", author, "\n", "\n",
                       "Song: ", title, "\n", "\n",
                       "Album: ", album, "\n", "\n",
                       "Playlist: ", playlist_data$name, " by ",
                       playlist_data$owner$display_name, "\n", "\n",
                       i()/3, "/", playlist_data$tracks$total)
        text
    })


    output[["album"]] <- renderText({
        url <- current_song()()$item$album$images$url[1]
        c('<img src="',url,'" width="150">')
    })

    table_of_scores <- reactiveVal(data.frame())

    observeEvent(input[["next_button"]], {

        song <- current_song()()
        skip_my_playback()

        aut <- paste0(song$item$artists$name, sep = ", ", collapse = "")
        author <- substr(aut, 1, nchar(aut) - 2)
        title <- song$item$name
        id <- song$item$id

        score_title <- as.numeric(input[["players"]] %in% input[["title_point"]])
        score_author <- as.numeric(input[["players"]] %in% input[["author_point"]])
        score_extra <- as.numeric(input[["players"]] %in% input[["extra_point"]])

        df <- t(data.frame(score_title/sum(score_title),
                           score_author/sum(score_author),
                           score_extra/sum(score_extra)))
        colnames(df) <- input[["players"]]
        rownames(df) <- NULL

        small_scores <- cbind(data.frame(index = i()/3,
                                         playlist = playlist()$name,
                                         id = id,
                                         author = author,
                                         title = title,
                                         category = c("Title",
                                                      "Author",
                                                      "Fun fact")), df)

        small_scores[is.na(small_scores)] <- 0
        tmp_scores <- table_of_scores()
        tmp_scores <- rbind(tmp_scores, small_scores)
        table_of_scores(tmp_scores)
        new_i <- i() + 3
        i(new_i)

        updateCheckboxGroupInput(session, "author_point",
                                 choices = input[["players"]])

        updateCheckboxGroupInput(session, "title_point",
                                 choices = input[["players"]])

        updateCheckboxGroupInput(session, "extra_point",
                                 choices = input[["players"]])

        updateCheckboxGroupInput(session, "players_plot",
                                 choices = input[["players"]],
                                 selected = input[["players"]])

        current_song()(get_my_currently_playing())
        current_song()(get_my_currently_playing())
    })


    output[["print_scores"]] <- renderTable({

        scores <- table_of_scores()

        if(all(dim(scores) == 0)) {

            results <- data.frame(Player = input[["players"]],
                                  Score = 0)
        }else {
            points <- scores %>%
                select(input[["players"]]) %>%
                colSums() %>%
                round(., 2)
            results <- data.frame(Player = input[["players"]],
                                  Score = points) %>%
                arrange(desc(Score))
        }
        results
    })

    #Summary
    output[["overall_results"]] <- renderTable({
        scores <- table_of_scores()
        results <- scores %>%
            select(input[["players"]], category) %>%
            group_by(category) %>%
            summarise_all(sum)

        rbind(results, c("SUM:", colSums(results[, -1])))

    })

    output[["results_vis"]] <- renderPlot({

        scores <- table_of_scores()

        players <- input[["players_plot"]]
        cat <- input[["category"]]

        dat <- scores %>%
            filter(category %in% cat) %>%
            gather(player, points, players) %>%
            filter(player %in% players) %>%
            group_by(index, player) %>%
            mutate(points = sum(points)) %>%
            select(-category) %>%
            unique() %>%
            group_by(player) %>%
            arrange(index) %>%
            mutate(csum = cumsum(points))

        dat$index <- as.factor(dat$index)

        dat %>%
            ggplot() +
            geom_line(aes(x = index, y = csum, col = player, group = player)) +
            theme_minimal() +
            xlab("Song") +
            ylab("Cumulative points")
    })


    output[["download_scores"]] <- downloadHandler(filename = paste0(playlist$name,
                                                                     "_scores.csv"),
                                                   content = function(file) {
                                                       write.csv(table_of_scores(), file)
                                                   })


}

shinyApp(ui = ui, server = server)
