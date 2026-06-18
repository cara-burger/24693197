
plot_score_by_genre <- function(titles_clean) {

    # genres to look at
    focus_genres <- c("drama", "comedy", "thriller", "action", "romance",
                      "documentation", "crime", "horror", "animation", "scifi",
                      "history", "fantasy", "music", "sport")

    # for each genre, grab movies that contain it and stack into one long dataframe
    genre_long <- map_dfr(focus_genres, ~{
        titles_clean %>% filter(grepl(.x, genres), !is.na(imdb_score)) %>%
        mutate(genre = .x) %>% select(id, genre, imdb_score)
    })

    # median score per genre, drop genres with fewer than 50 titles
    genre_summary <- genre_long %>% group_by(genre) %>%
        summarise(median_score = median(imdb_score), n = n()) %>%
        ungroup() %>%
        mutate(genre = fct_reorder(genre, median_score))

   g <- genre_summary %>% ggplot(aes(x = genre, y = median_score, fill = genre)) +
        geom_col(show.legend = FALSE, alpha = 0.85) +
        coord_flip() +
        labs(title = "Median IMDB Score by Genre", x = "", y = "Median IMDB Score") +
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(16), subtitle.size = fmxdat::ggpts(12)) +
        theme(legend.position = "none")

   fmxdat::finplot(g)

   g

}