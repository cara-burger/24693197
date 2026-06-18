plot_star_power <- function(titles_clean, actors_clean, n_stars = 50) {

    # input checks
    if (!"tmdb_popularity" %in% names(titles_clean)) stop("tmdb_popularity column not found.")
    if (!"id" %in% names(actors_clean)) stop("id column not found in actors_clean.")

    # identify top N stars by total imdb_votes across their movies
    top_stars <- actors_clean %>%
        inner_join(titles_clean %>% select(id, imdb_votes), by = "id") %>%
        group_by(name) %>%
        summarise(total_votes = sum(imdb_votes, na.rm = TRUE)) %>%
        arrange(desc(total_votes)) %>% head(n_stars) %>% pull(name)

    # movies featuring at least one top star
    star_movies <- actors_clean %>%
        filter(name %in% top_stars) %>% pull(id) %>%
        unique()

    # label each movie as star or non-star
    plot_data <- titles_clean %>%
        filter(!is.na(tmdb_popularity)) %>%
        mutate(star_power = ifelse(id %in% star_movies, "Star Cast", "No Star Cast"))


    g <- plot_data %>%
        ggplot(aes(x = star_power, y = tmdb_popularity, fill = star_power)) +
        geom_boxplot(alpha = 0.6, show.legend = FALSE) +
        coord_flip() +
        labs(title = "Does Star Power Drive Audience Engagement?", subtitle = "TMDb popularity for movies with and without top 50 actors by IMDb votes",
            caption = "Source: IMDb / TMDb via Netflix catalogue",
            x = "", y = "TMDb Popularity (log scale)") +
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(18), subtitle.size = fmxdat::ggpts(12))

    fmxdat::finplot(g, log.y = TRUE, title.centre = TRUE)
}