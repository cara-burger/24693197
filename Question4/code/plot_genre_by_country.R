plot_genre_by_country <- function(titles_clean,  n_countries = 10) {

    focus_genres <- c("drama", "comedy", "thriller", "action", "romance",
                      "documentation", "crime", "horror", "animation", "scifi")

    top_countries <- titles_clean %>%
        mutate(primary_country = gsub("^\\s+", "", gsub(",.*", "", production_countries))) %>%
        filter(primary_country != "") %>%
        count(primary_country, sort = TRUE) %>%
        head(n_countries) %>% pull(primary_country)

    # genre membership per title using grepl, stack into long format
    genre_long <- map_dfr(focus_genres, ~{
        titles_clean %>%
            mutate(primary_country = gsub("^\\s+", "", gsub(",.*", "", production_countries))) %>%
            filter(primary_country %in% top_countries) %>%
            filter(grepl(.x, genres)) %>%
            mutate(genre = .x) %>%
            select(id, primary_country, genre)
    })

    g <- genre_long %>%
        count(primary_country, genre) %>%
        mutate(genre = fct_reorder(genre, n)) %>%
        ggplot(aes(x = genre, y = n, fill = genre)) +
        geom_col(show.legend = FALSE, alpha = 0.85) +
        coord_flip() +
        facet_wrap(~ primary_country, scales = "free_x", ncol = 5) +
        labs(title = "Genre Counts by Country",
             x = "", y = "Number of Titles") +
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(18), subtitle.size = fmxdat::ggpts(12), strip.size = fmxdat::ggpts(12))

    fmxdat::finplot(g)
}