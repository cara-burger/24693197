plot_popularity_by_country <- function(titles_clean, n_countries = 10) {

    # stops the function early with a clear message if something is wrong
    if (!"tmdb_popularity" %in% names(titles_clean)) stop("tmdb_popularity column not found.")
    if (!"production_countries" %in% names(titles_clean)) stop("production_countries column not found.")

    # find the top N countries by movie count
    # note: using primary (first-listed) country to assign each movie to one group
    top_countries <- titles_clean %>% mutate(country = gsub("^\\s+", "", gsub(",.*", "", production_countries))) %>%
        filter(country != "") %>% count(country, sort = TRUE) %>%
        head(n_countries) %>% pull(country)

    # filter to only those countries, drop missing popularity scores
    plot_data <- titles_clean %>%
        mutate(country = gsub("^\\s+", "", gsub(",.*", "", production_countries))) %>%
        filter(country %in% top_countries) %>%
        filter(!is.na(tmdb_popularity))


    g <- plot_data %>% ggplot(aes(x = fct_reorder(country, tmdb_popularity, median), y = tmdb_popularity, fill = country)) +
        geom_boxplot(alpha = 0.6, show.legend = FALSE) +
        geom_jitter(alpha = 0.08, width = 0.2, size = 0.5, show.legend = FALSE) +
        coord_flip() +
        labs(title = "TMDb Popularity by Production Country", subtitle = "Distribution of audience engagement across top production countries",
            caption = "Source: TMDb via Netflix catalogue", x = "", y = "TMDb Popularity (log scale)") +
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(18), subtitle.size = fmxdat::ggpts(12))

    # finplot handles the log scale and title centering
    fmxdat::finplot(g, log.y = TRUE)
}