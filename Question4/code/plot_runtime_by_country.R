
plot_runtime_by_country <- function(titles_clean, n_countries = 10) {

    # input checks
    if (!"runtime" %in% names(titles_clean)) stop("runtime column not found.")
    if (!"production_countries" %in% names(titles_clean)) stop("production_countries column not found.")

    top_countries <- titles_clean %>%
        mutate(country = gsub("^\\s+", "", gsub(",.*", "", production_countries))) %>%
        filter(country != "") %>% count(country, sort = TRUE) %>%
        head(n_countries) %>% pull(country)

    # filter to those countries, drop missing runtimes
    plot_data <- titles_clean %>%
        mutate(country = gsub("^\\s+", "", gsub(",.*", "", production_countries))) %>%
        filter(country %in% top_countries) %>%
        filter(!is.na(runtime))

    g <- plot_data %>%
        ggplot(aes(x = fct_reorder(country, runtime, median), y = runtime, fill = country)) +
        geom_boxplot(alpha = 0.6, show.legend = FALSE) +
        geom_jitter(alpha = 0.08, width = 0.2, size = 0.5, show.legend = FALSE) +
        coord_flip() +
        labs(title = "Movie Runtime by Production Country",
            subtitle = "Distribution of movie length across top production countries",
            caption = "Source: IMDb via Netflix catalogue",
            x = "", y = "Runtime (minutes)") +
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(18), subtitle.size = fmxdat::ggpts(12))

    fmxdat::finplot(g)
}