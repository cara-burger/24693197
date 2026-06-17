keyword_plot <- function(df, keywords = c("chocolate", "aroma", "savory", "toned", "sweetly", "tart"), min_reviews = 20){

    plot_df <-df %>% group_by(origin) %>%
        summarise(keyword_pct = sum(grepl(paste(keywords, collapse = "|"), desc, ignore.case = TRUE)) / n(),
                  n_reviews = n()) %>%
        mutate(Term = glue::glue_collapse(keywords, sep = ", ", last = " and ")) %>%
        filter(n_reviews >= min_reviews) %>% arrange(desc(keyword_pct)) %>%
        head(10) %>% mutate(origin = reorder(origin, keyword_pct))

    g <- plot_df %>% ggplot(aes(x = origin, y = keyword_pct)) +
        geom_col(fill = "steelblue", alpha = 0.7) + coord_flip() +
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(20), subtitle.size = fmxdat::ggpts(16)) +
        labs(title = "Origins Best Matching Student Preferences",
             subtitle = glue::glue("% of reviews mentioning: {paste(keywords, collapse = ', ')}"),
             x = "", y = "% Reviews Matching Keywords",
             caption = "Source: Coffee Review Database")

    fmxdat::finplot(g, y.pct = TRUE, y.pct_acc = 1)
}