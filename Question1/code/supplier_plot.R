supplier_plot <- function(df, min_reviews = 5, max_cost = 20){

    plot_df <- df %>% group_by(roaster) %>%
        summarise(med_rating = median(Rating, na.rm = TRUE),
                  med_cost   = median(Cost_Per_100g, na.rm = TRUE),
                  n_reviews  = n()) %>% ungroup() %>%
        filter(n_reviews >= min_reviews, med_cost <= max_cost) %>%
        arrange(desc(med_rating)) %>%
        head(10) %>% mutate(roaster = reorder(roaster, med_rating))

    g <- plot_df %>%
        ggplot(aes(x = roaster, y = med_rating)) +
        geom_point(size = 4, color = "steelblue") +
        coord_flip() +
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(20), subtitle.size = fmxdat::ggpts(16)) +
        labs(title = "Top 10 Recommended Suppliers",
             subtitle = glue::glue("Minimum {min_reviews} reviews, max ${max_cost} per 100g"),
             x = "", y = "Median Rating",
             caption = "Source: Coffee Review Database")

    fmxdat::finplot(g)
}