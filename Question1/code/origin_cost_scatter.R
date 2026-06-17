origin_cost_scatter <- function(df, min_reviews = 10){

    plot_df <- df %>% group_by(origin) %>%
        summarise(med_rating = median(Rating, na.rm = TRUE), med_cost = median(Cost_Per_100g, na.rm = TRUE),
                  n_reviews  = n()) %>% filter(n_reviews >= min_reviews)

    # Label the best-rated and cheapest origins
    best_df  <- plot_df %>% filter(med_rating == max(med_rating)) %>% slice(1)
    cheap_df <- plot_df %>% filter(med_cost == min(med_cost)) %>% slice(1)

    g <- plot_df %>%
        ggplot() +
        geom_point(aes(x = med_cost, y = med_rating, size = n_reviews), color = "steelblue", alpha = 0.6) +
        guides(size = FALSE) +
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(20), subtitle.size = fmxdat::ggpts(16)) +
        labs(title = "Coffee Origin: Rating vs Cost",
             subtitle = "Bubble size = no. reviews",
             x = "Median Cost per 100g (USD)", y = "Median Rating",
             caption = "Source: Coffee Review Database") +
        #Label best rated as green
        ggrepel::geom_label_repel(data = best_df,
                                  aes(med_cost, med_rating, label = glue::glue("{origin}:\n{round(med_rating, 1)} pts")),
                                  size = 4, alpha = 0.35, fill = "green") +
        #Label cheapest as red
        ggrepel::geom_label_repel(data = cheap_df,
                                  aes(med_cost, med_rating, label = glue::glue("{origin}:\n${round(med_cost, 2)}")),
                                  size = 4, alpha = 0.35, fill = "red")

    fmxdat::finplot(g, x.comma.sep = TRUE)
}