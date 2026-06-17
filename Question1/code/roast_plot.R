roast_plot <- function(df){

    plot_df <- df %>%
        filter(!is.na(roast), roast != "") %>%
        mutate(roast = factor(roast, levels = c("Light", "Medium-Light",
                                                "Medium", "Medium-Dark", "Dark")))

    g <- plot_df %>%
        ggplot(aes(x = roast, y = Rating, fill = roast)) +
        geom_boxplot(alpha = 0.5, outlier.shape = NA) +
        geom_jitter(aes(color = roast), alpha = 0.3, width = 0.2, size = 0.8) +
        fmxdat::theme_fmx(title.size    = fmxdat::ggpts(20),
                          subtitle.size = fmxdat::ggpts(16)) +
        guides(fill = FALSE, color = FALSE) +
        labs(title = "Coffee Rating Distribution by Roast Strength",
             subtitle = "Boxplot with individual reviews shown as points",
             x = "Roast Strength",
             y = "Rating",
             caption  = "Source: Coffee Review Database")

    fmxdat::finplot(g)
}