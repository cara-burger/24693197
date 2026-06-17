persistence_plot <- function(df){
    g <- df %>% ggplot(aes(x = Year, y = Corr, color = Gender)) +
        geom_line(size = 1) +
        geom_vline(xintercept = 1990, linetype = "dashed", color = "grey40") +
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(20), subtitle.size = fmxdat::ggpts(16)) +
        labs(title = "Naming Trend Persistence Over Time",
             subtitle = "Spearman correlation between each year's top 25 names and the same top 25, 3 years later",
             x = "", y = "Rank Correlation", caption = "Dashed line represents 1990. Source: US Social Security Administration baby name data.")

    fmxdat::finplot(g)
}