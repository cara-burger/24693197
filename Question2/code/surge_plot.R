surge_plot <- function(df, top_n = 10){

    plot_df <- find_name_surges(df, top_n = top_n) %>%
        mutate(label = paste0(Name, " (", Year, ")")) %>%
        mutate(label = reorder(label, Surge))

    g <- plot_df %>% ggplot(aes(x = label, y = Surge, fill = Gender)) +
        geom_col(alpha = 0.8) +
        coord_flip() +
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(20), subtitle.size = fmxdat::ggpts(16)) +
        labs(title = "Biggest Year-on-Year Naming Surges",
             x = "", y = "Increase in Babies Named",
             caption = "Source: US Social Security Administration baby name data")

    fmxdat::finplot(g)
}