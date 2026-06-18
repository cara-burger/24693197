interest_rate_plot <- function(df){
    plot_df <- df %>%
        filter(!is.na(int_rate), !is.na(grade)) %>%
        filter(grade %in% c("A","B","C","D","E","F","G")) %>%
        group_by(grade) %>%
        summarise(Median_Rate = median(int_rate, na.rm = TRUE)) %>%
        ungroup()
    g <- plot_df %>% ggplot(aes(x = grade, y = Median_Rate, fill = grade)) +
        geom_col(width = 0.7, show.legend = FALSE) +
        geom_text(aes(label = glue::glue("{round(Median_Rate, 1)}%")), vjust = -0.4, size = 3) +
        scale_fill_manual(values = c("A" = "darkgreen", "B" = "steelblue", "C" = "goldenrod",
                                     "D" = "orange", "E" = "darkorange", "F" = "red",
                                     "G" = "darkred")) +
        labs(title = "Median Interest Rate by Credit Grade",
             x = "Credit Grade", y = "Median Interest Rate (%)") +
        ggplot2::theme_bw()
    g
}