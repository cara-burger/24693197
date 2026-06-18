defaults_by_grade_plot <- function(df){
    plot_df <- df %>% filter(!is.na(Default_Flag), !is.na(grade)) %>%
        group_by(grade) %>% summarise(Default_Rate = mean(Default_Flag, na.rm = TRUE) * 100) %>%
        ungroup()
    g <- plot_df %>% ggplot(aes(x = grade, y = Default_Rate, fill = grade)) +
        geom_col(width = 0.7, show.legend = FALSE) +
        geom_text(aes(label = glue::glue("{round(Default_Rate, 1)}%")), vjust = -0.4, size = 3) +
        scale_fill_manual(values = c("A" = "darkgreen", "B" = "steelblue", "C" = "goldenrod",
                                     "D" = "orange", "E" = "darkorange", "F" = "red", "G" = "darkred")) +
        labs(title = "Default Rate by Credit Grade", x = "Credit Grade", y = "Default Rate (%)",
             caption = "Source: Lending Club anonymised loan data") +
        ggplot2::theme_bw()
    g
}