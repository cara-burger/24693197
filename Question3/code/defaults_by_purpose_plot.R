defaults_by_purpose_plot <- function(df){

    plot_df <- df %>% filter(!is.na(Default_Flag), !is.na(purpose)) %>%
        group_by(purpose) %>% summarise(Default_Rate = mean(Default_Flag, na.rm = TRUE) * 100) %>%
        ungroup() %>%

        mutate(purpose = forcats::fct_reorder(purpose, Default_Rate))
    g <- plot_df %>% ggplot(aes(x = purpose, y = Default_Rate)) +
        geom_col(fill = "steelblue", width = 0.7) +
        geom_text(aes(label = glue::glue("{round(Default_Rate, 1)}%")), vjust = -0.4, size = 3) +
        labs(title = "Default Rate by Loan Purpose",
             subtitle = "Small business and high-risk purposes carry elevated default rates",
             x = "Loan Purpose", y = "Default Rate (%)",  caption = "Source: Lending Club anonymised loan data") +
        ggplot2::theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    g
}