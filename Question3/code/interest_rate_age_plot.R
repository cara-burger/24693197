interest_rate_age_plot <- function(df){
    plot_df <- df %>%
        filter(!is.na(int_rate), !is.na(grade), !is.na(earliest_cr_line)) %>%
        mutate(cr_year = as.numeric(gsub(".*-", "", earliest_cr_line)),
               cr_history = 2018 - cr_year,
               cr_group = ifelse(cr_history < 10, "Shorter History", "Longer History")) %>%
        filter(grade %in% c("A","B","C","D","E","F","G")) %>%
        group_by(grade, cr_group) %>%
        summarise(Median_Rate = median(int_rate, na.rm = TRUE)) %>%
        ungroup()

    g <- plot_df %>% ggplot(aes(x = grade, y = Median_Rate, fill = grade)) +
        geom_col(width = 0.65, show.legend = FALSE) +
        geom_text(aes(label = glue::glue("{round(Median_Rate, 1)}%")), vjust = -0.4, size = 2.8) +
        facet_wrap(~cr_group) +
        scale_fill_manual(values = c("A" = "darkgreen", "B" = "steelblue", "C" = "goldenrod",
                                     "D" = "orange", "E" = "darkorange", "F" = "red", "G" = "darkred")) +
        labs(title = "Median Interest Rate by Credit Grade",
             subtitle = "Credit history length used as age proxy",
             x = "Credit Grade", y = "Median Interest Rate (%)",
             caption = "Shorter history = likely younger borrowers") +
        ggplot2::theme_bw()
    g
}