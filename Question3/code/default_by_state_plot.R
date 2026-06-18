default_by_state_plot <- function(df){
    plot_df <- df %>% filter(!is.na(Default_Flag), !is.na(addr_state)) %>%
        group_by(addr_state) %>%
        summarise(Default_Rate = mean(Default_Flag, na.rm = TRUE) * 100) %>%
        ungroup()

    plot_df <- plot_orderset(df = plot_df, Column = "addr_state", Order  = plot_df %>% arrange(Default_Rate) %>% pull(addr_state) %>% as.character())

    g <- plot_df %>% ggplot(aes(x = addr_state, y = Default_Rate)) +
        geom_col(fill = "steelblue", width = 0.7) +
        labs(title = "Default Rate by US State",
             subtitle = "States ordered from lowest to highest default rate",
             x = "US State", y = "Default Rate (%)",
             caption = "Source: Lending Club anonymised loan data") +
        ggplot2::theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
    g
}