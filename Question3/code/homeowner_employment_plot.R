homeowner_employment_plot <- function(df){

    plot_df <- df %>% filter(!is.na(Default_Flag), !is.na(home_ownership), !is.na(emp_length_num)) %>%
        filter(term == 36) %>%
        filter(home_ownership %in% c("OWN", "MORTGAGE", "RENT")) %>%
        mutate(emp_group = ifelse(emp_length_num >= 10, "10+ Years", "Under 10 Years")) %>%
        group_by(home_ownership, emp_group) %>%
        summarise(Default_Rate = mean(Default_Flag, na.rm = TRUE) * 100) %>%
        ungroup()

    g <- plot_df %>% ggplot(aes(x = home_ownership, y = Default_Rate, group = emp_group, color = emp_group)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 4) +
        geom_text(aes(label = glue::glue("{round(Default_Rate, 1)}%")), vjust = -0.8, size = 3) +
        scale_color_manual(values = c("10+ Years" = "darkgreen", "Under 10 Years" = "darkred")) +
        labs(title = "Homeownership and Employment Length vs Default Rate",
             subtitle = "Short-term (36 month) loans only", x = "Home Ownership Status", y = "Default Rate (%)",
             color = "Employment Length",  caption = "Source: Lending Club anonymised loan data") +
        ggplot2::theme_bw()
    g
}
