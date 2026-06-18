plot_rising_names <- function(df, gender_input = "F", from_year = 2005, to_year = 2014, top_n = 10){

    # find the names with the biggest average surge in this window
    top_names <- find_name_surges(df, return_all = TRUE) %>%
        filter(Gender == gender_input, Year >= from_year, Year <= to_year) %>%
        filter(!is.na(Surge)) %>%
        group_by(Name) %>%
        summarise(avg_surge = mean(Surge)) %>%
        arrange(desc(avg_surge)) %>%
        head(top_n) %>%
        pull(Name)

    # get each of those names' actual yearly counts to plot as lines
    plot_df <- df %>%
        filter(Name %in% top_names, Gender == gender_input, Year >= from_year, Year <= to_year) %>%
        group_by(Name, Year) %>%
        summarise(Total = sum(Count)) %>%
        ungroup()

    g <- plot_df %>%
        ggplot(aes(x = Year, y = Total, color = Name)) +
        geom_line(size = 1) +
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(20), subtitle.size = fmxdat::ggpts(16)) +
        labs(title = glue::glue("Fastest-Rising {gender_input} Names, {from_year}-{to_year}"),
             x = "", y = "Babies Named",
             caption = "Source: US Social Security Administration baby name data")

    fmxdat::finplot(g)
}