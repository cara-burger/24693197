billboard_bubble_plot <- function(baby_df, billboard_df, from_year = 2000, to_year = 2014, top_n = 5){

    # get each year's top chart-topping artist first names, ranked by weeks at #1
    yearly_topname <- billboard_df %>%
        filter(rank == 1) %>%
        mutate(Year = lubridate::year(date), first_name = extract_first_name(artist)) %>%
        filter(Year >= from_year, Year <= to_year) %>% group_by(Year, first_name) %>%
        summarise(weeks_at_1 = n()) %>% ungroup() %>%
        arrange(Year, desc(weeks_at_1)) %>%
        group_by(Year) %>%
        mutate(rank_in_year = row_number()) %>%  # number rows within each year
        filter(rank_in_year <= top_n) %>%
        ungroup()

    # attach baby name counts for that same name/year
    plot_df <- yearly_topname %>%
        left_join(baby_df %>% group_by(Name, Year) %>% summarise(Total = sum(Count)), by = c("first_name" = "Name", "Year")) %>%
        filter(!is.na(Total))

    g <- plot_df %>% ggplot(aes(x = Year, y = first_name, size = Total)) +
        geom_point(color = "steelblue", alpha = 0.6) +
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(20), subtitle.size = fmxdat::ggpts(16)) +
        labs(title = "Billboard #1 Names vs Baby Naming Popularity",
             subtitle = glue::glue("Bubble size = babies given that name that year ({from_year}-{to_year})"),
             x = "", y = "", caption = "Source: Billboard Hot 100 & US Social Security Administration") +
        guides(size = "none")

    fmxdat::finplot(g)
}