# Find names with the biggest single-year surge in national popularity by comparing each year's count to the year before
find_name_surges <- function(df, top_n = 15, return_all = FALSE){

    #Summarise data into into one national total per name/year
    name_counts_by_year <- df %>%
        group_by(Name, Gender, Year) %>%
        summarise(Total = sum(Count)) %>%
        ungroup()

    # compare each year's total to the same name's previous year
    # sort by year first so lag() grabs the correct "previous year", not just whatever row happens to be above it
    name_counts_by_year <- name_counts_by_year %>%
        arrange(Name, Gender, Year) %>%
        group_by(Name, Gender) %>%
        mutate(Prev = lag(Total)) %>%
        mutate(Surge = Total - Prev) %>%
        ungroup()

    # if return_all = TRUE, return everything for use in bubble plot
    # if return_all = FALSE (default), return only the top N surges
    if(return_all) return(name_counts_by_year)

    # find the biggest surges
    name_counts_by_year %>%
        filter(!is.na(Surge)) %>%  # first year for each name has no "previous" so drop it
        arrange(desc(Surge)) %>%
        head(top_n)

}
