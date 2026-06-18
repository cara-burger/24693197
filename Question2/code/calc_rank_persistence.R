calc_rank_persistence <- function(df, yr, sex, gap = 3){
    # top 25 names in the base year
    base_year <- get_top_names(df, yr, sex)
    # full ranking in the future year so names that fell out still get a rank
    future_year <- get_all_names_ranked(df, yr + gap, sex)
    # join base year names against full future ranking
    joined <- base_year %>% select(Name, Rank_Base = Rank) %>%
        left_join(future_year %>% select(Name, Rank_Future = Rank), by = "Name")
    # names that disappeared get worst possible rank rather than being dropped
    worst <- nrow(future_year) + 1
    joined <- joined %>% mutate(Rank_Future = ifelse(is.na(Rank_Future), worst, Rank_Future))

    cor(joined$Rank_Base, joined$Rank_Future, method = "spearman")
}