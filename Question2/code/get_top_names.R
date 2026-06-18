get_top_names <- function(df, yr, sex, n = 25){
    df %>% filter(Year == yr, Gender == sex) %>% group_by(Name) %>%
        summarise(Total = sum(Count)) %>% arrange(desc(Total)) %>%
        head(n) %>% mutate(Rank = row_number())
}