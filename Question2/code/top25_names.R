top25_names <- function(df, yr, sex){
    df %>% filter(Year == yr, Gender == sex) %>% group_by(Name) %>%
        summarise(Total = sum(Count)) %>% arrange(desc(Total)) %>%
        head(25) %>% mutate(rank = row_number())
}