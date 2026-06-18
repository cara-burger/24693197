get_all_names_ranked <- function(df, yr, sex){
    df %>% filter(Year == yr, Gender == sex) %>%
        group_by(Name) %>% summarise(Total = sum(Count)) %>%
        arrange(desc(Total)) %>% mutate(Rank = row_number())
}