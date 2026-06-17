rank_persistence <- function(df, yr, sex, gap = 3){
    # get the top 25 for this year, and the top 25 a few years later
    t1 <- top25_names(df, yr, sex)
    t2 <- top25_names(df, yr + gap, sex)

    # only keep names that show up in BOTH lists
    joined <- inner_join(t1, t2, by = "Name", suffix = c("_t1", "_t2"))

    # if barely anything overlapped, a correlation wouldn't mean much
    if(nrow(joined) < 3) return(NA)

    # compare the two rankings:
    #Spearman logic says close to 1 = order barely changed, close to 0 = scrambled
    cor(joined$rank_t1, joined$rank_t2, method = "spearman")
}