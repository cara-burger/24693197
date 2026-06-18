clean_netflix <- function(data_path) {

    # load titles, keep movies with valid runtimes
    titles_raw <- read_rds(file.path(data_path, "titles.rds"))
    titles_clean <- titles_raw %>% filter(type == "MOVIE") %>% filter(!is.na(runtime), runtime > 0) %>%

        # remove the [] brackets
        mutate(genres = gsub("\\[|\\]|'", "", genres), production_countries = gsub("\\[|\\]|'", "", production_countries))

    # grab actors from credits
    credits_raw <- read_rds(file.path(data_path, "credits.rds"))
    actors_clean <- credits_raw %>% filter(role == "ACTOR")

    list(titles = titles_clean, actors = actors_clean)
}