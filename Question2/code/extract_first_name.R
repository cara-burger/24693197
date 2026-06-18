# Just grab the first word of the character/artist string as a rough "first name"
extract_first_name <- function(messy_string){
    gsub(" .*", "", messy_string)
}