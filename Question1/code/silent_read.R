silent_read <- function(path){
    hushread <- purrr::quietly(read_csv)
    df <- hushread(path)
    df$result
}