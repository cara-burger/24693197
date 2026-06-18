Winsorise_Loans <- function(df, col, BottomQ = 0.01, TopQ = 0.99){
    Bottom <- quantile(df[[col]], BottomQ, na.rm = TRUE)
    Top <- quantile(df[[col]], TopQ, na.rm = TRUE)
    df[[col]] <- ifelse(df[[col]] < Bottom, Bottom, ifelse(df[[col]] > Top, Top, df[[col]]))
    df
}