plot_orderset <- function(df, Column, Order){
    df[,Column][[1]] <- factor(df[,Column][[1]], levels = Order)
    df
}