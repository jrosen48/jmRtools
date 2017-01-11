# funs.R

convert_log_odds <- function(x){
    out <- ifelse (x > 0, (exp(1)^x), exp(1)^x / (1 + exp(1)^x))
    return(round(out, 3))
}

convert_odds <- function(x){
    out <- x / (1 + x)
    return(round(out, 3))
}

fix_missing <- function(x) { # from Hadley Wickham's Advanced R
    x[x == -99] <- NA
    return(x)
}

lunique <- function(x){
    return(length(unique(x)))
}

composite_matrix_maker <- function(x, ...){
    out <- dplyr::select(x, ...)
    out <- as.matrix(out)
    return(out)
}

composite_mean_maker <- function(x, ...){
    out <- dplyr::select(x, ...)
    out <- apply(out, 1, function(x) mean(x, na.rm = T))
    return(out)
}

composite_stat_maker <- function(df, ...){
    y <- composite_mean_maker(df, ...)
    y <- psych::describe(y)
    y1 <- paste0(round(y$mean, 2), " (", round(y$sd, 2), ")")
    x <- composite_matrix_maker(df, ...)
    xx <- psych::alpha(x)
    y2 <- paste0("α = ", round(xx$total$raw_alpha, 2))
    y3 <- paste0("λ = ", round(xx$total$`G6(smc)`, 2))
    out <- paste0(y1, ", ", y2, ", ", y3)
    return(out)
}