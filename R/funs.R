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
    out[is.nan(out)] <- NA
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

to_compare <- function(network1, network2, combine = F){

    # to_compare() adds structural zeroes for two-mode networks based on what is not included in the two networks being compared
    # takes two arguments, network1 and network2, both two-mode network matrices, i.e. to_compare(network1, network2)
    # both networks need to have row.names and col.names (or names for a data.frame) set

    network1 <- as.data.frame(network1)
    network2 <- as.data.frame(network2)

    print(paste0("Processing network1 with ", nrow(network1), " rows", " and ", ncol(network1), " columns."))
    print(paste0("Processing network2 with ", nrow(network2), " rows", " and ", ncol(network2), " columns."))

    print(paste0("### PROCESSING ###"))

    # for mode 1

    add_to_row_network1 <- row.names(network2)[!(row.names(network2) %in% row.names(network1))]
    add_to_row_network2 <- row.names(network1)[!(row.names(network1) %in% row.names(network2))]

    row_vector_network1 <- rep(0, ncol(network1))
    row_vector_network2 <- rep(0, ncol(network2))

    first_row_network1 <- nrow(network1)
    first_row_network2 <- nrow(network2)

    # Something is going wrong here

    for (i in 1:length(add_to_row_network1)){
        network1 <- rbind(network1, row_vector_network1)
        row.names(network1)[first_row_network1 + i] <- add_to_row_network1[i]
    }

    for (i in 1:length(add_to_row_network2)){
        network2 <- rbind(network2, row_vector_network2)
        row.names(network2)[first_row_network2 + i] <- add_to_row_network2[i]
    }

    # for mode 2

    add_to_col_network1 <- names(network2)[!(names(network2) %in% names(network1))]
    add_to_col_network2 <- names(network1)[!(names(network1) %in% names(network2))]

    if(length(add_to_col_network1) > 0 & length(add_to_col_network2) > 0){

        col_vector_network1 <- rep(0, nrow(network1))
        col_vector_network2 <- rep(0, nrow(network2))

        first_col_network1 <- ncol(network1)
        first_col_network2 <- ncol(network2)

        for (i in 1:length(add_to_col_network1)){
            network1 <- cbind(network1, col_vector_network1)
            names(network1)[first_col_network1 + i] <- add_to_col_network1[i]
        }

        for (i in 1:length(add_to_col_network2)){
            network2 <- cbind(network2, col_vector_network2)
            names(network2)[first_col_network2 + i] <- add_to_col_network2[i]
        }

    }

    row_target_network2 <- network1[, 1]
    network2 <- network2[match(row_target_network2, network2[, 1]),]

    col_order_network1 <- sort(names(network1)[1:length(names(network1))])
    network1 <- network1[, col_order_network1]

    col_order_network2 <- sort(names(network2)[1:length(names(network2))])
    network2 <- network2[, col_order_network2]

    print(paste0("Processed network1 with ", nrow(network1), " rows", " and ", ncol(network1), " columns."))
    print(paste0("Processed network2 with ", nrow(network2), " rows", " and ", ncol(network2), " columns."))

    print(paste0("Note: For the object the output is saved to, use [[1]] and [[2]] to index processed networks."))

    if (combine == T){
        print(paste0("For combined networks, for the object the output is saved to use [[3]]"))
        out[[3]] <- network1 + network2
        out <- list(network1, network2, combined_network)
    } else{
        out <- list(network1, network2)
    }
    invisible(out)

}

# cormat_plot is adapted from: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

cormat_plot <- function(df_ss){

    cormat <- round(cor(df_ss, use = "pairwise.complete"), 2)

    # Get lower triangle of the correlation matrix
    get_lower_tri<-function(cormat){
        cormat[upper.tri(cormat)] <- NA
        return(cormat)
    }

    # # Get upper triangle of the correlation matrix
    get_upper_tri <- function(cormat){
        cormat[lower.tri(cormat)]<- NA
        return(cormat)
    }

    lower_tri <- get_lower_tri(cormat)
    melted_cormat <- reshape2::melt(lower_tri, na.rm = T)

    ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
        geom_tile(color = "white")+
        scale_fill_gradient2(low = "#d8b365", high = "#5ab4ac", mid = "#f5f5f5",
                             midpoint = 0, limit = c(-1,1), space = "Lab",
                             name="Pearson\nCorrelation") +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                         size = 13, hjust = 1))+
        theme(axis.text.y = element_text(angle = 0, vjust = 1,
                                         size = 13, hjust = 1))+
        coord_fixed()

    ggheatmap +
        geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
        theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            legend.justification = c(1, 0),
            legend.position = c(0.6, 0.7),
            legend.direction = "horizontal")+
        guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                     title.position = "top", title.hjust = 0.5)) +
        theme(text=element_text(family="Times"))
}

scale_vector <- function(x) {
    x / stats::sd(x, na.rm = TRUE)
}

center_vector <- function(x) {
    x - mean(x, na.rm = TRUE)
}

center_and_scale_vector <- function(x) {
    if (stats::sd(x, na.rm = TRUE) == 0) {
        x - mean(x, na.rm = TRUE)
    } else {
        (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
    }
}

t_tester <- function(dv, fac, df) {

    dv_q <- as.character(substitute(dv))
    fac_q <- as.character(substitute(fac))

    dv_enquo <- enquo(dv)
    fac_enquo <- enquo(fac)

    the_formula <- as.formula(paste(dv_q, " ~ ", fac_q))
    test_results <- stats::t.test(the_formula, data = df)

    print(paste("Test statistic is ", round(test_results$statistic, 3)))
    print(paste("P-value is ", test_results$p.value))

    xx <- dplyr::count(df, !! fac_enquo)
    xx <- dplyr::pull(xx, n)

    effect_size_results <- compute.es::tes(test_results$statistic,
                                           n.1 = xx[1],
                                           n.2 = xx[2],
                                           verbose = F)

    print(paste("Effect size is ", effect_size_results$d))

    out <- dplyr::data_frame(test_statistic = test_results$statistic,
                             p_value = test_results$p.value,
                             effect_size = effect_size_results$d)

    out
}
