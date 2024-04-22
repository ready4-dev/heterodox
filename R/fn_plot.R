#' Plot average fit
#' @description plot_average_fit() is a Plot function that plots data. Specifically, this function implements an algorithm to plot average fit. The function returns Average fit (a plot).
#' @param cvdn_ds_tb Cross-validation dataset (a tibble)
#' @param clss_var_nm_1L_chr Classes variable name (a character vector of length one), Default: 'Classes'
#' @return Average fit (a plot)
#' @rdname plot_average_fit
#' @export 
#' @importFrom gridExtra grid.arrange
#' @importFrom ggpubr ggarrange
plot_average_fit <- function (cvdn_ds_tb, clss_var_nm_1L_chr = "Classes") 
{
    nbr_of_clss_1L_dbl <- get_nbr_of_clss(cvdn_ds_tb, clss_var_nm_1L_chr = clss_var_nm_1L_chr)
    plot_ls <- list()
    plot_ls[[1]] <- make_average_fit_plt(cvdn_ds_tb, nbr_of_clss_1L_dbl = nbr_of_clss_1L_dbl, 
        statistic_var_nm_1L_chr = "AIC")
    plot_ls[[2]] <- make_average_fit_plt(cvdn_ds_tb, nbr_of_clss_1L_dbl = nbr_of_clss_1L_dbl, 
        statistic_var_nm_1L_chr = "BIC")
    plot_ls[[3]] <- make_average_fit_plt(cvdn_ds_tb, maximise_1L_lgl = T, 
        nbr_of_clss_1L_dbl = nbr_of_clss_1L_dbl, statistic_var_nm_1L_chr = "logLik", 
        y_label_1L_chr = "Log-likelihood")
    average_fit_plt <- gridExtra::grid.arrange(ggpubr::ggarrange(plotlist = plot_ls, 
        ncol = 1))
    return(average_fit_plt)
}
#' Plot cluster
#' @description plot_cluster() is a Plot function that plots data. Specifically, this function implements an algorithm to plot cluster. The function is called for its side effects and does not return a value.
#' @param ds_tb Dataset (a tibble)
#' @param cluster_int Cluster (an integer vector)
#' @return No return value, called for side effects.
#' @rdname plot_cluster
#' @export 
#' @importFrom stringr str_to_sentence str_sub
#' @importFrom ggplot2 ggplot aes_string geom_density aes labs theme_bw theme scale_fill_manual scale_colour_manual
#' @importFrom youthvars get_guide_box_lgd
#' @importFrom gridExtra grid.arrange
#' @importFrom ggpubr ggarrange
plot_cluster <- function (ds_tb, cluster_int) 
{
    colNames <- names(ds_tb)[21:26]
    ds_tb$cluster_int <- cluster_int
    plot_ls <- list()
    for (i in colNames) {
        labelx <- eval(parse(text = paste0("attributes(ds_tb$", 
            i, ")$label")))
        labelx <- stringr::str_to_sentence(stringr::str_sub(labelx, 
            38, 55))
        plot_ls[[i]] <- ggplot2::ggplot(ds_tb, ggplot2::aes_string(i)) + 
            ggplot2::geom_density(ggplot2::aes(fill = cluster_int, 
                colour = cluster_int), alpha = 0.4, adjust = 2.5) + 
            ggplot2::labs(x = labelx, y = "Density") + ggplot2::theme_bw() + 
            ggplot2::theme(legend.position = "none") + ggplot2::scale_fill_manual(values = c("#fdcc8a", 
            "#fc8d59", "#e34a33", "#b30000")) + ggplot2::scale_colour_manual(values = c("#fdcc8a", 
            "#fc8d59", "#e34a33", "#b30000"))
    }
    legend <- ggplot2::ggplot(ds_tb, ggplot2::aes_string(i)) + 
        ggplot2::geom_density(ggplot2::aes(fill = cluster_int, 
            colour = cluster_int), alpha = 0.4, adjust = 2.5) + 
        ggplot2::labs(x = labelx, y = "Density", fill = "Class", 
            col = "Class") + ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom") + 
        ggplot2::scale_fill_manual(values = c("#fdcc8a", "#fc8d59", 
            "#e34a33", "#b30000")) + ggplot2::scale_colour_manual(values = c("#fdcc8a", 
        "#fc8d59", "#e34a33", "#b30000"))
    legend <- youthvars::get_guide_box_lgd(legend)
    print(gridExtra::grid.arrange(ggpubr::ggarrange(plotlist = plot_ls, 
        nrow = 3, ncol = 2), legend, nrow = 2, heights = c(10, 
        1)))
}
#' Plot individual fit
#' @description plot_individual_fit() is a Plot function that plots data. Specifically, this function implements an algorithm to plot individual fit. The function returns Individual fit (a plot).
#' @param cvdn_ds_tb Cross-validation dataset (a tibble)
#' @param clss_var_nm_1L_chr Classes variable name (a character vector of length one), Default: 'Classes'
#' @param fold_var_nm_1L_chr Fold variable name (a character vector of length one), Default: 'Fold'
#' @param legend_label_1L_chr Legend label (a character vector of length one), Default: 'Datasets'
#' @return Individual fit (a plot)
#' @rdname plot_individual_fit
#' @export 
#' @importFrom ggplot2 ggplot aes geom_line theme_bw theme labs
#' @importFrom rlang sym
#' @importFrom youthvars get_guide_box_lgd
#' @importFrom gridExtra grid.arrange
#' @importFrom ggpubr ggarrange
plot_individual_fit <- function (cvdn_ds_tb, clss_var_nm_1L_chr = "Classes", fold_var_nm_1L_chr = "Fold", 
    legend_label_1L_chr = "Datasets") 
{
    nbr_of_clss_1L_dbl <- get_nbr_of_clss(cvdn_ds_tb, clss_var_nm_1L_chr = clss_var_nm_1L_chr)
    tfd_cvdn_ds_tb <- transform_cvdn_ds_for_indl_plts(cvdn_ds_tb, 
        clss_var_nm_1L_chr = clss_var_nm_1L_chr, fold_var_nm_1L_chr = fold_var_nm_1L_chr)
    plot_ls <- list()
    plot_ls[[1]] <- make_individual_fit_plt(tfd_cvdn_ds_tb, nbr_of_clss_1L_dbl = nbr_of_clss_1L_dbl, 
        statistic_var_nm_1L_chr = "AIC", clss_var_nm_1L_chr = clss_var_nm_1L_chr, 
        fold_var_nm_1L_chr = fold_var_nm_1L_chr, maximise_1L_lgl = F, 
        y_label_1L_chr = NA_character_)
    plot_ls[[2]] <- make_individual_fit_plt(tfd_cvdn_ds_tb, nbr_of_clss_1L_dbl = nbr_of_clss_1L_dbl, 
        statistic_var_nm_1L_chr = "BIC", clss_var_nm_1L_chr = clss_var_nm_1L_chr, 
        fold_var_nm_1L_chr = fold_var_nm_1L_chr, maximise_1L_lgl = F, 
        y_label_1L_chr = NA_character_)
    plot_ls[[3]] <- make_individual_fit_plt(tfd_cvdn_ds_tb, nbr_of_clss_1L_dbl = nbr_of_clss_1L_dbl, 
        statistic_var_nm_1L_chr = "logLik", clss_var_nm_1L_chr = clss_var_nm_1L_chr, 
        fold_var_nm_1L_chr = fold_var_nm_1L_chr, maximise_1L_lgl = T, 
        y_label_1L_chr = "Log-likelihood")
    legend <- ggplot2::ggplot(tfd_cvdn_ds_tb, ggplot2::aes(x = !!rlang::sym(clss_var_nm_1L_chr), 
        y = AIC, color = factor(!!rlang::sym(fold_var_nm_1L_chr)))) + 
        ggplot2::geom_line() + ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom") + 
        ggplot2::labs(col = legend_label_1L_chr)
    legend <- youthvars::get_guide_box_lgd(legend)
    individual_fit_plt <- gridExtra::grid.arrange(ggpubr::ggarrange(plotlist = plot_ls, 
        ncol = 1), legend, nrow = 2, heights = c(10, 1))
    return(individual_fit_plt)
}
#' Plot principal component analysis
#' @description plot_pca() is a Plot function that plots data. Specifically, this function implements an algorithm to plot principal component analysis. The function is called for its side effects and does not return a value.
#' @param ds_tb Dataset (a tibble)
#' @param pca_df Principal component analysis (a data.frame)
#' @param class_var_nm_1L_chr Class variable name (a character vector of length one), Default: 'class_int'
#' @return No return value, called for side effects.
#' @rdname plot_pca
#' @export 
#' @importFrom ggplot2 ggplot aes geom_jitter theme_bw
#' @importFrom rlang sym
plot_pca <- function (ds_tb, pca_df, class_var_nm_1L_chr = "class_int") 
{
    ggplot2::ggplot(pca_df, ggplot2::aes(x = PC1, y = PC2, colour = !!rlang::sym(class_var_nm_1L_chr))) + 
        ggplot2::geom_jitter(alpha = 0.3) + ggplot2::theme_bw()
}
