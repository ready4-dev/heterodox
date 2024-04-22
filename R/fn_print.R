#' Print cluster plots
#' @description print_cluster_plots() is a Print function that prints output to console. Specifically, this function implements an algorithm to print cluster plots. The function is called for its side effects and does not return a value.
#' @param ds_tb Dataset (a tibble)
#' @param clusters_1L_int Clusters (an integer vector of length one)
#' @param var_nms_chr Variable names (a character vector)
#' @param nbr_of_folds_1L_int Number of folds (an integer vector of length one), Default: 2
#' @param var_idx_1L_int Variable index (an integer vector of length one), Default: 1
#' @return No return value, called for side effects.
#' @rdname print_cluster_plots
#' @export 
#' @importFrom caret createFolds
#' @importFrom dplyr pull
#' @importFrom rlang sym
print_cluster_plots <- function (ds_tb, clusters_1L_int, var_nms_chr, nbr_of_folds_1L_int = 2L, 
    var_idx_1L_int = 1L) 
{
    folds_int <- caret::createFolds(ds_tb %>% dplyr::pull(!!rlang::sym(var_nms_chr[var_idx_1L_int])), 
        k = nbr_of_folds_1L_int, list = FALSE)
    for (i in 1:nbr_of_folds_1L_int) {
        testing_set <- ds_tb[folds_int == i, ]
        model_mdl <- fit_mixture_mdl(testing_set, clusters_1L_int = clusters_1L_int, 
            var_nms_chr = var_nms_chr)
        print(plot_cluster(testing_set, cluster_int = as.factor(apply(model_mdl@posterior[, 
            2:(clusters_1L_int + 1)], 1, which.max))))
    }
}
