#' Add class variable
#' @description add_class_var() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add class variable. The function returns Dataset (a tibble).
#' @param ds_tb Dataset (a tibble)
#' @param clusters_1L_int Clusters (an integer vector of length one)
#' @param model_mdl Model (a model)
#' @param class_var_nm_1L_chr Class variable name (a character vector of length one), Default: 'class_int'
#' @return Dataset (a tibble)
#' @rdname add_class_var
#' @export 
#' @importFrom dplyr mutate
#' @importFrom rlang sym
add_class_var <- function (ds_tb, clusters_1L_int, model_mdl, class_var_nm_1L_chr = "class_int") 
{
    ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(class_var_nm_1L_chr), 
        as.factor(apply(model_mdl@posterior[, 2:(clusters_1L_int + 
            1)], 1, which.max))))
    return(ds_tb)
}
#' Add k-means clustering class variable
#' @description add_kmean_cls_var() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add k-means clustering class variable. The function returns Dataset (a tibble).
#' @param ds_tb Dataset (a tibble)
#' @param classes_1L_int Classes (an integer vector of length one)
#' @param components_1L_int Components (an integer vector of length one)
#' @param pca_df Principal component analysis (a data.frame)
#' @param kmean_var_nm_1L_chr K-means clustering variable name (a character vector of length one), Default: 'kmeans_cls_int'
#' @param start_1L_int Start (an integer vector of length one), Default: 25
#' @return Dataset (a tibble)
#' @rdname add_kmean_cls_var
#' @export 
#' @importFrom stats kmeans
#' @importFrom dplyr mutate
#' @importFrom rlang sym
add_kmean_cls_var <- function (ds_tb, classes_1L_int, components_1L_int, pca_df, kmean_var_nm_1L_chr = "kmeans_cls_int", 
    start_1L_int = 25L) 
{
    kmean_ls <- stats::kmeans(pca_df[, 1:components_1L_int], 
        centers = classes_1L_int, nstart = start_1L_int)
    ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(kmean_var_nm_1L_chr), 
        as.factor(kmean_ls$cluster)))
    return(ds_tb)
}
