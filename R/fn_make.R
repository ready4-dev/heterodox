#' Make average fit plot
#' @description make_average_fit_plt() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make average fit plot. The function returns Plot (a plot).
#' @param cvdn_ds_tb Cross-validation dataset (a tibble)
#' @param nbr_of_clss_1L_dbl Number of classes (a double vector of length one)
#' @param statistic_var_nm_1L_chr Statistic variable name (a character vector of length one)
#' @param clss_var_nm_1L_chr Classes variable name (a character vector of length one), Default: 'Classes'
#' @param maximise_1L_lgl Maximise (a logical vector of length one), Default: F
#' @param y_label_1L_chr Y label (a character vector of length one), Default: 'NA'
#' @return Plot (a plot)
#' @rdname make_average_fit_plt
#' @export 
#' @importFrom ggplot2 ggplot geom_line aes theme_bw theme labs scale_x_continuous geom_point
#' @importFrom rlang sym
#' @importFrom dplyr pull
#' @keywords internal
make_average_fit_plt <- function (cvdn_ds_tb, nbr_of_clss_1L_dbl, statistic_var_nm_1L_chr, 
    clss_var_nm_1L_chr = "Classes", maximise_1L_lgl = F, y_label_1L_chr = NA_character_) 
{
    smry_cvdn_ds_tb <- make_smry_cvdn_ds(cvdn_ds_tb, clss_var_nm_1L_chr = clss_var_nm_1L_chr, 
        statistic_var_nm_1L_chr = statistic_var_nm_1L_chr)
    cvdn_points_ds_df <- make_cvdn_points_ds(smry_cvdn_ds_tb, 
        maximise_1L_lgl = maximise_1L_lgl, statistic_var_nm_1L_chr = statistic_var_nm_1L_chr)
    plt <- ggplot2::ggplot(smry_cvdn_ds_tb) + ggplot2::geom_line(ggplot2::aes(x = !!rlang::sym(clss_var_nm_1L_chr), 
        y = !!rlang::sym(statistic_var_nm_1L_chr))) + ggplot2::theme_bw() + 
        ggplot2::theme(legend.position = "none")
    if (!is.na(y_label_1L_chr)) {
        plt <- plt + ggplot2::labs(y = y_label_1L_chr)
    }
    plt <- plt + ggplot2::scale_x_continuous(breaks = 2:nbr_of_clss_1L_dbl, 
        limits = c(2, nbr_of_clss_1L_dbl)) + ggplot2::geom_point(ggplot2::aes(x = cvdn_points_ds_df %>% 
        dplyr::pull(!!rlang::sym(clss_var_nm_1L_chr)), y = cvdn_points_ds_df %>% 
        dplyr::pull(!!rlang::sym(statistic_var_nm_1L_chr))), 
        color = "red", size = 3)
    return(plt)
}
#' Make cross-validation dataset
#' @description make_cvdn_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make cross-validation dataset. The function returns Cross-validation dataset (an output object of multiple potential types).
#' @param cvdn_results_ls Cross-validation results (a list)
#' @param select_from_ls_1L_int Select from list (an integer vector of length one), Default: 1
#' @param select_from_df_int Select from data.frame (an integer vector), Default: NA
#' @param fold_id_nm_1L_chr Fold identity name (a character vector of length one), Default: 'Fold'
#' @return Cross-validation dataset (an output object of multiple potential types)
#' @rdname make_cvdn_ds
#' @export 
#' @importFrom data.table rbindlist
make_cvdn_ds <- function (cvdn_results_ls, select_from_ls_1L_int = 1L, select_from_df_int = NA_integer_, 
    fold_id_nm_1L_chr = "Fold") 
{
    tfd_cvdn_results_ls <- lapply(cvdn_results_ls, `[[`, select_from_ls_1L_int)
    if (!is.na(select_from_df_int[1])) {
        cvdn_ds_xx <- lapply(tfd_cvdn_results_ls, `[`, select_from_df_int)
    }
    else {
        cvdn_ds_xx <- data.table::rbindlist(tfd_cvdn_results_ls, 
            idcol = fold_id_nm_1L_chr)
    }
    return(cvdn_ds_xx)
}
#' Make cross-validation list
#' @description make_cvdn_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make cross-validation list. The function returns Cross-validation (a list).
#' @param ds_tb Dataset (a tibble)
#' @param folds_int Folds (an integer vector)
#' @param nbr_cores_1L_int Number cores (an integer vector of length one), Default: 1
#' @param nbr_clss_1L_int Number classes (an integer vector of length one), Default: 15
#' @param var_nms_chr Variable names (a character vector)
#' @return Cross-validation (a list)
#' @rdname make_cvdn_ls
#' @export 
#' @importFrom parallel mclapply
make_cvdn_ls <- function (ds_tb, folds_int, nbr_cores_1L_int = 1L, nbr_clss_1L_int = 15L, 
    var_nms_chr) 
{
    cvdn_ls <- parallel::mclapply(1:max(folds_int), function(i) {
        print(i)
        training_set <- ds_tb[folds_int != i, ]
        train_return <- fit_mixture_mdl_clusters(training_set, 
            nbr_clss_1L_int = nbr_clss_1L_int, var_nms_chr = var_nms_chr)
    }, mc.cores = nbr_cores_1L_int)
    return(cvdn_ls)
}
#' Make cross-validation points dataset
#' @description make_cvdn_points_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make cross-validation points dataset. The function returns Cross-validation points dataset (a data.frame).
#' @param smry_cvdn_ds_tb Summary cross-validation dataset (a tibble)
#' @param statistic_var_nm_1L_chr Statistic variable name (a character vector of length one)
#' @param clss_var_nm_1L_chr Classes variable name (a character vector of length one), Default: 'Classes'
#' @param maximise_1L_lgl Maximise (a logical vector of length one), Default: F
#' @return Cross-validation points dataset (a data.frame)
#' @rdname make_cvdn_points_ds
#' @export 
#' @importFrom rlang exec sym
#' @importFrom dplyr pull
#' @keywords internal
make_cvdn_points_ds <- function (smry_cvdn_ds_tb, statistic_var_nm_1L_chr, clss_var_nm_1L_chr = "Classes", 
    maximise_1L_lgl = F) 
{
    if (maximise_1L_lgl) {
        fns_ls <- list(fn1 = which.max, fn2 = max)
    }
    else {
        fns_ls <- list(fn1 = which.min, fn2 = min)
    }
    cvdn_points_ds_df <- data.frame(var_one = smry_cvdn_ds_tb[rlang::exec(fns_ls$fn1, 
        smry_cvdn_ds_tb %>% dplyr::pull(!!rlang::sym(statistic_var_nm_1L_chr))), 
        clss_var_nm_1L_chr], var_two = rlang::exec(fns_ls$fn2, 
        smry_cvdn_ds_tb %>% dplyr::pull(!!rlang::sym(statistic_var_nm_1L_chr))))
    names(cvdn_points_ds_df) <- c(clss_var_nm_1L_chr, statistic_var_nm_1L_chr)
    return(cvdn_points_ds_df)
}
#' Make dataset forand Index calculation
#' @description make_ds_for_ri_calc() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make dataset forand index calculation. The function returns Transformed dataset (a tibble).
#' @param cvdn_ds_ls Cross-validation dataset (a list)
#' @param ds_tb Dataset (a tibble)
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one), Default: 'ID'
#' @return Transformed dataset (a tibble)
#' @rdname make_ds_for_ri_calc
#' @export 
#' @importFrom dplyr select left_join mutate_all
#' @importFrom rlang sym
#' @keywords internal
make_ds_for_ri_calc <- function (cvdn_ds_ls, ds_tb, id_var_nm_1L_chr = "ID") 
{
    tfd_ds_tb <- ds_tb %>% dplyr::select(!!rlang::sym(id_var_nm_1L_chr))
    for (i in 1:length(cvdn_ds_ls)) {
        tfd_ds_tb <- tfd_ds_tb %>% dplyr::left_join(cvdn_ds_ls[[i]], 
            by = id_var_nm_1L_chr)
        names(tfd_ds_tb)[i + 1] <- paste("fold", i)
    }
    tfd_ds_tb <- tfd_ds_tb %>% dplyr::select(-!!rlang::sym(id_var_nm_1L_chr)) %>% 
        dplyr::mutate_all(as.factor)
    return(tfd_ds_tb)
}
#' Make individual fit plot
#' @description make_individual_fit_plt() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make individual fit plot. The function returns Plot (a plot).
#' @param tfd_cvdn_ds_tb Transformed cross-validation dataset (a tibble)
#' @param nbr_of_clss_1L_dbl Number of classes (a double vector of length one)
#' @param statistic_var_nm_1L_chr Statistic variable name (a character vector of length one)
#' @param clss_var_nm_1L_chr Classes variable name (a character vector of length one), Default: 'Classes'
#' @param fold_var_nm_1L_chr Fold variable name (a character vector of length one), Default: 'Fold'
#' @param maximise_1L_lgl Maximise (a logical vector of length one), Default: F
#' @param y_label_1L_chr Y label (a character vector of length one), Default: 'NA'
#' @return Plot (a plot)
#' @rdname make_individual_fit_plt
#' @export 
#' @importFrom ggplot2 ggplot aes labs geom_line theme_bw theme scale_x_continuous geom_point
#' @importFrom rlang sym
#' @keywords internal
make_individual_fit_plt <- function (tfd_cvdn_ds_tb, nbr_of_clss_1L_dbl, statistic_var_nm_1L_chr, 
    clss_var_nm_1L_chr = "Classes", fold_var_nm_1L_chr = "Fold", 
    maximise_1L_lgl = F, y_label_1L_chr = NA_character_) 
{
    smry_var_nms_chr <- make_smry_var_nms(statistic_var_nm_1L_chr, 
        maximise_1L_lgl = maximise_1L_lgl)
    plt <- ggplot2::ggplot(tfd_cvdn_ds_tb, ggplot2::aes(x = !!rlang::sym(clss_var_nm_1L_chr), 
        y = !!rlang::sym(statistic_var_nm_1L_chr), color = factor(!!rlang::sym(fold_var_nm_1L_chr))))
    if (!is.na(y_label_1L_chr)) {
        plt <- plt + ggplot2::labs(y = y_label_1L_chr)
    }
    plt <- plt + ggplot2::geom_line() + ggplot2::theme_bw() + 
        ggplot2::theme(legend.position = "none") + ggplot2::scale_x_continuous(breaks = 2:nbr_of_clss_1L_dbl, 
        limits = c(2, nbr_of_clss_1L_dbl)) + ggplot2::geom_point(ggplot2::aes(x = !!rlang::sym(smry_var_nms_chr[2]), 
        y = !!rlang::sym(smry_var_nms_chr[1])), size = 2)
    return(plt)
}
#' Make principal component analysis table
#' @description make_pca_tbl() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make principal component analysis table. The function returns Principal component analysis (a data.frame).
#' @param ds_tb Dataset (a tibble)
#' @param var_nms_chr Variable names (a character vector)
#' @param class_var_nm_1L_chr Class variable name (a character vector of length one), Default: 'class_int'
#' @return Principal component analysis (a data.frame)
#' @rdname make_pca_tbl
#' @export 
#' @importFrom stats prcomp
#' @importFrom dplyr select mutate_all mutate pull
#' @importFrom tidyselect all_of
#' @importFrom rlang sym
make_pca_tbl <- function (ds_tb, var_nms_chr, class_var_nm_1L_chr = "class_int") 
{
    pca_ls <- stats::prcomp(ds_tb %>% dplyr::select(tidyselect::all_of(var_nms_chr)) %>% 
        dplyr::mutate_all(as.numeric), scale. = T)
    pca_df <- as.data.frame(pca_ls$x) %>% dplyr::mutate(`:=`(!!rlang::sym(class_var_nm_1L_chr), 
        as.factor(ds_tb %>% dplyr::pull(!!rlang::sym(class_var_nm_1L_chr)))))
    return(pca_df)
}
#' Makeand Index matrix
#' @description make_ri_mat() is a Make function that creates a new R object. Specifically, this function implements an algorithm to makeand index matrix. The function returns a Rand Index (a matrix).
#' @param cvdn_ds_ls Cross-validation dataset (a list)
#' @param ds_tb Dataset (a tibble)
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one), Default: 'ID'
#' @return a Rand Index (a matrix)
#' @rdname make_ri_mat
#' @export 
#' @keywords internal
make_ri_mat <- function (cvdn_ds_ls, ds_tb, id_var_nm_1L_chr = "ID") 
{
    RI_calcn_ds_tb <- make_ds_for_ri_calc(cvdn_ds_ls = cvdn_ds_ls, 
        ds_tb = ds_tb, id_var_nm_1L_chr = id_var_nm_1L_chr)
    nbr_of_folds_1L_int <- ncol(RI_calcn_ds_tb)
    cal_ri <- Vectorize(calculate_sngl_ri, vectorize.args = list("i", 
        "j"))
    ri_mat <- outer(1:nbr_of_folds_1L_int, 1:nbr_of_folds_1L_int, 
        cal_ri, data_tb = RI_calcn_ds_tb)
    diag(ri_mat) <- NA
    return(ri_mat)
}
#' Make summary cross-validation dataset
#' @description make_smry_cvdn_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make summary cross-validation dataset. The function returns Summary cross-validation dataset (a tibble).
#' @param cvdn_ds_tb Cross-validation dataset (a tibble)
#' @param statistic_var_nm_1L_chr Statistic variable name (a character vector of length one)
#' @param clss_var_nm_1L_chr Classes variable name (a character vector of length one), Default: 'Classes'
#' @return Summary cross-validation dataset (a tibble)
#' @rdname make_smry_cvdn_ds
#' @export 
#' @importFrom dplyr group_by summarise ungroup
#' @importFrom rlang sym
#' @keywords internal
make_smry_cvdn_ds <- function (cvdn_ds_tb, statistic_var_nm_1L_chr, clss_var_nm_1L_chr = "Classes") 
{
    smry_cvdn_ds_tb <- cvdn_ds_tb %>% dplyr::group_by(!!rlang::sym(clss_var_nm_1L_chr)) %>% 
        dplyr::summarise(`:=`(!!rlang::sym(statistic_var_nm_1L_chr), 
            mean(!!rlang::sym(statistic_var_nm_1L_chr)))) %>% 
        dplyr::ungroup()
    return(smry_cvdn_ds_tb)
}
#' Make summary variable names
#' @description make_smry_var_nms() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make summary variable names. The function returns Summary variable names (a character vector).
#' @param statistic_var_nm_1L_chr Statistic variable name (a character vector of length one)
#' @param maximise_1L_lgl Maximise (a logical vector of length one), Default: F
#' @return Summary variable names (a character vector)
#' @rdname make_smry_var_nms
#' @export 
#' @keywords internal
make_smry_var_nms <- function (statistic_var_nm_1L_chr, maximise_1L_lgl = F) 
{
    smry_var_nms_chr <- paste0(ifelse(maximise_1L_lgl, "max", 
        "min"), statistic_var_nm_1L_chr, c("", "which"))
    return(smry_var_nms_chr)
}
