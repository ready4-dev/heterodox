#' Calculate meanand Index
#' @description calculate_mean_ri() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate meanand index. The function returns Meanand Index (a double vector).
#' @param cvdn_results_ls Cross-validation results (a list)
#' @param ds_tb Dataset (a tibble)
#' @param fold_id_nm_1L_chr Fold identity name (a character vector of length one), Default: 'Fold'
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one), Default: 'ID'
#' @param select_from_ls_1L_int Select from list (an integer vector of length one), Default: 2
#' @param select_from_df_int Select from data.frame (an integer vector), Default: c(1L, 4L)
#' @return Meanand Index (a double vector)
#' @rdname calculate_mean_ri
#' @export 
calculate_mean_ri <- function (cvdn_results_ls, ds_tb, fold_id_nm_1L_chr = "Fold", 
    id_var_nm_1L_chr = "ID", select_from_ls_1L_int = 2L, select_from_df_int = c(1L, 
        4L)) 
{
    cvdn_ds_ls <- make_cvdn_ds(cvdn_results_ls, fold_id_nm_1L_chr = fold_id_nm_1L_chr, 
        select_from_ls_1L_int = select_from_ls_1L_int, select_from_df_int = select_from_df_int)
    ri_mat <- make_ri_mat(cvdn_ds_ls, ds_tb = ds_tb, id_var_nm_1L_chr = id_var_nm_1L_chr)
    mean_ri_dbl <- mean(ri_mat, na.rm = TRUE)
    return(mean_ri_dbl)
}
#' Calculate singleand Index
#' @description calculate_sngl_ri() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate singleand index. The function returns a Rand Index (a double vector).
#' @param i An object
#' @param j An object
#' @param data_tb Data (a tibble)
#' @return a Rand Index (a double vector)
#' @rdname calculate_sngl_ri
#' @export 
#' @importFrom aricode RI
#' @importFrom dplyr pull
#' @keywords internal
calculate_sngl_ri <- function (i, j, data_tb) 
{
    ri_dbl <- 1
    if (i != j) {
        data_tb <- data_tb[, c(i, j)] %>% na.omit()
        ri_dbl <- aricode::RI(data_tb %>% dplyr::pull(1), data_tb %>% 
            dplyr::pull(2))
    }
    return(ri_dbl)
}
