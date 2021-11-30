calculate_mean_RI <- function(cv_results_ls,
                              ds_tb,
                              fold_id_nm_1L_chr = "Fold",
                              id_var_nm_1L_chr = "ID",
                              select_from_ls_1L_int = 2L,
                              select_from_df_int = c(1L,4L)){
  cv_ds_ls <- make_cvdn_ds(cv_results_ls,
                         fold_id_nm_1L_chr = fold_id_nm_1L_chr,
                         select_from_ls_1L_int = select_from_ls_1L_int,
                         select_from_df_int = select_from_df_int)
  Rand_idx_mat <- make_Rand_idx_mat(cv_ds_ls,
                                    ds_tb = ds_tb,
                                    id_var_nm_1L_chr = id_var_nm_1L_chr)
  mean_RI_dbl <- mean(Rand_idx_mat,na.rm = TRUE)
  return(mean_RI_dbl)
}
calculate_sngl_Rand_idx <- function(i,j,data_tb){
  Rand_idx_dbl <- 1
  if(i!=j){
    data_tb <- data_tb[,c(i,j)] %>% na.omit()
    Rand_idx_dbl <- aricode::RI(data_tb %>% dplyr::pull(1), # EDITED
                                data_tb %>% dplyr::pull(2)) # EDITED
  }
  return(Rand_idx_dbl)
}
