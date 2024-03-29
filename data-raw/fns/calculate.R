calculate_mean_ri <- function(cvdn_results_ls,
                              ds_tb,
                              fold_id_nm_1L_chr = "Fold",
                              id_var_nm_1L_chr = "ID",
                              select_from_ls_1L_int = 2L,
                              select_from_df_int = c(1L,4L)){
  cvdn_ds_ls <- make_cvdn_ds(cvdn_results_ls,
                         fold_id_nm_1L_chr = fold_id_nm_1L_chr,
                         select_from_ls_1L_int = select_from_ls_1L_int,
                         select_from_df_int = select_from_df_int)
  ri_mat <- make_ri_mat(cvdn_ds_ls,
                                    ds_tb = ds_tb,
                                    id_var_nm_1L_chr = id_var_nm_1L_chr)
  mean_ri_dbl <- mean(ri_mat,na.rm = TRUE)
  return(mean_ri_dbl)
}
calculate_sngl_ri <- function(i,j,data_tb){
  ri_dbl <- 1
  if(i!=j){
    data_tb <- data_tb[,c(i,j)] %>% na.omit()
    ri_dbl <- aricode::RI(data_tb %>% dplyr::pull(1), # EDITED
                                data_tb %>% dplyr::pull(2)) # EDITED
  }
  return(ri_dbl)
}
