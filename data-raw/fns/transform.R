transform_cvdn_ds_for_indl_plts <- function(cvdn_ds_tb,
                                         clss_var_nm_1L_chr = "Classes",
                                         fold_var_nm_1L_chr = "Fold"){
  aic_vars_chr <- make_smry_var_nms("AIC")
  bic_vars_chr <- make_smry_var_nms("BIC")
  logLik_vars_chr <- make_smry_var_nms("logLik",
                                       maximise_1L_lgl = T)
  tfd_cvdn_ds_tb <- cvdn_ds_tb %>%
    dplyr::group_by(!!rlang::sym(fold_var_nm_1L_chr)) %>%
    dplyr::mutate(!!rlang::sym(aic_vars_chr[1]) := min(AIC),
                  !!rlang::sym(bic_vars_chr[1]) := min(BIC),
                  !!rlang::sym(logLik_vars_chr[1]) := max(logLik)) %>%
    dplyr::mutate(!!rlang::sym(aic_vars_chr[2]) := ifelse(!!rlang::sym(aic_vars_chr[1]) == AIC,
                                                          !!rlang::sym(clss_var_nm_1L_chr),
                                                          NA),
                  !!rlang::sym(bic_vars_chr[2]) := ifelse(!!rlang::sym(bic_vars_chr[1]) == BIC,
                                                          !!rlang::sym(clss_var_nm_1L_chr),
                                                          NA),
                  !!rlang::sym(logLik_vars_chr[2]) := ifelse(!!rlang::sym(logLik_vars_chr[1]) == logLik,
                                                             !!rlang::sym(clss_var_nm_1L_chr),
                                                             NA)) %>%
    dplyr::mutate(!!rlang::sym(aic_vars_chr[2]) := max(!!rlang::sym(aic_vars_chr[2]),na.rm = TRUE),
                  !!rlang::sym(bic_vars_chr[2]) := max(!!rlang::sym(bic_vars_chr[2]),na.rm = TRUE),
                  !!rlang::sym(logLik_vars_chr[2]) := max(!!rlang::sym(logLik_vars_chr[2]),na.rm = TRUE)) %>%
    dplyr::ungroup()
  return(tfd_cvdn_ds_tb)
}
