make_cvdn_ds <- function(cvdn_results_ls,
                       select_from_ls_1L_int = 1L,
                       select_from_df_int = NA_integer_,
                       fold_id_nm_1L_chr = "Fold"){
  # cvdn_results<-lapply(cvdn_results_ls, `[[`, 2)
  # cvdn_results<-lapply(cvdn_results, `[`, c(1,4))
  tfd_cvdn_results_ls <- lapply(cvdn_results_ls, `[[`, select_from_ls_1L_int)
  if(!is.na(select_from_df_int[1])){
    cvdn_ds_xx <- lapply(tfd_cvdn_results_ls, `[`, select_from_df_int)
  }else{
    cvdn_ds_xx <- data.table::rbindlist(tfd_cvdn_results_ls,
                                        idcol = fold_id_nm_1L_chr)
  }
  return(cvdn_ds_xx)
}
make_cvdn_points_ds <- function(smry_cvdn_ds_tb,
                              statistic_var_nm_1L_chr,
                              clss_var_nm_1L_chr = "Classes",
                              maximise_1L_lgl = F){
  if(maximise_1L_lgl){
    fns_ls <- list(fn1 = which.max,
                   fn2 = max)
  }else{
    fns_ls <- list(fn1 = which.min,
                   fn2 = min)
  }
  cvdn_points_ds_df <- data.frame(var_one = smry_cvdn_ds_tb[rlang::exec(fns_ls$fn1,smry_cvdn_ds_tb %>% dplyr::pull(!!rlang::sym(statistic_var_nm_1L_chr))),#DIFF
                                                        clss_var_nm_1L_chr],
                                var_two = rlang::exec(fns_ls$fn2,smry_cvdn_ds_tb %>%
                                                        dplyr::pull(!!rlang::sym(statistic_var_nm_1L_chr))))
  names(cvdn_points_ds_df) <- c(clss_var_nm_1L_chr,statistic_var_nm_1L_chr)#DIFF
  return(cvdn_points_ds_df)
}
make_average_fit_plt <- function(cvdn_ds_tb,
                                 nbr_of_clss_1L_dbl,
                                 statistic_var_nm_1L_chr,
                                 clss_var_nm_1L_chr = "Classes",
                                 maximise_1L_lgl = F,
                                 y_label_1L_chr = NA_character_){
  smry_cvdn_ds_tb <- make_smry_cvdn_ds(cvdn_ds_tb,
                                   clss_var_nm_1L_chr = clss_var_nm_1L_chr,
                                   statistic_var_nm_1L_chr = statistic_var_nm_1L_chr)
  cvdn_points_ds_df <- make_cvdn_points_ds(smry_cvdn_ds_tb,
                                       maximise_1L_lgl = maximise_1L_lgl,
                                       statistic_var_nm_1L_chr = statistic_var_nm_1L_chr)
  plt <- ggplot2::ggplot(smry_cvdn_ds_tb) +
    ggplot2::geom_line(ggplot2::aes(x = !!rlang::sym(clss_var_nm_1L_chr),#
                                    y = !!rlang::sym(statistic_var_nm_1L_chr)) ) + #DIFF
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="none")
  if(!is.na(y_label_1L_chr)){
    plt <- plt   +
      ggplot2::labs(y=y_label_1L_chr)  #DIFF [EXTRA]
  }
  plt <- plt +
    ggplot2::scale_x_continuous(breaks = 2:nbr_of_clss_1L_dbl,
                                limits = c(2, nbr_of_clss_1L_dbl)) +
    ggplot2::geom_point(ggplot2::aes(x = cvdn_points_ds_df %>% dplyr::pull(!!rlang::sym(clss_var_nm_1L_chr)), #DIFF
                                     y = cvdn_points_ds_df %>% dplyr::pull(!!rlang::sym(statistic_var_nm_1L_chr))), #DIFF
                        color = "red",
                        size=3)
  return(plt)
}
make_individual_fit_plt <- function(tfd_cvdn_ds_tb,
                                    nbr_of_clss_1L_dbl,
                                    statistic_var_nm_1L_chr,
                                    clss_var_nm_1L_chr = "Classes",
                                    fold_var_nm_1L_chr = "Fold",
                                    maximise_1L_lgl = F,
                                    y_label_1L_chr = NA_character_){
  smry_var_nms_chr <- make_smry_var_nms(statistic_var_nm_1L_chr,
                                        maximise_1L_lgl = maximise_1L_lgl)
  plt <- ggplot2::ggplot(tfd_cvdn_ds_tb,
                         ggplot2::aes(x = !!rlang::sym(clss_var_nm_1L_chr),#
                                      y = !!rlang::sym(statistic_var_nm_1L_chr),#DIFF
                                      color = factor(!!rlang::sym(fold_var_nm_1L_chr))))
  if(!is.na(y_label_1L_chr)){
    plt <- plt   +
      ggplot2::labs(y = y_label_1L_chr)  #DIFF [EXTRA]
  }
  plt <- plt +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="none") +
    ggplot2::scale_x_continuous(breaks = 2:nbr_of_clss_1L_dbl,
                                limits = c(2, nbr_of_clss_1L_dbl)) +
    ggplot2::geom_point(ggplot2::aes(x = !!rlang::sym(smry_var_nms_chr[2]), # DIFF
                                     y = !!rlang::sym(smry_var_nms_chr[1])), # DIFF
                        size=2)
  return(plt)
}
make_Rand_idx_mat <- function(cvdn_ds_ls,
                              ds_tb,
                              id_var_nm_1L_chr = "ID"){
  RI_calcn_ds_tb <- make_ds_for_RI_calc(cvdn_ds_ls = cvdn_ds_ls,
                                        ds_tb = ds_tb,
                                        id_var_nm_1L_chr = id_var_nm_1L_chr)
  nbr_of_folds_1L_int <- ncol(RI_calcn_ds_tb)
  cal_RI <- Vectorize(calculate_sngl_Rand_idx,
                      vectorize.args = list("i","j"))
  Rand_idx_mat <- outer(1:nbr_of_folds_1L_int,
                        1:nbr_of_folds_1L_int,
                        cal_RI,
                        data_tb = RI_calcn_ds_tb)
  diag(Rand_idx_mat) <- NA
  return(Rand_idx_mat)
}
make_ds_for_RI_calc <- function(cvdn_ds_ls,
                                ds_tb,
                                id_var_nm_1L_chr = "ID"){
  tfd_ds_tb <- ds_tb  %>%
    dplyr::select(!!rlang::sym(id_var_nm_1L_chr))
  for(i in 1:length(cvdn_ds_ls)){
    tfd_ds_tb <- tfd_ds_tb %>%
      dplyr::left_join(cvdn_ds_ls[[i]],
                       by = id_var_nm_1L_chr)
    names(tfd_ds_tb)[i+1] <- paste("fold",i)
  }
  tfd_ds_tb <- tfd_ds_tb %>%
    dplyr::select(-!!rlang::sym(id_var_nm_1L_chr)) %>%
    dplyr::mutate_all(as.factor)
  return(tfd_ds_tb)
}
make_pca_tbl <- function(ds_tb,
                         var_nms_chr,
                         class_var_nm_1L_chr = "class_int"){
  pca_ls <- stats::prcomp(ds_tb %>%
                            dplyr::select(tidyselect::all_of(var_nms_chr)) %>%
                            dplyr::mutate_all(as.numeric),
                          scale.=T)
  pca_df <- as.data.frame(pca_ls$x) %>%
    dplyr::mutate(!!rlang::sym(class_var_nm_1L_chr) := as.factor(ds_tb %>%
                                                                   dplyr::pull(!!rlang::sym(class_var_nm_1L_chr))))
  return(pca_df)
}
make_smry_cvdn_ds <- function(cvdn_ds_tb,
                            statistic_var_nm_1L_chr,
                            clss_var_nm_1L_chr = "Classes"
){
  smry_cvdn_ds_tb <- cvdn_ds_tb %>% # RENAME
    dplyr::group_by(!!rlang::sym(clss_var_nm_1L_chr)) %>%
    dplyr::summarise(!!rlang::sym(statistic_var_nm_1L_chr) := mean(!!rlang::sym(statistic_var_nm_1L_chr))) %>% #DIFF
    dplyr::ungroup()
  return(smry_cvdn_ds_tb)
}
make_smry_var_nms <- function(statistic_var_nm_1L_chr,
                              maximise_1L_lgl = F){
  smry_var_nms_chr <- paste0(ifelse(maximise_1L_lgl,"max","min"),
                             statistic_var_nm_1L_chr,
                             c("","which"))
  return(smry_var_nms_chr)
}
make_cvdn_ls <- function(ds_tb,
                         folds_int,
                         nbr_cores_1L_int = 1L,
                         nbr_clss_1L_int = 15L,
                         var_nms_chr){
  cvdn_ls <- parallel::mclapply(1:max(folds_int), function(i) {
    print(i)
    training_set <- ds_tb[nbr_of_folds_1L_int != i,]
    train_return<-fit_mixture_mdl_clusters(training_set,
                                           nbr_clss_1L_int = nbr_clss_1L_int,
                                           var_nms_chr = var_nms_chr)
  }, mc.cores = nbr_cores_1L_int)
  return(cvdn_ls )
}
