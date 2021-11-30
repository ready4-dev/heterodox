add_class_var <- function(ds_tb,
                          clusters_1L_int,
                          model_mdl,
                          class_var_nm_1L_chr = "class_int"){
  ds_tb <- ds_tb %>%
    dplyr::mutate(!!rlang::sym(class_var_nm_1L_chr) := as.factor(apply(model_mdl@posterior[,2:(clusters_1L_int + 1)],
                                                                       1,
                                                                       which.max)))
  return(ds_tb)
}
add_kmeans_cls_var <- function(ds_tb,
                               classes_1L_int,
                               components_1L_int,
                               pca_df,
                               kmeans_var_nm_1L_chr = "kmeans_cls_int",
                               start_1L_int = 25L){
  kmeans_ls <- kmeans(pca_df[,1:components_1L_int],
                      centers = classes_1L_int,
                      nstart = start_1L_int)
  ds_tb <- ds_tb %>%
    dplyr::mutate(!!rlang::sym(kmeans_var_nm_1L_chr) := as.factor(kmeans_ls$cluster))
  return(ds_tb)
}
