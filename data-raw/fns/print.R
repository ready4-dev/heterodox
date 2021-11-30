print_cluster_plots <- function(ds_tb,
                                clusters_1L_int,
                                var_nms_chr,
                                nbr_of_folds_1L_int = 2L,
                                var_idc_1L_int = 1L){
  folds_int <- caret::createFolds(ds_tb$Q1,
                                  k = nbr_of_folds_1L_int,
                                  list = FALSE)
  for(i in 1:nbr_of_folds_1L_int){
    testing_set <- ds_tb[folds_int == i,]
    model_mdl <- fit_mixture_mdl(testing_set,
                                 clusters_1L_int = clusters_1L_int,#4
                                 var_nms_chr = var_nms_chr)#paste0("Q",1:20)
    print(plot_cluster(testing_set,
                       cluster_int = as.factor(apply(model_mdl@posterior[,2:(clusters_1L_int + 1)],
                                                     1,
                                                     which.max))))
  }
}
