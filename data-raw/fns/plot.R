plot_average_fit <- function(cv_ds_tb,
                             clss_var_nm_1L_chr = "Classes"){
  nbr_of_clss_1L_dbl <- get_nbr_of_clss(cv_ds_tb,
                                        clss_var_nm_1L_chr = clss_var_nm_1L_chr)
  plot_ls <- list()
  plot_ls[[1]] <- make_average_fit_plt(cv_ds_tb,
                                       nbr_of_clss_1L_dbl = nbr_of_clss_1L_dbl,
                                       statistic_var_nm_1L_chr = "AIC")
  plot_ls[[2]] <- make_average_fit_plt(cv_ds_tb,
                                       nbr_of_clss_1L_dbl = nbr_of_clss_1L_dbl,
                                       statistic_var_nm_1L_chr = "BIC")
  plot_ls[[3]] <- make_average_fit_plt(cv_ds_tb,
                                       maximise_1L_lgl = T,
                                       nbr_of_clss_1L_dbl = nbr_of_clss_1L_dbl,
                                       statistic_var_nm_1L_chr = "logLik",
                                       y_label_1L_chr = "Log-likelihood")
  average_fit_plt <- gridExtra::grid.arrange(ggpubr::ggarrange(plotlist = plot_ls,
                                                               ncol=1))
  return(average_fit_plt)
}
plot_cluster<- function(ds_tb,
                        cluster_int){
  colNames <- names(ds_tb)[21:26]
  ds_tb$cluster_int <- cluster_int

  plot_ls<-list()
  for(i in colNames){
    labelx <- eval(parse(text=paste0("attributes(ds_tb$",i,")$label")))
    labelx <- stringr::str_to_sentence(stringr::str_sub(labelx,38,55))
    plot_ls[[i]]<- ggplot2::ggplot(ds_tb, ggplot2::aes_string(i)) +
      ggplot2::geom_density(ggplot2::aes(fill = cluster_int,
                                         colour = cluster_int),
                            alpha = 0.4 ,
                            adjust = 2.5) +
      ggplot2::labs(x=labelx,y="Density") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position="none") +
      ggplot2::scale_fill_manual(values  = c("#fdcc8a","#fc8d59","#e34a33", "#b30000")) +
      ggplot2::scale_colour_manual(values  = c("#fdcc8a","#fc8d59","#e34a33", "#b30000"))
  }
  legend <- ggplot2::ggplot(ds_tb, ggplot2::aes_string(i)) +
    ggplot2::geom_density(ggplot2::aes(fill = cluster_int,
                                       colour = cluster_int),
                          alpha=0.4,
                          adjust=2.5) +
    ggplot2::labs(x=labelx,
                  y="Density",
                  fill="Class",
                  col="Class")  +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="bottom") +
    ggplot2::scale_fill_manual(values  = c("#fdcc8a","#fc8d59","#e34a33", "#b30000")) +
    ggplot2::scale_colour_manual(values  = c("#fdcc8a","#fc8d59","#e34a33", "#b30000"))
  legend <- youthvars::get_guide_box_lgd(legend)
  print(gridExtra::grid.arrange(ggpubr::ggarrange(plotlist = plot_ls,
                                                  nrow = 3,
                                                  ncol = 2) ,
                                legend,
                                nrow = 2,
                                heights=c(10, 1)))
}
plot_individual_fit <- function(cv_ds_tb,
                                clss_var_nm_1L_chr = "Classes",
                                fold_var_nm_1L_chr = "Fold",
                                legend_label_1L_chr = "Datasets"){
  nbr_of_clss_1L_dbl <- get_nbr_of_clss(cv_ds_tb,
                                        clss_var_nm_1L_chr = clss_var_nm_1L_chr)
  tfd_cvdn_ds_tb <- transform_cvdn_ds_for_ind_plts(cv_ds_tb,
                                               clss_var_nm_1L_chr = clss_var_nm_1L_chr,
                                               fold_var_nm_1L_chr = fold_var_nm_1L_chr)

  plot_ls <- list()
  plot_ls[[1]] <- make_individual_fit_plt(tfd_cvdn_ds_tb,
                                          nbr_of_clss_1L_dbl = nbr_of_clss_1L_dbl,
                                          statistic_var_nm_1L_chr = "AIC",
                                          clss_var_nm_1L_chr = clss_var_nm_1L_chr,
                                          fold_var_nm_1L_chr = fold_var_nm_1L_chr,
                                          maximise_1L_lgl = F,
                                          y_label_1L_chr = NA_character_)
  plot_ls[[2]] <- make_individual_fit_plt(tfd_cvdn_ds_tb,
                                          nbr_of_clss_1L_dbl = nbr_of_clss_1L_dbl,
                                          statistic_var_nm_1L_chr = "BIC",
                                          clss_var_nm_1L_chr = clss_var_nm_1L_chr,
                                          fold_var_nm_1L_chr = fold_var_nm_1L_chr,
                                          maximise_1L_lgl = F,
                                          y_label_1L_chr = NA_character_)
  plot_ls[[3]] <- make_individual_fit_plt(tfd_cvdn_ds_tb,
                                          nbr_of_clss_1L_dbl = nbr_of_clss_1L_dbl,
                                          statistic_var_nm_1L_chr = "logLik",
                                          clss_var_nm_1L_chr = clss_var_nm_1L_chr,
                                          fold_var_nm_1L_chr = fold_var_nm_1L_chr,
                                          maximise_1L_lgl = T,
                                          y_label_1L_chr = "Log-likelihood")
  legend <- ggplot2::ggplot(tfd_cvdn_ds_tb, # USE TTU MTHD
                            ggplot2::aes(x = !!rlang::sym(clss_var_nm_1L_chr),
                                         y= AIC,
                                         color=factor(!!rlang::sym(fold_var_nm_1L_chr)))) +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(col = legend_label_1L_chr)
  legend <- youthvars::get_guide_box_lgd(legend)
  individual_fit_plt <- gridExtra::grid.arrange(ggpubr::ggarrange(plotlist = plot_ls,
                                                                  ncol=1),
                                                legend,
                                                nrow = 2,
                                                heights = c(10, 1))
  return(individual_fit_plt)
}
plot_pca <- function(ds_tb,
                     pca_df,
                     class_var_nm_1L_chr = "class_int"){
  ggplot2::ggplot(pca_df,
                  ggplot2::aes(x = PC1,
                               y = PC2,
                               colour = !!rlang::sym(class_var_nm_1L_chr))) +
    ggplot2::geom_jitter(alpha = 0.3) +
    ggplot2::theme_bw()
}
