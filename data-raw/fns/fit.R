fit_mixture_mdl<-function(ds_tb,
                          clusters_1L_int,
                          var_nms_chr){
  model_def <- depmixS4::mix(purrr::map(var_nms_chr,
                                        ~ eval(parse(text = paste0(.x," ~ 1")))),
                             family = rep(list(depmixS4::multinomial("identity")), length(var_nms_chr)),
                             data = ds_tb,
                             nstates = clusters_1L_int,
                             nstart = rep(1/clusters_1L_int,clusters_1L_int))
  model_mdl <- depmixS4::fit(model_def)
  return(model_mdl)
}
fit_mixture_mdl_clusters <- function(ds_tb,
                                     nbr_clss_1L_int = 15L,
                                     var_nms_chr){
  return_df <- data.frame(
    Classes = integer(),
    AIC = double(),
    BIC = double(),
    logLik = double(),
    Par = double())
  return_ds_tb <- ds_tb %>% dplyr::select(ID)
  for(n in 2:nbr_clss_1L_int){
    model_mdl <- fit_mixture_mdl(ds_tb,
                                 classes_1L_int = n,
                                 var_nms_chr = var_nms_chr)
    return_df <- rbind(return_df,
                       data.frame(Classes = n,
                                  AIC = stats:AIC(model_mdl),
                                  BIC = stats::BIC(model_mdl),
                                  logLik = stats::logLik(model_mdl),
                                  Par = model_mdl@npars))
    return_ds_tb[[paste0("n",n)]] <- apply(model_mdl@posterior %>%
                                             dplyr::select(-state),
                                           1,
                                           which.max)
  }
  mdl_smry_ls <- list(return_df,return_ds_tb)
  return(mdl_smry_ls)
}
