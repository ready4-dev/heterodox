#' Fit mixture model
#' @description fit_mixture_mdl() is a Fit function that fits a model of a specified type to a dataset Specifically, this function implements an algorithm to fit mixture model. The function returns Model (a model).
#' @param ds_tb Dataset (a tibble)
#' @param clusters_1L_int Clusters (an integer vector of length one)
#' @param var_nms_chr Variable names (a character vector)
#' @return Model (a model)
#' @rdname fit_mixture_mdl
#' @export 
#' @importFrom depmixS4 mix multinomial fit
#' @importFrom purrr map
#' @keywords internal
fit_mixture_mdl <- function (ds_tb, clusters_1L_int, var_nms_chr) 
{
    model_def <- depmixS4::mix(purrr::map(var_nms_chr, ~eval(parse(text = paste0(.x, 
        " ~ 1")))), family = rep(list(depmixS4::multinomial("identity")), 
        length(var_nms_chr)), data = ds_tb, nstates = clusters_1L_int, 
        nstart = rep(1/clusters_1L_int, clusters_1L_int))
    model_mdl <- depmixS4::fit(model_def)
    return(model_mdl)
}
#' Fit mixture model clusters
#' @description fit_mixture_mdl_clusters() is a Fit function that fits a model of a specified type to a dataset Specifically, this function implements an algorithm to fit mixture model clusters. The function returns Model summary (a list).
#' @param ds_tb Dataset (a tibble)
#' @param nbr_clss_1L_int Number classes (an integer vector of length one), Default: 15
#' @param var_nms_chr Variable names (a character vector)
#' @return Model summary (a list)
#' @rdname fit_mixture_mdl_clusters
#' @export 
#' @importFrom dplyr select
#' @importFrom stats BIC logLik
#' @keywords internal
fit_mixture_mdl_clusters <- function (ds_tb, nbr_clss_1L_int = 15L, var_nms_chr) 
{
    return_df <- data.frame(Classes = integer(), AIC = double(), 
        BIC = double(), logLik = double(), Par = double())
    return_ds_tb <- ds_tb %>% dplyr::select(ID)
    for (n in 2:nbr_clss_1L_int) {
        model_mdl <- fit_mixture_mdl(ds_tb, classes_1L_int = n, 
            var_nms_chr = var_nms_chr)
        return_df <- rbind(return_df, data.frame(Classes = n, 
            AIC = stats:AIC(model_mdl), BIC = stats::BIC(model_mdl), 
            logLik = stats::logLik(model_mdl), Par = model_mdl@npars))
        return_ds_tb[[paste0("n", n)]] <- apply(model_mdl@posterior %>% 
            dplyr::select(-state), 1, which.max)
    }
    mdl_smry_ls <- list(return_df, return_ds_tb)
    return(mdl_smry_ls)
}
