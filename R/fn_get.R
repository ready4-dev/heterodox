#' Get fake Assessment of Quality of Life Six Dimension dataset
#' @description get_fake_aqol6d_ds() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get fake assessment of quality of life six dimension dataset. The function returns Dataset (a tibble).

#' @return Dataset (a tibble)
#' @rdname get_fake_aqol6d_ds
#' @export 
#' @importFrom ready4use Ready4useRepos
#' @importFrom dplyr rename_with rename filter
#' @importFrom stringr str_replace
get_fake_aqol6d_ds <- function () 
{
    ds_tb <- ready4use::Ready4useRepos(dv_nm_1L_chr = "fakes", 
        dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/W95KED", 
        dv_server_1L_chr = "dataverse.harvard.edu") %>% ingest(fls_to_ingest_chr = "ymh_clinical_dyad_r4") %>% 
        procureSlot("b_Ready4useIngest") %>% procure("ymh_clinical_dyad_r4") %>% 
        procureSlot(slot_nm_1L_chr = "ds_tb") %>% dplyr::rename_with(~stringr::str_replace(., 
        "aqol6d_q", "Q"), .cols = everything()) %>% dplyr::rename(ID = fkClientID) %>% 
        dplyr::filter(round == "Baseline")
    return(ds_tb)
}
#' Get number of classes
#' @description get_nbr_of_clss() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get number of classes. Function argument cvdn_ds_tb specifies the where to look for the required object. The function returns Number of classes (a double vector of length one).
#' @param cvdn_ds_tb Cross-validation dataset (a tibble)
#' @param clss_var_nm_1L_chr Classes variable name (a character vector of length one), Default: 'Classes'
#' @return Number of classes (a double vector of length one)
#' @rdname get_nbr_of_clss
#' @export 
#' @importFrom dplyr pull
#' @importFrom rlang sym
#' @keywords internal
get_nbr_of_clss <- function (cvdn_ds_tb, clss_var_nm_1L_chr = "Classes") 
{
    nbr_of_clss_1L_dbl <- cvdn_ds_tb %>% dplyr::pull(!!rlang::sym(clss_var_nm_1L_chr)) %>% 
        unique() %>% length() + 1
    return(nbr_of_clss_1L_dbl)
}
