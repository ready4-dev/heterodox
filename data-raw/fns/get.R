get_fake_aqol6d_ds <- function(){
  ds_tb <- ready4use::Ready4useRepos(dv_nm_1L_chr = "fakes",
                                     dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/W95KED",
                                     dv_server_1L_chr = "dataverse.harvard.edu") %>%
    ingest(fls_to_ingest_chr = "ymh_clinical_dyad_r4") %>%
    procureSlot("b_Ready4useIngest") %>%
    procure("ymh_clinical_dyad_r4") %>%
    procureSlot(slot_nm_1L_chr = "ds_tb") %>%
    dplyr::rename_with(~stringr::str_replace(.,
                                             "aqol6d_q",
                                             "Q"), .cols = everything()) %>%
    dplyr::rename(ID = fkClientID) %>%
    dplyr::filter(round == "Baseline")
  return(ds_tb)
}
get_nbr_of_clss <- function(cvdn_ds_tb,
                            clss_var_nm_1L_chr = "Classes"){
  nbr_of_clss_1L_dbl <- cvdn_ds_tb %>%
    dplyr::pull(!!rlang::sym(clss_var_nm_1L_chr)) %>% #
    unique() %>%
    length() + 1
  return(nbr_of_clss_1L_dbl)
}
