library(ready4use)
x <- ready4use::Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                               gh_tag_1L_chr = "Documentation_0.0")
y <- ingest(x)
abbreviations_lup <- procure(procureSlot(y,
                                     "b_Ready4useIngest"),
                         "abbreviations_lup")
abbreviations_lup <- abbreviations_lup %>% dplyr::filter(!startsWith(short_name_chr,"Aus"))
abbreviations_lup <-  abbreviations_lup %>%
#  dplyr::filter(!short_name_chr  %in% c("vicinity_template","VicinityLocalpRaw"))
  renew(short_name_chr = c("aus", "erp", "ppr", "sa2", "sa3", "sfs"),
        long_name_chr = c("Australia", "Estimated Resident Population", "population projection", "Statistical Area 2", "Statistical Area 3", "simple features objects"),
        plural_lgl = c(F, F, F, F, F, T)) %>%
  dplyr::arrange(short_name_chr)
# treat_as_words_chr <-  y@b_Ready4useIngest@objects_ls$treat_as_words_chr %>%
#   c("isochrone", "isochrones", "polyline") %>% sort()

  # dplyr::filter(!startsWith(short_name_chr,"Vicinity"))
  # dplyr::mutate(short_name_chr = dplyr::case_when(short_name_chr == "RI" ~ "ri",
  #                                                 short_name_chr == "RIs" ~ "ris",
  #                                                 T ~ short_name_chr)) %>%
  # dplyr::mutate(long_name_chr = dplyr::case_when(long_name_chr == "Rand Indexs" ~ "Rand Indices",
  #                                                T ~ long_name_chr))
y <- renewSlot(y,
               new_val_xx = Ready4useIngest(objects_ls = list(abbreviations_lup = abbreviations_lup#,treat_as_words_chr = treat_as_words_chr
                                                              )),
               slot_nm_1L_chr = "b_Ready4useIngest")
# prototype_lup <-  procure(procureSlot(y,
#                                       "b_Ready4useIngest"),
#                           "prototype_lup")
# prototype_lup <- dplyr::filter(prototype_lup,
#                                !type_chr %in% c("VicinityLocalpRaw","vicinity_template"))
# y <- renewSlot(y,
#                new_val_xx = Ready4useIngest(objects_ls = list(prototype_lup = prototype_lup)),
#                slot_nm_1L_chr = "b_Ready4useIngest")
y <- share(y,
           type_1L_chr = "prefer_gh")

# ready4::write_env_objs_to_dv(list(abbreviations_lup = abbreviations_lup),
#                             descriptions_chr = "Abbreviations lookup table",
#                             ds_url_1L_chr = character(0),
#                             piggyback_to_1L_chr = "ready4-dev/ready4",
#                             publish_dv_1L_lgl = F)
z$x_ready4fun_manifest <- renew.ready4fun_manifest(z$x_ready4fun_manifest,
                                                        tf_to_singular_chr = c(kmean = "kmeans"),
                                                        type_1L_chr = "abbreviations",
                                                        long_name_chr = c("cross-validation",
                                                                          "individual",
                                                                          "k-means clustering",#,
                                                                          "principal component analysis",
                                                                          "Rand Index"))
