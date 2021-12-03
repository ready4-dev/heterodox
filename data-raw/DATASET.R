library(youthvars)
fns_env_ls <- ready4fun::read_fns(c("data-raw/fns/","data-raw/mthds/"),
                                  fns_env = new.env(parent = globalenv()))
x <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Explore and Characterise Heterogeneity in Quality of Life Data",
                                 pkg_desc_1L_chr = "Tools to explore heterogeneity in quality of life data.
  This development version of the heterodox package has been made available as part of the process of testing and documenting the package.
                            If you have any questions, please contact the authors.",
                                 authors_prsn = c(utils::person(given = "Caroline",family = "Gao",email = "caroline.gao@orygen.org.au", role = c("aut"),comment = c(ORCID = "0000-0002-0987-2759")),
                                                  utils::person(given = "Matthew", family = "Hamilton",email = "matthew.hamilton@orygen.org.au", role = c("aut", "cre"),comment = c(ORCID = "0000-0001-7407-9194")),
                                                  utils::person("Orygen", role = c("cph", "fnd")),
                                                  utils::person("Headspace", role = c( "fnd")),
                                                  utils::person("National Health and Medical Research Council", role = c( "fnd"))),
                                 urls_chr = c("https://ready4-dev.github.io/heterodox/",
                                              "https://github.com/ready4-dev/heterodox",
                                              "https://ready4-dev.github.io/ready4/")) %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(depends_chr = c("ready4","depmixS4"),
                                                                       suggests_chr = "rmarkdown",
                                                                       imports_chr = "knitrBootstrap"),
                           build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
                           check_type_1L_chr = "ready4",
                           copyright_holders_chr = "Orygen",
                           custom_dmt_ls = ready4fun::make_custom_dmt_ls(user_manual_fns_chr = c("add_class_var",
                                                                                                 "add_kmean_cls_var",
                                                                                                 "calculate_mean_ri",
                                                                                                 "fit_mix",
                                                                                                 "get_fake_aqol6d_ds",
                                                                                                 "make_cvdn_ds",
                                                                                                 "make_cvdn_ls",
                                                                                                 "make_pca_tbl",
                                                                                                 "plot_average_fit",
                                                                                                 "plot_cluster",
                                                                                                 "plot_individual_fit",
                                                                                                 "plot_pca",
                                                                                                 "print_cluster_plots")),##
                           dev_pkgs_chr = c("ready4","youthvars"),
                           lifecycle_stage_1L_chr = "experimental",
                           path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/heterodox-logo/default.png",
                           piggyback_to_1L_chr = "ready4-dev/ready4",
                           ready4_type_1L_chr = "modelling",
                           zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5751193.svg)](https://doi.org/10.5281/zenodo.5751193)")
z <- ready4pack::make_pt_ready4pack_manifest(x) %>%
  ready4pack::ready4pack_manifest()
z <- ready4::author(z)
