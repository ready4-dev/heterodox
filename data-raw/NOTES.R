library(ready4fun)
z$x_ready4fun_manifest <- renew.ready4fun_manifest(z$x_ready4fun_manifest,
                                                        tf_to_singular_chr = c(kmean = "kmeans"),
                                                        type_1L_chr = "abbreviations",
                                                        long_name_chr = c("cross-validation",
                                                                          "individual",
                                                                          "k-means clustering",#,
                                                                          "principal component analysis",
                                                                          "Rand Index"))
