% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_ri_mat}
\alias{make_ri_mat}
\title{Makeand Index matrix}
\usage{
make_ri_mat(cvdn_ds_ls, ds_tb, id_var_nm_1L_chr = "ID")
}
\arguments{
\item{cvdn_ds_ls}{Cross-validation dataset (a list)}

\item{ds_tb}{Dataset (a tibble)}

\item{id_var_nm_1L_chr}{Identity variable name (a character vector of length one), Default: 'ID'}
}
\value{
a Rand Index (a matrix)
}
\description{
make_ri_mat() is a Make function that creates a new R object. Specifically, this function implements an algorithm to makeand index matrix. The function returns a Rand Index (a matrix).
}
\keyword{internal}
