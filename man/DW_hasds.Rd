% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DW_Server.R
\name{DW_hasds}
\alias{DW_hasds}
\title{Check DW State For Datasets}
\usage{
DW_hasds(state)
}
\arguments{
\item{state}{DW state from \code{DW_fetch_state()}}
}
\value{
Logical TRUE if there is a dataset or FALSE otherwise.
}
\description{
Walks through the DW state object to see if there are any
datasets available
}
\examples{
sess_res = DW_test_mksession()
state = sess_res[["state"]]
DW_hasds(state)
}
