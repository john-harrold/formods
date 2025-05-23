% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DW_Server.R
\name{DW_fetch_ds}
\alias{DW_fetch_ds}
\title{Fetch Module Datasets}
\usage{
DW_fetch_ds(state, meta_only = FALSE)
}
\arguments{
\item{state}{UD state from \code{UD_fetch_state()}}

\item{meta_only}{Include only metadata and not the dataset (default \code{FALSE})}
}
\value{
Character object vector with the lines of code

list containing the following elements
\itemize{
 \item{isgood:}    Return status of the function.
 \item{hasds:}     Boolean indicator if the module has any datasets
 \item{msgs:}      Messages to be passed back to the user.
 \item{ds:}        List with datasets. Each list element has the name of
 the R-object for that dataset. Each element has the following structure:
 \itemize{
   \item{label: Text label for the dataset}
   \item{MOD_TYPE: Short name for the type of module.}
   \item{id: module ID}
   \item{idx: unique numerical ID to identify this dataset in the module.}
   \item{res_label: optional label that can be defined by a user and used in
   workflows. Must be unique to the module.}
   \item{DS: Dataframe containing the actual dataset.}
   \item{DSMETA: Metadata describing DS, see \code{FM_fetch_ds()} for
   details on the format.}
   \item{code: Complete code to build dataset.}
   \item{checksum: Module checksum.}
   \item{DSchecksum: Dataset checksum.}
 }
}
}
\description{
Fetches the datasets contained in the module.
}
\examples{
# We need a state variable
sess_res = DW_test_mksession()
state = sess_res$state

ds = DW_fetch_ds(state)
}
