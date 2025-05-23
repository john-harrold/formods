% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UD_Server.R
\name{UD_fetch_ds}
\alias{UD_fetch_ds}
\title{Fetch Module Datasets}
\usage{
UD_fetch_ds(state, meta_only = FALSE)
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
# YAML configuration files from the package:
FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
MOD_yaml_file = system.file(package = "formods", "templates", "UD.yaml")
# This is the module id:
id = "UD"
# Within shiny both session and input variables will exist,
# this creates examples here for testing purposes:
sess_res = UD_test_mksession(session=list())
session = sess_res$session
input   = sess_res$input
state = UD_fetch_state(
           id            = id,
           input         = input,
           session       = session,
           FM_yaml_file  = FM_yaml_file,
           MOD_yaml_file = MOD_yaml_file )

 ds_res = UD_fetch_ds(state)
}
