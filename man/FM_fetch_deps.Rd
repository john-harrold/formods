% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formods.R
\name{FM_fetch_deps}
\alias{FM_fetch_deps}
\title{Fetches Dependency Information}
\usage{
FM_fetch_deps(state, session)
}
\arguments{
\item{state}{Current module state after yaml file has been read}

\item{session}{Shiny session variable}
}
\value{
list with the following elements:
\itemize{
\item{mod_ids}  Dependent module ids.
\item{packages}     List of package dependencies.
\item{package_code} Library commands to load packages.
}
}
\description{
For a given state and session this function will determine the
module ids that are dependent as well as any packages the module elements
might depend on.
}
\examples{
# We need a Shiny session object to use this function:
sess_res = UD_test_mksession()
session  = sess_res$session
state    = sess_res$state
mod_deps = FM_fetch_deps(state, session)
}
