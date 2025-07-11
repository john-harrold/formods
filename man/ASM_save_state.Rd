% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ASM_Server.R
\name{ASM_save_state}
\alias{ASM_save_state}
\title{Write State to File for Saving}
\usage{
ASM_save_state(state, session, file_path, pll = NULL, update_modal = TRUE)
}
\arguments{
\item{state}{ASM state from \code{ASM_fetch_state()}}

\item{session}{Shiny session variable}

\item{file_path}{File path to write zipped state}

\item{pll}{Preload list of the format generated by \code{FM_mk_app_preload()}. IF set to \code{NULL} it will be generated from the contents of the session variable.}

\item{update_modal}{Logical controlling updates to modal messages (\code{TRUE})}
}
\value{
This function only writes the state and returns a list with the
following elements:
\itemize{
  \item{isgood:}      Boolean indicating the exit status of the function.
  \item{msgs:}        Messages to be passed back to the user.
}
}
\description{
Called from download handler and used to write a saved state
value if that is null
}
\examples{
# Within shiny the session variable will exist
# this creates examples here for testing purposes:
sess_res = ASM_test_mksession()
session = sess_res$session
state   = sess_res$state

ssf  = tempfile(fileext=".zip")

ss_res =
ASM_save_state(state, session,
               file_path  = ssf)
}
