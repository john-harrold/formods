% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ASM_Server.R
\name{ASM_load_state}
\alias{ASM_load_state}
\title{Write State to File for Saving}
\usage{
ASM_load_state(state, session, file_path)
}
\arguments{
\item{state}{ASM state from \code{ASM_fetch_state()}}

\item{session}{Shiny session variable}

\item{file_path}{Zip file with the saved sate}
}
\value{
This function only writes the state and returns a list with the
following elements:
\itemize{
  \item{isgood:}      Boolean indicating the exit status of the function.
  \item{msgs:}        Messages to be passed back to the user.
  \item{state:}       AMS state
  \item{session:}     Session object
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
               file_path = ssf)

ls_res =
ASM_load_state(state, session,
               file_path = ssf)
}
