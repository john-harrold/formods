% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formods.R
\name{FM_fetch_user_files_path}
\alias{FM_fetch_user_files_path}
\title{Fetches the Path to the User Files}
\usage{
FM_fetch_user_files_path(state)
}
\arguments{
\item{state}{module state after yaml read}
}
\value{
Character string with the path to the log file.
}
\description{
Use this to get the path to the temporary directory where formods stores user files.
}
\examples{
# We need a state object to use this function:
sess_res = UD_test_mksession()
state = sess_res$state
user_dir = FM_fetch_user_files_path(state)
user_dir
}
