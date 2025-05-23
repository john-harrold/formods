% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DW_Server.R
\name{DW_update_checksum}
\alias{DW_update_checksum}
\title{Updates DW Module Checksum}
\usage{
DW_update_checksum(state)
}
\arguments{
\item{state}{DW state from \code{DW_fetch_state()}}
}
\value{
DW state object with the checksum updated
}
\description{
Takes a DW state and updates the checksum used to trigger
downstream updates
}
\examples{
# Within shiny both session and input variables will exist,
# this creates examples here for testing purposes:
sess_res = DW_test_mksession()
session = sess_res$session
input   = sess_res$input

# We also need a state variable
state = sess_res$state

state = DW_update_checksum(state)
}
