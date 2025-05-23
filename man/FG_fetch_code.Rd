% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FG_Server.R
\name{FG_fetch_code}
\alias{FG_fetch_code}
\title{Fetch Module Code}
\usage{
FG_fetch_code(state)
}
\arguments{
\item{state}{UD state from \code{FG_fetch_state()}}
}
\value{
Character object vector with the lines of code
}
\description{
Fetches the code to generate results seen in the app
}
\examples{
\donttest{
# This will create a populated FG state object:
sess_res = FG_test_mksession()
state   = sess_res$state
code  = FG_fetch_code(state)
cat(paste(code, collapse="\n"))
}
}
