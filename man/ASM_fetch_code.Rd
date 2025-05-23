% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ASM_Server.R
\name{ASM_fetch_code}
\alias{ASM_fetch_code}
\title{Fetch Module Code}
\usage{
ASM_fetch_code(state)
}
\arguments{
\item{state}{ASM state from \code{ASM_fetch_state()}}
}
\value{
The ASM module does not generate code
}
\description{
Fetches the code to generate results seen in the app
}
\examples{
# Creating a state object for testing
sess_res = ASM_test_mksession()
state = sess_res$state
code = ASM_fetch_code(state)
}
