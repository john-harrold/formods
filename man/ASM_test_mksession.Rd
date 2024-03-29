% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ASM_Server.R
\name{ASM_test_mksession}
\alias{ASM_test_mksession}
\title{Populate Session Data for Module Testing}
\usage{
ASM_test_mksession(
  session,
  id = "ASM",
  id_UD = "UD",
  id_DW = "DW",
  full_session = TRUE
)
}
\arguments{
\item{session}{Shiny session variable (in app) or a list (outside of app)}

\item{id}{An ID string that corresponds with the ID used to call the modules UI elements}

\item{id_UD}{An ID string that corresponds with the ID used to call the UD modules UI elements}

\item{id_DW}{An ID string that corresponds with the ID used to call the DW modules UI elements}

\item{full_session}{Boolean to indicate if the full test session should be created (default \code{TRUE}).}
}
\value{
list with the following elements
\itemize{
  \item{isgood:} Boolean indicating the exit status of the function.
  \item{session:} The value Shiny session variable (in app) or a list (outside of app) after initialization.
  \item{input:} The value of the shiny input at the end of the session initialization.
  \item{state:} App state.
  \item{rsc:} The \code{react_state} components.
}
}
\description{
Populates the supplied session variable for testing.
}
\examples{
sess_res = ASM_test_mksession(session=list(), full_session=FALSE)
}
