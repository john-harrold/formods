% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formods.R
\name{FM_app_preload}
\alias{FM_app_preload}
\title{Preload Data Into App}
\usage{
FM_app_preload(
  session,
  sources = NULL,
  react_state = list(),
  quickload = FALSE
)
}
\arguments{
\item{session}{Shiny session variable (in app) or a list (outside of app)}

\item{sources}{Vector of at corresponds with the ID used to call the modules UI elements}

\item{react_state}{Reactive shiny object (in app) or a list (outside of app) used to trigger reactions}

\item{quickload}{Logical \code{TRUE} to load reduced analysis \code{FALSE} to load the full analysis}
}
\value{
list with the following elements
\itemize{
  \item{isgood:}       Boolean indicating the exit status of the function.
  \item{msgs:}         Messages to be passed back to the user.
  \item{all_sess_res:} List containing the result for each module stored in
  the list name with the module ID.
  \item{session:} Returning the session variable to be used in scripting (not in app).
}
}
\description{
Populates session data for testing or to load a specific
analysis.
}
\examples{
sources=c(system.file(package="formods", "preload", "ASM_preload.yaml"),
          system.file(package="formods", "preload", "UD_preload.yaml"))
res = FM_app_preload(session=list(), sources=sources)
}
