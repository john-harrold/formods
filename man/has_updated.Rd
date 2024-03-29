% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formods.R
\name{has_updated}
\alias{has_updated}
\title{Detect if a UI element has updated}
\usage{
has_updated(ui_val = NULL, old_val = NULL)
}
\arguments{
\item{ui_val}{Current value from the UI.}

\item{old_val}{Last value of of the element.
defined.}
}
\value{
Boolean result of the comparison
}
\description{
Takes a UI element value and an older value and determines if
it has been modified
}
\examples{
changed_true  = has_updated(ui_val = "a", old_val = "")
changed_true
changed_false = has_updated(ui_val = "a", old_val = "a")
changed_false
}
