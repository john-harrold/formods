% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DM_Server.R
\name{is_excel}
\alias{is_excel}
\title{Determines if a File is Excel}
\usage{
is_excel(file_name)
}
\arguments{
\item{file_name}{Name of file to check.}
}
\value{
Logical indicating if a file is an excel file or not.
}
\description{
Check to see if a file is an excel file
}
\examples{
is_excel("file.xls")
is_excel("file.xlsx")
is_excel("file.csv")
}
