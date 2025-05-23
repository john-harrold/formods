% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UD_Server.R
\name{UD_fetch_state}
\alias{UD_fetch_state}
\title{Fetch Upload Data State}
\usage{
UD_fetch_state(id, input, session, FM_yaml_file, MOD_yaml_file)
}
\arguments{
\item{id}{Shiny module ID}

\item{input}{Shiny input variable}

\item{session}{Shiny session variable}

\item{FM_yaml_file}{App configuration file with FM as main section.}

\item{MOD_yaml_file}{Module configuration file with MC as main section.}
}
\value{
list containing the current state of the app including default
values from the yaml file as well as any changes made by the user. The list
has the following structure:
\itemize{
\item{yaml:} Full contents of the supplied yaml file.
\item{MC:} Module components of the yaml file.
\item{DS:} Loaded dataset with the following elements
\itemize{
  \item{isgood:} Boolean object indicating if the file was successfully loaded.
  \item{load_msg:} Text message indicated the success or any problems
  encountered when uploading the file.
  \item{data_file_local:} Full path to the data file on the server.
  \item{data_file:} Dataset file name without the path.
  \item{data_file_ext:} File extension of the uploaded file.
  \item{sheet:} If the uploaded file is an excel file, this is the
  currently selected sheet.
  \item{sheets:} If the uploaded file is an excel file, this is a character vector of the sheets present in that file.
  \item{contents:} Data frame containting the contents of the data file.
  \item{checksum:} This is an MD5 sum of the contents element and can be
  used to detect changes in the loaded file.
}
 \item{MOD_TYPE:} Character data containing the type of module \code{"UD"}
 \item{id:} Character data containing the module id module in the session variable.
 \item{FM_yaml_file:} App configuration file with FM as main section.
 \item{MOD_yaml_file:}  Module configuration file with MC as main section.
}
}
\description{
Merges default app options with the changes made in the UI
}
\examples{
# YAML configuration files from the package:
FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
MOD_yaml_file = system.file(package = "formods", "templates", "UD.yaml")
# This is the module id:
id = "UD"
# Within shiny both session and input variables will exist,
# this creates examples here for testing purposes:
sess_res = UD_test_mksession(session=list())
session = sess_res$session
input   = sess_res$input
state = UD_fetch_state(
           id            = id,
           input         = input,
           session       = session,
           FM_yaml_file  = FM_yaml_file,
           MOD_yaml_file = MOD_yaml_file )
}
