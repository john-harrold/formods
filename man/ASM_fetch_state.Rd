% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ASM_Server.R
\name{ASM_fetch_state}
\alias{ASM_fetch_state}
\title{Fetch State Manager State}
\usage{
ASM_fetch_state(id, input, session, FM_yaml_file, MOD_yaml_file)
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
\item{ASM:}
\itemize{
  \item{isgood:} Boolean object indicating if the file was successfully loaded.
  \item{checksum:} This is an MD5 sum of the loaded state file
}
 \item{MOD_TYPE:} Character data containing the type of module \code{"ASM"}
 \item{id:} Character data containing the module id module in the session variable.
 \item{FM_yaml_file:} App configuration file with FM as main section.
 \item{MOD_yaml_file:}  Module configuration file with MC as main section.
}
}
\description{
Merges default app options with the changes made in the UI
}
\examples{
# Within shiny both session and input variables will exist,
# this creates examples here for testing purposes:
sess_res = ASM_test_mksession()
session = sess_res$session
input   = sess_res$input

# Configuration files
FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
MOD_yaml_file = system.file(package = "formods", "templates", "ASM.yaml")

# We need to specify the ID of the ASM module
id = "ASM"

state = ASM_fetch_state(id           = id,
                        input        = input,
                        session      = session,
                        FM_yaml_file = FM_yaml_file,
                        MOD_yaml_file = MOD_yaml_file)

state
}
