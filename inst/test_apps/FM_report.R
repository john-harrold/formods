# Within shiny both session and input variables will exist,
# this creates examples here for testing purposes:
sess_res = FG_test_mksession(session=list())
session = sess_res$session
input   = sess_res$input

# This will create a populated FG state object:
state   = sess_res$state


# This is the directory to write the report:
file_dir = tempdir()

# This is the file name that determines the type of report to write:
file_name = "my_report.pptx"

rpt_res =
FM_generate_report(state         = state,
                   session       = session,
                   file_dir      = file_dir,
                   file_name     = file_name,
                   gen_code_only = TRUE,
                   rpterrors     = TRUE)

# This contains the exit status of the report generation
rpt_res$isgood

# This is the underlying code that was used to generate the report
cat(paste0(rpt_res$code, collapse="\n"))
