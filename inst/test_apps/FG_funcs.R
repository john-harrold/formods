\donttest{
library(formods)
# Within shiny both session and input variables will exist,
# this creates examples here for testing purposes:
sess_res = FG_test_mksession(session=list())
session = sess_res$session
input   = sess_res$input

# This will create a populated FG state object:
state   = sess_res$state

# This sets the current active figure to Fig_1
state[["FG"]][["current_fig"]]  =  "Fig_1"

# If this is a paginated figure, and we can access a specific
pg_1 = FG_extract_page(state, 1)

# This will give you access to the current figure directly:
current_fig = FG_fetch_current_fig(state)

# For example this will set the key for that figure:
current_fig$key = "Individual profiles by cohort (multiple pages)"

# Once you're done you can put it back into the state:
state = FG_set_current_fig(state, current_fig)

# If you made any changes to the actual figure, this will
# force a rebuild of the current figure:
state = FG_build( state=state, del_row = NULL, cmd = NULL)

# To create a new empty figure you can do this:
state = FG_new_fig(state)
}
