
# Within shiny both session and input variables will exist, 
# this creates examples here for testing purposes:
sess_res = DW_test_mksession(session=list())
session = sess_res$session
input   = sess_res$input

# For this example we also need a state variable
state = sess_res$state

# This sets a hold on the specified inputID. This is normally done in 
# your XX_fetch_state() function.
state = set_hold(state, inputId = "select_dw_views")

# This will remove the hold and is normally done in one of the UI outputs
# with a priority set to ensure it happens after the rest of the UI has
# refreshed.
state = remove_hold(state, session, inputId = "select_dw_views")
