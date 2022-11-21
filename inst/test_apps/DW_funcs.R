library(formods)
# The example requires a formods DW state object
state  = DW_test_mksession(session=list())$state
state[["DW"]][["ui"]][["select_dw_element"]]          = "filter"
state[["DW"]][["ui"]][["select_fds_filter_column"]]   = "EVID"
state[["DW"]][["ui"]][["select_fds_filter_operator"]] = "=="
state[["DW"]][["ui"]][["fds_filter_rhs"]]             = 0

# This builds the data wrangling statement based on
# elemets scraped from the UI
dwb_res  = dwrs_builder(state)

# Here we evaluate the resulting command:
dwee_res = dw_eval_element(state, dwb_res[["cmd"]])

# Next we add this wrangling element to the state
state    = DW_add_wrangling_element(state, dwb_res, dwee_res)

# This creates a new data view and makes it active
state = DW_new_view(state)

# Here we can pluck out that data view from the state
current_view = DW_fetch_current_view(state)

# This will update the key in this view
current_view[["key"]] = "My new view"

# And this will place it back into the state
state = DW_set_current_view(state, current_view)
