# Within shiny both session and input variables will exist,
# this creates examples here for testing purposes:
sess_res = DM_test_mksession(session=list())
state   = sess_res$state
session = sess_res$session
input   = sess_res$input

# Creates a new empty element
state = DM_new_element(state)

# Delete the current element
state = DM_del_current_element(state)

# Fetch a list of the current element
element = DM_fetch_current_element(state)

# You can modify the element
element[["name"]] = "A more descriptive name"

# You can now place element back in the state
state = DM_set_current_element(state, element)

# This will pull the portion of the code associated with this module.
code = DM_fetch_code(state)

cat(code)

# This forces and update of the module checksum
state = DM_update_checksum(state)
