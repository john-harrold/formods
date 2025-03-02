# Within shiny both session and input variables will exist,
# this creates examples here for testing purposes:
sess_res = ===ZZ===_test_mksession(session=list())
state   = sess_res$state
session = sess_res$session
input   = sess_res$input

# Creates a new empty element
state = ===ZZ===_new_element(state)

# Delete the current element
state = ===ZZ===_del_current_element(state)

# Fetch a list of the current element
element = ===ZZ===_fetch_current_element(state)

# You can modify the element
element[["ui"]][["element_name"]] = "A more descriptive name"

# You can now place element back in the state
state = ===ZZ===_set_current_element(state, element)

# This will pull the portion of the code associated with this module.
code = ===ZZ===_fetch_code(state)

cat(code)

# This forces and update of the module checksum
state = ===ZZ===_update_checksum(state)
