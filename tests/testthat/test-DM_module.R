sess_res = suppressMessages(DM_test_mksession(session=list()))

state   = sess_res$state
session = sess_res$session

test_that("DM -- Fetch state", {
  # Test data:
  expect_true(state$DM$isgood)
  expect_true(sess_res$isgood)
})

# Deleting the defined elements:
ele_ids =  names(state[["DM"]][["elements"]])
while(state[["DM"]][["current_element"]] %in% ele_ids){
  state =  suppressMessages(DM_del_current_element(state))
}

test_that("DM -- Removing all sources and starting from scratch", {
  # This will remove all pre-defined sources
  sources_df = state[["DM"]][["defined_sources"]]
  source_ids = sources_df$ID
  for(source_id in source_ids){
    state = suppressMessages(DM_delete_source(state = state, id = source_id))
    expect_true(state[["DM"]][["res"]][["del_file"]][["isgood"]])
  }
  # Adding a file
  state = suppressMessages(
    DM_add_file(
      state     = state,
      file_name = "TEST_DATA.xlsx",
      file_path = system.file(package="formods", "test_data", "TEST_DATA.xlsx"),
      load_ID   = 10)
  )
  expect_true(state[["DM"]][["res"]][["add_file"]][["isgood"]])

  # Adding a url
  state = suppressMessages(
    DM_add_url(
      state     = state,
      url       = 'https://raw.githubusercontent.com/john-harrold/formods/refs/heads/master/inst/test_data/SDTM_DM.csv',
      load_ID   = 11)
  )
  expect_true(state[["DM"]][["res"]][["add_url"]][["isgood"]])

  # Testing dataset from file:
  current_ele  = DM_fetch_current_element(state)
  current_ele[["ui"]][["source_id"]] = "10"
  current_ele[["ui"]][["ds_sheet"]]  = "DATA"
  fsres = DM_fetch_source(state=state, element=current_ele)
  expect_true(fsres[["isgood"]])

  current_ele = DM_update_element_code(state = state, element=current_ele, session)
  expect_true(current_ele[["res"]][["code_update"]][["isgood"]])

  current_ele = DM_run_code(state = state, element=current_ele)
  expect_true(current_ele[["res"]][["run_code"]][["isgood"]])

  state = suppressMessages(
    DM_set_current_element(
      state   = state,
      element = current_ele)
  )

  # New datasete and attaching a url:
  state = DM_new_element(state)

  current_ele  = DM_fetch_current_element(state)
  current_ele[["ui"]][["source_id"]] = "11"
  fsres = DM_fetch_source(state=state, element=current_ele)
  expect_true(fsres[["isgood"]])

  current_ele = DM_update_element_code(state = state, element=current_ele, session)
  expect_true(current_ele[["res"]][["code_update"]][["isgood"]])

  current_ele = DM_run_code(state = state, element=current_ele)
  expect_true(current_ele[["res"]][["run_code"]][["isgood"]])

  state = suppressMessages(
    DM_set_current_element(
      state   = state,
      element = current_ele)
  )

})
