sess_res = suppressMessages(DW_test_mksession(session=list()))


test_that("DW -- Fetch state", {
  # Test data:
  state = sess_res$state
  expect_true(state$DW$isgood)
})
