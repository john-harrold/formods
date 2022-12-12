sess_res = suppressMessages(FG_test_mksession(session=list()))


test_that("FG -- Fetch state", {
  # Test data:
  state = sess_res$state
  expect_true(state$FG$isgood)
})
