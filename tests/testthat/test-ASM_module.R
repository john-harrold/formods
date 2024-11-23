sess_res = suppressMessages(ASM_test_mksession(session=list()))


test_that("ASM -- Fetch state ", {
  state   = sess_res$state
  expect_true(state$ASM$isgood)
  expect_true(sess_res$isgood)
})

