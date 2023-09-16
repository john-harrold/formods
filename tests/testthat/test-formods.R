sess_res = suppressMessages(UD_test_mksession(session=list()))


test_that("FM -- formods support functions ", {
  expect_true(is_installed("formods"))
  expect_false(is_installed("for mods"))
})

