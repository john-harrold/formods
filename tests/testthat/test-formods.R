sess_res = suppressMessages(UD_test_mksession(session=list()))


test_that("FM -- formods support functions ", {
  expect_true(is_installed("formods"))
  expect_false(is_installed("for mods"))

  expect_false(has_changed(ui_val = "",          old_val=""        ))
  expect_false(has_changed(ui_val = 0,           old_val=""        ))
  expect_false(has_changed(ui_val = 0,           old_val=0         ))
  expect_false(has_changed(ui_val = "",          old_val=0         ))
  expect_false(has_changed(ui_val = 0,           old_val=2         ))
  expect_false(has_changed(ui_val = "",          old_val=2         ))

  expect_true( has_changed(ui_val = 1,           old_val=2         ))
  expect_true( has_changed(ui_val = c(1, 2, 3),  old_val=c(1, 2)   ))
  expect_false(has_changed(ui_val = c(1, 2, 3),  old_val=c(1,2,3)  ))
  expect_true( has_changed(ui_val = c(1, 2, 3),  old_val=c(1,2,4)  ))

  expect_true(autocast(1)                      == 1)
  expect_true(autocast('1')                    == 1)
  expect_true(autocast(NA)                     == '"NA"')
  expect_false(is.numeric(autocast(NULL)))
  expect_true( is.numeric(autocast("1")))

})

