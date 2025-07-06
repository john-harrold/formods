sess_res = suppressMessages(DW_test_mksession(session=list()))


test_that("DW -- Fetch state", {
  state = sess_res$state
  expect_true(state$DW$isgood)
  expect_true(sess_res$isgood)
})


test_that("DW -- view dependencies", {
  state = sess_res$state
  session  = sess_res$session
  view_ids = names(state[["DW"]][["views"]])[1]
  view_deps = DW_fetch_view_deps(
    state    = state, 
    session  = session, 
    view_ids = view_ids)
  expect_true(view_deps$isgood)

  view_ids = "NOTREAL"
  view_deps = DW_fetch_view_deps(
    state    = state, 
    session  = session, 
    view_ids = view_ids)
  expect_false(view_deps$isgood)

})

test_that("DW -- rectify views", {
  state = sess_res$state
  session  = sess_res$session
  view_ids = names(state[["DW"]][["views"]])[1]
  expect_no_error(
    DW_rectify(
      state    = state, 
      session  = session, 
      view_ids = view_ids))
})