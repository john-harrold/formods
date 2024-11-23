sess_res = suppressMessages(FG_test_mksession(session=list()))


test_that("FG -- Fetch state", {
  # Test data:
  state = sess_res$state
  expect_true(state$FG$isgood)
  expect_true(sess_res$isgood)
})

test_that("FG -- Forcing builds of figures", {
  for(fid in names(sess_res$state$FG$figs)){
    expect_equal(class(ggplot2::ggplot_build(sess_res[["state"]][["FG"]][["figs"]][[fid]][["fobj"]])),
                 "ggplot_built")
  }
})
