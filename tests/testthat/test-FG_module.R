sess_res = suppressMessages(FG_test_mksession(session=list()))


test_that("FG -- Fetch state", {
  # Test data:
  state = sess_res$state
  expect_true(state$FG$isgood)
  expect_true(sess_res$isgood)
})

test_that("FG -- Forcing builds of figures", {
  for(fid in names(sess_res$state$FG$figs)){
    expect_true(inherits(ggplot2::ggplot_build(sess_res[["state"]][["FG"]][["figs"]][[fid]][["fobj"]]),
                         c("ggplot2::ggplot_built")))
  # expect_equal(class(ggplot2::ggplot_build(sess_res[["state"]][["FG"]][["figs"]][[fid]][["fobj"]])),
  #              "ggplot_built")
  }
})

test_that("FG -- fetch and set current figure", {
  state = sess_res$state
  fig   = FG_fetch_current_fig(state)
  expect_false(is.null(fig))
  expect_true(!is.null(fig[["id"]]))
  expect_true(!is.null(fig[["key"]]))
  expect_false(is.null(fig[["fobj"]]))

  # Modify key and round-trip
  old_key = fig[["key"]]
  fig[["key"]] = "modified_fig_key"
  state = FG_set_current_fig(state, fig)
  fig_after = FG_fetch_current_fig(state)
  expect_equal(fig_after[["key"]], "modified_fig_key")
})

test_that("FG -- new figure creation", {
  state = sess_res$state
  n_before = length(state[["FG"]][["figs"]])
  cf_before = state[["FG"]][["current_fig"]]
  state = suppressMessages(FG_new_fig(state))
  n_after = length(state[["FG"]][["figs"]])
  cf_after = state[["FG"]][["current_fig"]]
  expect_equal(n_after, n_before + 1)
  expect_false(cf_before == cf_after)
  new_fig = FG_fetch_current_fig(state)
  expect_true(!is.null(new_fig[["id"]]))
})

test_that("FG -- build with new element", {
  state = sess_res$state
  fig   = FG_fetch_current_fig(state)
  fg_obj = fig[["fg_object_name"]]

  cmd = paste0(fg_obj, " <- ", fg_obj, " + ggplot2::geom_point(ggplot2::aes(x=TIME_DY, y=DV))")
  pll = list(type="point",
             aes=list(x="TIME_DY", y="DV"))
  state = suppressMessages(
    FG_build(state, cmd=cmd, pll=pll, element="point", desc="add points"))
  fig_after = FG_fetch_current_fig(state)
  expect_true(fig_after[["add_isgood"]])
  expect_true(is.data.frame(fig_after[["elements_table"]]))
  expect_true("point" %in% fig_after[["elements_table"]][["Element"]])
})

test_that("FG -- build with bad command", {
  state = sess_res$state
  fig   = FG_fetch_current_fig(state)
  fg_obj = fig[["fg_object_name"]]
  cmd = paste0(fg_obj, " <- ", fg_obj, " + not_a_real_function()")
  state = suppressMessages(
    FG_build(state, cmd=cmd, pll=list(type="bad"), element="bad", desc="bad build"))
  fig_after = FG_fetch_current_fig(state)
  expect_false(fig_after[["add_isgood"]])
})

test_that("FG -- build delete row", {
  state = sess_res$state
  fig = FG_fetch_current_fig(state)
  # Only test if there are elements to delete
  if(!is.null(fig[["elements_table"]]) && nrow(fig[["elements_table"]]) > 0){
    n_before = nrow(fig[["elements_table"]])
    state = suppressMessages(FG_build(state, del_row=1))
    fig_after = FG_fetch_current_fig(state)
    if(!is.null(fig_after[["elements_table"]])){
      expect_true(nrow(fig_after[["elements_table"]]) < n_before)
    } else {
      # Deleting the only row results in NULL table
      expect_equal(n_before, 1)
    }
  }
})

test_that("FG -- fetch code", {
  state = sess_res$state
  code  = FG_fetch_code(state)
  expect_true(!is.null(code))
  expect_true(is.character(code))
  expect_true(nchar(paste(code, collapse="")) > 0)
  expect_true(any(grepl("ggplot", code)))
})

test_that("FG -- update checksum", {
  state = sess_res$state
  state = suppressMessages(FG_update_checksum(state))
  chk1 = state[["FG"]][["checksum"]]
  expect_true(!is.null(chk1))
  expect_true(is.character(chk1))
  state = suppressMessages(FG_update_checksum(state))
  chk2 = state[["FG"]][["checksum"]]
  expect_equal(chk1, chk2)
})

test_that("FG -- mk_preload", {
  state = sess_res$state
  mkp   = suppressMessages(FG_mk_preload(state))
  expect_true(mkp[["isgood"]])
  expect_true(!is.null(mkp[["yaml_list"]]))
})

test_that("FG -- append report", {
  state = sess_res$state
  rpt   = list(summary = list(), sheets = list())

  # xlsx unsupported for FG
  rpt_res = suppressMessages(
    FG_append_report(state, rpt, rpttype="xlsx"))
  expect_false(rpt_res[["hasrptele"]])

  # pptx with gen_code_only (no onbrand rpt object needed since code only)
  rpt_res2 = suppressMessages(
    FG_append_report(state, rpt, rpttype="pptx", gen_code_only=TRUE))
  expect_true(rpt_res2[["hasrptele"]])
  expect_true(length(rpt_res2[["code"]]) > 0)
})

test_that("FG -- full session with faceted figures", {
  full_res = suppressMessages(
    FG_test_mksession(session=list(), full=TRUE))
  expect_true(full_res$isgood)
  state_full = full_res$state
  fig_ids = names(state_full[["FG"]][["figs"]])
  expect_true(length(fig_ids) >= 2)

  # Second figure should have facet element
  second_fig = state_full[["FG"]][["figs"]][[fig_ids[2]]]
  expect_true("facet" %in% second_fig[["elements_table"]][["Element"]])

  # Test extract page on the faceted figure
  state_full[["FG"]][["current_fig"]] = fig_ids[2]
  page_obj = suppressMessages(FG_extract_page(state_full, 1))
  expect_true(inherits(page_obj, "ggplot"))
})
