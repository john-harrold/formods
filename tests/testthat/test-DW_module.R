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

test_that("DW -- fetch and set current view", {
  state   = sess_res$state
  session = sess_res$session
  dw_view = DW_fetch_current_view(state)
  expect_false(is.null(dw_view))
  expect_true(!is.null(dw_view[["id"]]))
  expect_true(!is.null(dw_view[["key"]]))
  expect_true(is.data.frame(dw_view[["WDS"]]))

  # Modify the key and set it back
  old_key = dw_view[["key"]]
  dw_view[["key"]] = "modified_key"
  state = DW_set_current_view(state, session, dw_view)
  dw_view_after = DW_fetch_current_view(state)
  expect_equal(dw_view_after[["key"]], "modified_key")
})

test_that("DW -- new view creation", {
  state   = sess_res$state
  session = sess_res$session
  n_before = length(state[["DW"]][["views"]])
  cv_before = state[["DW"]][["current_view"]]
  state = suppressMessages(DW_new_view(state, session))
  n_after = length(state[["DW"]][["views"]])
  cv_after = state[["DW"]][["current_view"]]
  expect_equal(n_after, n_before + 1)
  expect_false(cv_before == cv_after)
  new_view = DW_fetch_current_view(state)
  expect_true(!is.null(new_view[["id"]]))
  expect_true(new_view[["isgood"]])
})

test_that("DW -- attach data source", {
  state   = sess_res$state
  session = sess_res$session
  dw_view = DW_fetch_current_view(state)

  # Valid source
  ds_source_id = names(state[["DW"]][["DSV"]][["ds"]])[1]
  dw_view_attached = suppressMessages(
    DW_attach_ds(state, dw_view, ds_source_id))
  expect_true(dw_view_attached[["isgood"]])
  expect_true(is.data.frame(dw_view_attached[["WDS"]]))
  expect_equal(dw_view_attached[["ds_source_id"]], ds_source_id)

  # Invalid source
  dw_view_bad = suppressMessages(
    DW_attach_ds(state, dw_view, "NOT_A_REAL_SOURCE"))
  expect_false(dw_view_bad[["isgood"]])
})

test_that("DW -- wrangling via proc_pll", {
  state   = sess_res$state
  session = sess_res$session

  # Filter action
  pll_filter = list(action="filter", column="EVID", operator="==", rhs=0)
  state_f = suppressMessages(
    formods:::DW_proc_pll(state, session, pll_filter))
  expect_true(state_f[["DW"]][["res"]][["proc_pll"]][["isgood"]])
  dw_view_f = DW_fetch_current_view(state_f)
  expect_true(is.data.frame(dw_view_f[["elements_table"]]))
  expect_true(nrow(dw_view_f[["elements_table"]]) > 0)

  # Bad action
  pll_bad = list(action="not_a_real_action")
  state_b = suppressMessages(
    formods:::DW_proc_pll(state, session, pll_bad))
  expect_false(state_b[["DW"]][["res"]][["proc_pll"]][["isgood"]])
})

test_that("DW -- fetch code", {
  state = sess_res$state
  code  = DW_fetch_code(state)
  expect_true(!is.null(code))
  expect_true(is.character(code))
  expect_true(nchar(code) > 0)
})

test_that("DW -- fetch datasets", {
  state  = sess_res$state
  ds_res = DW_fetch_ds(state)
  expect_true(ds_res[["isgood"]])
  expect_true(ds_res[["hasds"]])
  expect_true(length(ds_res[["ds"]]) > 0)
  first_ds = ds_res[["ds"]][[1]]
  expect_true(!is.null(first_ds[["label"]]))
  expect_equal(first_ds[["MOD_TYPE"]], "DW")
  expect_true(is.data.frame(first_ds[["DS"]]))

  # meta_only
  ds_meta = DW_fetch_ds(state, meta_only=TRUE)
  expect_true(ds_meta[["isgood"]])
  expect_true(ds_meta[["hasds"]])
  expect_null(ds_meta[["ds"]][[1]][["DS"]])
})

test_that("DW -- hasds check", {
  state = sess_res$state
  expect_true(DW_hasds(state))
})

test_that("DW -- update checksum", {
  state = sess_res$state
  state = suppressMessages(DW_update_checksum(state))
  chk1 = state[["DW"]][["checksum"]]
  expect_true(!is.null(chk1))
  expect_true(is.character(chk1))
  # Same data should produce same checksum
  state = suppressMessages(DW_update_checksum(state))
  chk2 = state[["DW"]][["checksum"]]
  expect_equal(chk1, chk2)
})

test_that("DW -- fetch available sources", {
  state   = sess_res$state
  session = sess_res$session
  avail = suppressMessages(
    DW_fetch_available_sources(state, session))
  expect_true(avail[["isgood"]])
  expect_true(!is.null(avail[["catalog"]]))
  expect_true(!is.null(avail[["choices"]]))
})

test_that("DW -- mk_preload round-trip", {
  state = sess_res$state
  mkp   = suppressMessages(DW_mk_preload(state))
  expect_true(mkp[["isgood"]])
  expect_true(!is.null(mkp[["yaml_list"]]))
  yl = mkp[["yaml_list"]]
  first_key = names(yl)[1]
  expect_true(!is.null(yl[[first_key]][["fm_yaml"]]))
  expect_true(!is.null(yl[[first_key]][["mod_yaml"]]))
  expect_true(!is.null(yl[[first_key]][["elements"]]))
})

test_that("DW -- append report", {
  state = sess_res$state
  rpt   = list(summary = list(), sheets = list())

  # xlsx with gen_code_only
  rpt_res = suppressMessages(
    DW_append_report(state, rpt, rpttype="xlsx", gen_code_only=TRUE))
  expect_true(rpt_res[["isgood"]])
  expect_true(rpt_res[["hasrptele"]])
  expect_true(length(rpt_res[["code"]]) > 0)

  # pptx unsupported for DW
  rpt_res2 = suppressMessages(
    DW_append_report(state, rpt, rpttype="pptx"))
  expect_false(rpt_res2[["hasrptele"]])
})