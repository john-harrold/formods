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

  expect_true( has_updated(ui_val = 0,           old_val=2         ))
  expect_true( has_updated(ui_val = "",          old_val=2         ))
  expect_false(has_updated(ui_val = 0,           old_val=2,        init_val=c(0, "") ))
  expect_false(has_updated(ui_val = "",          old_val=2,        init_val=c(0, "") ))

  expect_true(autocast(1)                      == 1)
  expect_true(autocast('1')                    == 1)
  expect_true(autocast(NA)                     == '"NA"')
  expect_false(is.numeric(autocast(NULL)))
  expect_true( is.numeric(autocast("1")))

})

test_that("FM -- preload functions ", {

old_wd = getwd()
tmp_wd = tempdir()
setwd(tmp_wd)
on.exit( setwd(old_wd))

# Creating files needed for testing in the temp directory:
if(dir.exists("config")){
  unlink("config", recursive = TRUE)
}


DM_data_dir =  file.path("data","DM")
if(dir.exists("data")){
  unlink("data", recursive = TRUE)
}
dir.create(DM_data_dir, recursive = TRUE)

if(file.exists("TEST_DATA.xlsx")){
  unlink("TEST_DATA.xlsx")
}

dir.create("config")

file.copy(from = system.file(package="formods", "templates", "formods.yaml"),    to="config")
file.copy(from = system.file(package="formods", "templates", "ASM.yaml"),        to="config")
file.copy(from = system.file(package="formods", "templates", "UD.yaml"),         to="config")
file.copy(from = system.file(package="formods", "templates", "FG.yaml"),         to="config")
file.copy(from = system.file(package="formods", "templates", "DW.yaml"),         to="config")
file.copy(from = system.file(package="formods", "templates", "DM.yaml"),         to="config")
file.copy(from = system.file(package="formods", "test_data", "TEST_DATA.xlsx"),  to=".")
file.copy(from = system.file(package="formods", "test_data", "TEST_DATA.xlsx"),  to=DM_data_dir)

# Testing using the standard preload functions:
sources = c(system.file(package="formods", "preload", "UD_preload.yaml"),
            system.file(package="formods", "preload", "ASM_preload.yaml"),
            system.file(package="formods", "preload", "DW_preload.yaml"),
            system.file(package="formods", "preload", "DM_preload.yaml"),
            system.file(package="formods", "preload", "FG_preload.yaml"))

sess_res = suppressMessages(FM_app_preload(session=list(), sources=sources))
expect_true(sess_res[["isgood"]])

# Creating preload from current state
mkp_res  = suppressMessages(FM_mk_app_preload(sess_res$session))
expect_true(mkp_res[["isgood"]])

# Writing the preload to file and rerunning it
tmp_pll_file = tempfile(fileext=".yaml")
yaml::write_yaml(x=mkp_res[["yaml_list"]], file=tmp_pll_file)
sess_res = suppressMessages(FM_app_preload(session=list(), sources=tmp_pll_file))
expect_true(sess_res[["isgood"]])

setwd(old_wd)

})

test_that("FM -- unfactor utility", {
  # Factor numeric -> unfactored
  f_num = factor(c(1, 2, 3))
  expect_false(is.factor(unfactor(f_num)))

  # Factor character -> unfactored (coercion warning expected)
  f_chr = factor(c("a", "b", "c"))
  suppressWarnings(expect_false(is.factor(unfactor(f_chr))))

  # Non-factor passthrough
  plain = c(1, 2, 3)
  expect_equal(unfactor(plain), plain)
})

test_that("FM -- linspace utility", {
  res11 = formods:::linspace(0, 10, 11)
  expect_equal(length(res11), 11)
  expect_equal(res11[1], 0)
  expect_equal(res11[11], 10)

  res3 = formods:::linspace(0, 1, 3)
  expect_equal(res3, c(0, 0.5, 1.0))

  # Default n=100
  res_def = formods:::linspace(0, 1)
  expect_equal(length(res_def), 100)

  # n < 2 warns and defaults to 100
  expect_message(
    res_warn <- formods:::linspace(0, 1, 1))
  expect_equal(length(res_warn), 100)
})

test_that("FM -- render_str utility", {
  # Evaluable expression
  res = render_str(estr = "paste0('hello', ' ', 'world')")
  expect_equal(res, "hello world")

  # Non-evaluable returns original string
  res_bad = render_str(estr = "not_a_function_xyz()")
  expect_equal(res_bad, "not_a_function_xyz()")

  # Empty string
  res_empty = render_str(estr = "")
  expect_equal(res_empty, "")
})

test_that("FM -- FM_pretty_sort", {
  # Character sorting
  unsrt = c("b10", "b2", "a1")
  srt   = FM_pretty_sort(unsrt)
  expect_true(is.character(srt))
  expect_equal(length(srt), 3)

  # Numeric sorting
  unsrt_num = c(3, 1, 2)
  srt_num   = FM_pretty_sort(unsrt_num)
  expect_equal(srt_num, c(1, 2, 3))
})

test_that("FM -- FM_build_comment", {
  c1 = FM_build_comment(1, "Level 1 header")
  expect_true(nchar(c1) == 75)
  expect_true(grepl("-", c1))

  c2 = FM_build_comment(2, "Level 2 header")
  expect_true(nchar(c2) == 75)
  expect_true(grepl("=", c2))

  c3 = FM_build_comment(3, "Level 3 header")
  expect_true(nchar(c3) == 75)
  expect_true(grepl("##", c3))

  # Long string truncated to 75
  long_str = paste0(rep("x", 100), collapse="")
  c_long = FM_build_comment(1, long_str)
  expect_true(nchar(c_long) == 75)
})

test_that("FM -- FM_tc try/catch", {
  # Successful command with capture
  res_good = FM_tc("my_val = 42", list(), c("my_val"))
  expect_true(res_good[["isgood"]])
  expect_equal(res_good[["capture"]][["my_val"]], 42)

  # Failed command
  res_bad = FM_tc("bad = not_a_command_xyz()", list(), c("bad"))
  expect_false(res_bad[["isgood"]])
  expect_true(length(res_bad[["msgs"]]) > 0)

  # tc_env variables available
  res_env = FM_tc("result = x + y", list(x=10, y=5), c("result"))
  expect_true(res_env[["isgood"]])
  expect_equal(res_env[["capture"]][["result"]], 15)

  # NULL capture captures all new objects
  res_null = FM_tc("a = 1; b = 2", list(), NULL)
  expect_true(res_null[["isgood"]])
  expect_true("a" %in% names(res_null[["capture"]]))
  expect_true("b" %in% names(res_null[["capture"]]))
})

test_that("FM -- is_shiny utility", {
  expect_false(is_shiny(list()))
  expect_true(is_shiny(shiny::MockShinySession$new()))
})

test_that("FM -- icon_link", {
  res = icon_link(href="https://example.com")
  expect_true(inherits(res, "shiny.tag"))

  res_null = icon_link(href=NULL)
  expect_null(res_null)
})

test_that("FM -- FM_fetch_current_mods", {
  mods_res = FM_fetch_current_mods()
  expect_true(is.list(mods_res))
  expect_true("mods" %in% names(mods_res))
  expect_true("ASM" %in% names(mods_res[["mods"]]))
  expect_true("UD"  %in% names(mods_res[["mods"]]))
  expect_true("DW"  %in% names(mods_res[["mods"]]))
  expect_true("FG"  %in% names(mods_res[["mods"]]))
})

test_that("FM -- FM_read_yaml", {
  yaml_file = system.file(package="formods", "templates", "formods.yaml")
  res = FM_read_yaml(yaml_file)
  expect_true(is.list(res))
  expect_true("FM" %in% names(res))
})

test_that("FM -- state get/set functions", {
  session = sess_res$session
  state   = sess_res$state

  # FM_fetch_app_state
  app_state = FM_fetch_app_state(session)
  expect_true(is.list(app_state))
  expect_true(length(app_state) > 0)

  # FM_fetch_mod_state
  ud_state = FM_fetch_mod_state(session, "UD")
  expect_true(!is.null(ud_state))
  expect_equal(ud_state[["MOD_TYPE"]], "UD")

  # FM_set_mod_state round-trip
  session_after = FM_set_mod_state(session, "UD", ud_state)
  ud_state2 = FM_fetch_mod_state(session_after, "UD")
  expect_equal(ud_state2[["MOD_TYPE"]], "UD")

  # FM_set_ui_msg
  state = FM_set_ui_msg(state, "test message")
  expect_true(grepl("test message", state[[state$MOD_TYPE]][["ui_msg"]]))

  # FM_set_ui_msg with append
  state = FM_set_ui_msg(state, " appended", append=TRUE)
  expect_true(grepl("appended", state[[state$MOD_TYPE]][["ui_msg"]]))
})

test_that("FM -- FM_set_notification", {
  state = sess_res$state
  state = suppressMessages(
    FM_set_notification(state, "test notification", "test_id", type="info"))
  expect_true(!is.null(state[["notifications"]][["test_id"]]))
  expect_equal(state[["notifications"]][["test_id"]][["text"]], "test notification")
  expect_equal(state[["notifications"]][["test_id"]][["type"]], "info")
  expect_true(is.numeric(state[["notifications"]][["test_id"]][["timestamp"]]))

  # Non-character notify_text should not set notification
  state2 = sess_res$state
  state2 = suppressMessages(
    FM_set_notification(state2, 123, "bad_id"))
  expect_null(state2[["notifications"]][["bad_id"]])
})

test_that("FM -- FM_message", {
  entry_types = c("alert", "danger", "warning", "info", "success", "h1", "h2", "h3")
  for(et in entry_types){
    expect_no_error(suppressMessages(FM_message("test line", entry_type=et)))
  }
  # escape_braces=FALSE
  expect_no_error(suppressMessages(FM_message("test {line}", escape_braces=FALSE)))
  # Returns NULL
  res = suppressMessages(FM_message("test"))
  expect_null(res)
})

test_that("FM -- FM_fetch_ds integration", {
  # Use the FG session for deepest dependency chain
  fg_res = suppressMessages(FG_test_mksession(session=list()))
  fg_state   = fg_res$state
  fg_session = fg_res$session

  ds_res = FM_fetch_ds(fg_state, fg_session)
  expect_true(ds_res[["hasds"]])
  expect_true(!is.null(ds_res[["catalog"]]))
  expect_true(is.data.frame(ds_res[["catalog"]]))
  expect_true(!is.null(ds_res[["choices"]]))

  # With specific ids
  ds_res2 = FM_fetch_ds(fg_state, fg_session, ids=c("UD", "DW"))
  expect_true(ds_res2[["hasds"]])
})

test_that("FM -- remove_hold", {
  state = sess_res$state
  MT = state[["MOD_TYPE"]]

  # Pick the first hold id
  hold_ids = names(state[[MT]][["ui_hold"]])
  if(length(hold_ids) > 0){
    inputId = hold_ids[1]

    # Set hold to TRUE
    state = set_hold(state, inputId)
    expect_true(fetch_hold(state, inputId))

    # Use MockShinySession for reference semantics (remove_hold returns NULL)
    mock_session = shiny::MockShinySession$new()
    mock_session = FM_set_mod_state(mock_session, state[["id"]], state)

    # remove_hold modifies session in place - requires reference semantics
    expect_no_error(
      suppressMessages(remove_hold(state, mock_session, inputId)))

    # With MockShinySession, the in-place modification is visible
    updated_state = FM_fetch_mod_state(mock_session, state[["id"]])
    expect_false(updated_state[[MT]][["ui_hold"]][[inputId]])
  }
})

test_that("FM -- FM_set_app_state", {
  # Use a DW session which has ASM state for the set_holds logic
  dw_res = suppressMessages(DW_test_mksession(session=list()))

  # FM_set_app_state modifies session in place (returns NULL), so we need
  # MockShinySession for reference semantics
  mock_session = shiny::MockShinySession$new()
  mock_session$userData[["FM"]] = FM_fetch_app_state(dw_res$session)
  app_state = FM_fetch_app_state(mock_session)

  # FM_set_app_state should set holds and reload yaml configs
  suppressMessages(
    FM_set_app_state(mock_session, app_state, set_holds=TRUE))

  # Verify the state was written back via reference semantics
  new_app_state = FM_fetch_app_state(mock_session)
  expect_true(is.list(new_app_state))
  expect_true(length(new_app_state) > 0)

  # Check holds were set to TRUE for all modules
  for(mod_key in names(new_app_state)){
    MT = new_app_state[[mod_key]][["MOD_TYPE"]]
    if(!is.null(MT)){
      for(hold_id in names(new_app_state[[mod_key]][[MT]][["ui_hold"]])){
        expect_true(new_app_state[[mod_key]][[MT]][["ui_hold"]][[hold_id]])
      }
    }
  }

  # Test with set_holds=FALSE and no ASM state (exercises else branch)
  plain_session = shiny::MockShinySession$new()
  plain_session$userData[["FM"]] = list()
  suppressMessages(
    FM_set_app_state(plain_session, list(), set_holds=FALSE))
})

test_that("FM -- FM_fetch_data_format", {
  state = sess_res$state

  # Create a simple test data frame with different column types
  df = data.frame(
    num_col  = c(1.1, 2.2, 3.3),
    chr_col  = c("a", "b", "c"),
    int_col  = c(1L, 2L, 3L),
    stringsAsFactors = FALSE
  )

  fmt = FM_fetch_data_format(df, state)
  expect_true(is.list(fmt))
  expect_true("col_heads"   %in% names(fmt))
  expect_true("col_subtext" %in% names(fmt))
  expect_true("col_info"    %in% names(fmt))

  # Should have an entry for each column
  expect_equal(length(fmt[["col_heads"]]),   3)
  expect_equal(length(fmt[["col_subtext"]]), 3)
  expect_equal(length(fmt[["col_info"]]),    3)

  # Each col_info entry should have color, label, and factor fields
  for(cname in names(df)){
    expect_true(!is.null(fmt[["col_info"]][[cname]][["color"]]))
    expect_true(!is.null(fmt[["col_info"]][[cname]][["label"]]))
    expect_true(is.logical(fmt[["col_info"]][[cname]][["factor"]]))
  }

  # Verify factor detection
  df_factor = data.frame(fct_col = factor(c("x", "y", "z")))
  fmt2 = FM_fetch_data_format(df_factor, state)
  expect_true(fmt2[["col_info"]][["fct_col"]][["factor"]])
})

test_that("FM -- fetch_package_version", {
  # Existing loaded package
  res = fetch_package_version("digest")
  expect_true(res[["isgood"]])
  expect_true(res[["version"]] != "NA")
  expect_true(nchar(res[["version_verb"]]) > 0)

  # Non-existent package
  res_bad = fetch_package_version("not_a_real_package_xyz")
  expect_false(res_bad[["isgood"]])
  expect_true(length(res_bad[["msgs"]]) > 0)
})

test_that("FM -- new_module_template", {
  tmp_dir = tempfile(pattern = "nmt_test_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  mod_files = new_module_template(
    SN          = "TT",
    Module_Name = "Test Template",
    package     = "testpkg",
    element     = "widget",
    file_dir    = tmp_dir)

  expect_true(is.list(mod_files))

  # Should have created files for server, yaml, mc, preload, funcs
  for(fkey in names(mod_files)){
    expect_true(file.exists(mod_files[[fkey]][["dest_full"]]))
  }

  # Verify substitutions were applied - the server file should contain "TT" not "ZZ"
  server_lines = readLines(mod_files[["server"]][["dest_full"]])
  server_text  = paste(server_lines, collapse = "\n")
  expect_true(grepl("TT", server_text))
  expect_false(grepl("===ZZ===", server_text))
})

test_that("FM -- use_formods", {
  tmp_repo = tempfile(pattern = "uf_test_")
  dir.create(tmp_repo)
  on.exit(unlink(tmp_repo, recursive = TRUE))

  res = suppressMessages(
    use_formods(
      SN          = "XX",
      Module_Name = "Test Mod",
      package     = "testpkg",
      element     = "item",
      repo_root   = tmp_repo))

  expect_true(is.list(res))

  # Verify directory structure was created
  expect_true(dir.exists(file.path(tmp_repo, "R")))
  expect_true(dir.exists(file.path(tmp_repo, "inst", "templates")))
  expect_true(dir.exists(file.path(tmp_repo, "inst", "preload")))
  expect_true(dir.exists(file.path(tmp_repo, "inst", "test_apps")))

  # Verify files were copied
  expect_true(file.exists(file.path(tmp_repo, "R", "XX_Server.R")))
  expect_true(file.exists(file.path(tmp_repo, "inst", "templates", "XX.yaml")))
  expect_true(file.exists(file.path(tmp_repo, "inst", "preload", "XX_preload.yaml")))
})
