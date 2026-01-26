

test_that("ASM -- Fetch state ", {
  sess_res = suppressMessages(ASM_test_mksession(session=list()))
  state   = sess_res$state
  expect_true(state$ASM$isgood)
  expect_true(sess_res$isgood)
})

test_that("ASM -- Saving and loading app state ", {

sources = c(system.file(package="formods", "preload", "UD_preload.yaml"),
            system.file(package="formods", "preload", "ASM_preload.yaml"),
            system.file(package="formods", "preload", "DM_preload.yaml"),
            system.file(package="formods", "preload", "DW_preload.yaml"),
            system.file(package="formods", "preload", "FG_preload.yaml"))

  pldir = tempfile(pattern="preload_")
  mpd_res = mk_preload_dir(
    directory = pldir,
    preload   = sources, 
    mod_yaml  = c( 
      system.file(package="formods",  "templates", "formods.yaml"),
      system.file(package="formods",  "templates", "ASM.yaml"),
      system.file(package="formods",  "templates", "DW.yaml"),
      system.file(package="formods",  "templates", "FG.yaml"),
      system.file(package="formods",  "templates", "DM.yaml"),
      system.file(package="formods",  "templates", "UD.yaml")),
    include = list(
      UD = list(
        from = system.file(package="formods", "test_data", "TEST_DATA.xlsx"),
        to   = "TEST_DATA.xlsx" ),
      DM = list(
        path = file.path("data", "DM"),
        from = system.file(package="formods", "test_data", "TEST_DATA.xlsx"),
        to   = "TEST_DATA.xlsx" )
    )
  )
  
  old_dir = getwd()
  setwd(pldir)
  on.exit(setwd(old_dir))
  sess_res = FM_app_preload(session=list(), sources="preload.yaml")

  setwd(old_dir)
  unlink(pldir, recursive = TRUE)

session = sess_res$session
state   = sess_res$all_sess_res$ASM$state

tmp_wd = tempdir()

# Testing with no pll
save_res = suppressMessages(ASM_save_state(state = state, session = session, file_path=file.path(tmp_wd ,'state.zip')))
expect_true(save_res[["isgood"]])

mkp_res  = suppressMessages(FM_mk_app_preload(sess_res$session))
expect_true(mkp_res[["isgood"]])

# Testing with pll
save_res = suppressMessages(ASM_save_state(state = state, session = session, file_path=file.path(tmp_wd ,'state.zip'), pll=mkp_res[["yaml_list"]]))
expect_true(save_res[["isgood"]])

load_res = suppressMessages(ASM_load_state(state = state, session = session, file_path=file.path(tmp_wd ,'state.zip')))
expect_true(load_res[["isgood"]])
})

test_that("ASM -- Checking workflows ", {
 sess_res = suppressMessages(FG_test_mksession(session=list()))
 session = sess_res[["session"]]
 state = FM_fetch_mod_state(id="ASM", session=session)
 pll = formods::FM_read_yaml(system.file(package="formods", "preload", "workflow_DW_merge.yaml"))
 cwf_res = ASM_check_workflow(state=state, session=session, pll=pll)
 expect_true(cwf_res[["deps_found"]])


 sess_res = suppressMessages(ASM_test_mksession(session=list()))
 session = sess_res[["session"]]
 state = FM_fetch_mod_state(id="ASM", session=session)
 pll = formods::FM_read_yaml(system.file(package="formods", "preload", "workflow_DW_merge.yaml"))
 cwf_res = ASM_check_workflow(state=state, session=session, pll=pll)
 expect_false(cwf_res[["deps_found"]])

})