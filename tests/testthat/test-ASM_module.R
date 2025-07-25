

test_that("ASM -- Fetch state ", {
  sess_res = suppressMessages(ASM_test_mksession(session=list()))
  state   = sess_res$state
  expect_true(state$ASM$isgood)
  expect_true(sess_res$isgood)
})

test_that("ASM -- Saving and loading app state ", {

tmp_wd = tempdir()
sources = c(system.file(package="formods", "preload", "UD_preload.yaml"),
            system.file(package="formods", "preload", "ASM_preload.yaml"),
            system.file(package="formods", "preload", "DM_preload.yaml"),
            system.file(package="formods", "preload", "DW_preload.yaml"),
            system.file(package="formods", "preload", "FG_preload.yaml"))

sess_res = suppressMessages(FM_app_preload(session=list(), sources=sources))

session = sess_res$session
state   = sess_res$all_sess_res$ASM$state

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