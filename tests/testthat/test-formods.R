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

if(file.exists("TEST_DATA.xlsx")){
  unlink("TEST_DATA.xlsx")
}

dir.create("config")

file.copy(from = system.file(package="formods", "templates", "formods.yaml"),    to="config")
file.copy(from = system.file(package="formods", "templates", "ASM.yaml"),        to="config")
file.copy(from = system.file(package="formods", "templates", "UD.yaml"),         to="config")
file.copy(from = system.file(package="formods", "templates", "FG.yaml"),         to="config")
file.copy(from = system.file(package="formods", "templates", "DW.yaml"),         to="config")
file.copy(from = system.file(package="formods", "test_data", "TEST_DATA.xlsx"),  to=".")
# Testing using the standard preload functions:
sources = c(system.file(package="formods", "preload", "UD_preload.yaml"),
            system.file(package="formods", "preload", "ASM_preload.yaml"),
            system.file(package="formods", "preload", "DW_preload.yaml"),
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
