
library(testthat)

test_that("FM -- formods test preloads ", {

  sources = c(
  system.file(package="formods", "preload", "UD_preload.yaml"),
  system.file(package="formods", "preload", "ASM_preload.yaml"))

  res = suppressMessages( FM_test_preload(sources=sources))

  expect_true(res[["isgood"]])

  #---------------------------------------------------------------
  sources = c(system.file(package="formods", "preload", "UD_preload.yaml"),
              system.file(package="formods", "preload", "ASM_preload.yaml"),
              system.file(package="formods", "preload", "DM_preload.yaml"),
              system.file(package="formods", "preload", "DW_preload.yaml"),
              system.file(package="formods", "preload", "FG_preload.yaml"))

  preload_files = dplyr::tribble(
   ~src,                                                           ~dest,
   system.file(package="formods", "templates", "formods.yaml"),    "config",
   system.file(package="formods", "templates", "ASM.yaml"),        "config",
   system.file(package="formods", "templates", "UD.yaml"),         "config",
   system.file(package="formods", "templates", "FG.yaml"),         "config",
   system.file(package="formods", "templates", "DW.yaml"),         "config",
   system.file(package="formods", "templates", "DM.yaml"),         "config",
   system.file(package="formods", "test_data", "TEST_DATA.xlsx"),  ".", 
   system.file(package="formods", "test_data", "TEST_DATA.xlsx"),  "data/DM"
  )

  res = suppressMessages(
    FM_test_preload(
      sources       = sources, 
      preload_files = preload_files, 
      preload_dir   = tempdir()
    )
  )

  expect_true(res[["isgood"]])
  #---------------------------------------------------------------
  sources = c(system.file(package="formods", "preload", "workflow_DW_merge.yaml"))

  preload_files = dplyr::tribble(
   ~src,                                                           ~dest,
   system.file(package="formods", "templates", "formods.yaml"),    "config",
   system.file(package="formods", "templates", "ASM.yaml"),        "config",
   system.file(package="formods", "templates", "UD.yaml"),         "config",
   system.file(package="formods", "templates", "FG.yaml"),         "config",
   system.file(package="formods", "templates", "DW.yaml"),         "config",
   system.file(package="formods", "templates", "DM.yaml"),         "config" 
  )

  res = suppressMessages(
    FM_test_preload(
      sources       = sources, 
      preload_files = preload_files, 
      preload_dir   = tempdir()
    )
  )

  expect_true(res[["isgood"]])
})
