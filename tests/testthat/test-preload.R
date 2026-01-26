
library(testthat)

test_that("ASM -- formods test preloads ", {

  sources = c(
  system.file(package="formods", "preload", "UD_preload.yaml"),
  system.file(package="formods", "preload", "ASM_preload.yaml"))

  preload_files = dplyr::tribble(
   ~src,                                                           ~dest,
   system.file(package="formods", "templates", "formods.yaml"),    "config",
   system.file(package="formods", "templates", "ASM.yaml"),        "config",
   system.file(package="formods", "templates", "UD.yaml"),         "config",
   system.file(package="formods", "test_data", "TEST_DATA.xlsx"),  "."
  )

  res = suppressMessages( 
    ASM_test_preload(
      sources       = sources, 
      preload_files = preload_files
    )
  )

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
    ASM_test_preload(
      sources       = sources, 
      preload_files = preload_files
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
    ASM_test_preload(
      sources       = sources, 
      preload_files = preload_files
    )
  )

  expect_true(res[["isgood"]])
  #---------------------------------------------------------------
  # Testing workflow option
  # Creating a yaml file with only the source data elements:
  sd_list = FM_read_yaml(system.file(package="formods", "preload", "workflow_DW_merge.yaml"))
  keep_ele = c("ASM", "UD",  "DM")
  sd_list = sd_list[keep_ele]
  sd_file = tempfile(fileext=".yaml")
  yaml::write_yaml(sd_list, file=sd_file)

  # Along with the source data we need empty values for the other modles so
  # that their config files will be included
  sources  = c(sd_file, 
              system.file(package="formods", "preload", "DW_preload_empty.yaml"),
              system.file(package="formods", "preload", "FG_preload_empty.yaml"))

  workflow = c(system.file(package="formods", "preload", "workflow_DW_merge.yaml"))
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
    ASM_test_preload(
      sources       = sources, 
      preload_files = preload_files,
      workflow      = workflow
    )
  )
  expect_true(res[["isgood"]])
#---------------------------------------------------------------


})

