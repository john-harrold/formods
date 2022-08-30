test_that("UD -- Upload data module", {


  # Creating a module state object
  FM_yaml_file  = system.file(package="formods","templates", "formods.yaml")
  MOD_yaml_file = system.file(package="formods","templates", "UD.yaml")
  id            = "UD"
  state = UD_init_state(
            FM_yaml_file  = FM_yaml_file, 
            MOD_yaml_file = MOD_yaml_file, 
            id            = id)
  expect_true(state$UD$isgood)

  # Test data from the package
  df   = readxl::read_excel(
           path  = system.file(package="formods", "data", "TEST_DATA.xlsx"),
           sheet = "DATA")

  # Files to hold the test data:
  path_csv  = tempfile(fileext=".csv")
  path_tsv  = tempfile(fileext=".tsv")
  path_xlsx = tempfile(fileext=".xlsx")

  sheet_xl  = "my_data"
  catalog = list()
  catalog[[sheet_xl]] = df

  # Writing the test data to files:
  write.csv(  df, path_csv,           row.names=FALSE)
  write.table(df, path_tsv, sep="\t", row.names=FALSE)
  writexl::write_xlsx(catalog, path=path_xlsx)

  res_csv = UD_ds_read(state,
     data_file_ext   = "csv",
     data_file       = basename(path_csv),
     data_file_local = path_csv)
  expect_true(res_csv$isgood)

  res_tsv = UD_ds_read(state,
     data_file_ext   = "tsv",
     data_file       = basename(path_tsv),
     data_file_local = path_tsv)
  expect_true(res_tsv$isgood)

  res_xlsx = UD_ds_read(state,
     sheet           = sheet_xl,
     sheets          = sheet_xl,
     data_file_ext   = "xlsx",
     data_file       = basename(path_xlsx),
     data_file_local = path_xlsx)
  expect_true(res_xlsx$isgood)

  # Attacing datasets:
  state = UD_attach_ds(
            state,
            data_file_local = path_xlsx              ,
            data_file_ext   = "xlsx"                 ,
            data_file       = basename(path_xlsx)    ,
            sheet           = sheet_xl               ,
            sheets          = sheet_xl               ,
            code            = res_xlsx[["code"]]     ,
            contents        = res_xlsx[["contents"]] ,
            object_name     = res_xlsx[["object_name"]] ,
            isgood          = TRUE)

  checksum        = digest::digest(res_xlsx[["contents"]], algo=c("md5"))
  expect_equal(state$UD$checksum, checksum)

})
