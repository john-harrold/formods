sess_res = suppressMessages(UD_test_mksession(session=list()))


test_that("UD -- Fetch state ", {
  state   = sess_res$state
  expect_true(state$UD$isgood)
  expect_true(sess_res$isgood)
})

test_that("UD -- Source file types", {

  state   = sess_res$state
  session = sess_res$session


  # Test data from the package
  df   = rio::import(
           file  = system.file(package="formods", "test_data", "TEST_DATA.xlsx"),
           which = "DATA")

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
  rio::export(catalog, file=path_xlsx, format="xlsx")

  res_csv =
  suppressMessages(
      UD_ds_read(state,
       data_file_ext   = "csv",
       data_file       = basename(path_csv),
       data_file_local = path_csv)
   )
  expect_true(res_csv$isgood)

  res_tsv =
  suppressMessages(
    UD_ds_read(state,
     data_file_ext   = "tsv",
     data_file       = basename(path_tsv),
     data_file_local = path_tsv)
   )
  expect_true(res_tsv$isgood)

  res_xlsx =
  suppressMessages(
    UD_ds_read(state,
     sheet           = sheet_xl,
     sheets          = sheet_xl,
     data_file_ext   = "xlsx",
     data_file       = basename(path_xlsx),
     data_file_local = path_xlsx)
   )
  expect_true(res_xlsx$isgood)

})
