test_that("DW -- Data wrangling module", {


  # Test data:
  DATA = readxl::read_excel(
           path  = system.file(package="formods", "test_data", "TEST_DATA.xlsx"),
           sheet = "DATA")
  react_FM    = reactiveValues()

  ds = list(
     UD = list(
          data_file_local = NULL,
          data_file_ext   = NULL,
          data_file       = NULL,
          sheet           = NULL,
          sheets          = NULL,
          code            = "# Raw data loading code goes here",
          object_name     = "TMPDS",
          contents        = DATA,
          checksum        = digest::digest(DATA, algo=c("md5")),
          isgood          = TRUE
        )
    )
  react_FM$UD = ds

  # Creating the DW state:
  FM_yaml_file  = system.file(package="formods","templates", "formods.yaml")
  MOD_yaml_file = system.file(package="formods","templates", "DW.yaml")
  id            = "DW"

  state = DW_init_state(FM_yaml_file  = FM_yaml_file,
                        MOD_yaml_file = MOD_yaml_file,
                        id            = id,
                        id_UD         = "UD",
                        react_state   = react_FM)
  expect_true(state$DW$isgood)
})
