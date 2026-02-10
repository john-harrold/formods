skip_on_cran()
skip_if(nzchar(Sys.getenv("R_COVR")), "Not running shinytest2 under covr")
skip_if_not_installed("shinytest2")
skip_if_not_installed("shinydashboard")
skip_if_not_installed("ggpubr")
skip_if_not_installed("plotly")
skip_if_not_installed("shinybusy")
skip_if_not_installed("prompter")
skip_if_not_installed("clipr")
skip_if_not_installed("janitor")
skip_if_not_installed("zoo")
library(shinytest2)

# Build a temporary app directory from inst/ sources.
# Uses FM_compact.R (with the if(interactive()) wrapper stripped) and
# TEST_DATA.xlsx -- nothing is stored under tests/testthat.
build_app_dir <- function() {
  app_dir <- file.path(tempdir(), "formods_shinytest2_app")
  dir.create(app_dir, showWarnings = FALSE, recursive = TRUE)

  # Read FM_compact.R template and strip the if(interactive()) guard
  src <- system.file("templates", "FM_compact.R", package = "formods")
  lines <- readLines(src)
  # Remove the opening  if(interactive()){  and the closing  }
  lines <- lines[lines != "if(interactive()){"]
  # Drop the trailing lone "}" that closed the if block
  if (lines[length(lines)] == "}") lines <- lines[-length(lines)]
  writeLines(lines, file.path(app_dir, "app.R"))

  # Copy test data into the app directory for upload_file()
  file.copy(
    system.file("test_data", "TEST_DATA.xlsx", package = "formods"),
    file.path(app_dir, "TEST_DATA.xlsx"),
    overwrite = TRUE)

  app_dir
}

# Helper: navigate to a shinydashboard tab by its tabName
goto_tab <- function(app, tab_name) {
  app$run_js(sprintf("$('a[data-value=\"%s\"]').click()", tab_name))
  app$wait_for_idle(timeout = 10000)
  Sys.sleep(1)
  app$wait_for_idle(timeout = 10000)
}

# Helper: create a fresh AppDriver with data already uploaded
new_app_with_data <- function() {
  app_dir <- build_app_dir()
  app <- AppDriver$new(app_dir = app_dir, name = "formods_e2e",
                       height = 800, width = 1200,
                       timeout = 30000, load_timeout = 60000)

  # Navigate to upload tab and upload test data
  goto_tab(app, "upload")
  data_file <- file.path(app_dir, "TEST_DATA.xlsx")
  app$upload_file(`UD-input_data_file` = data_file)
  app$wait_for_idle(timeout = 20000)
  Sys.sleep(2)
  app$wait_for_idle(timeout = 20000)

  app
}


test_that("DW -- shinytest2 data upload and filter view", {
  app <- new_app_with_data()
  on.exit(app$stop(), add = TRUE)

  # Navigate to wrangle tab so DW UI renders
  goto_tab(app, "wrangle")

  # Set view key to "Observations" and save
  app$set_inputs(`DW-current_key` = "Observations")
  app$click("DW-button_dw_save")
  app$wait_for_idle(timeout = 10000)

  # Select filter column = EVID
  app$set_inputs(`DW-select_fds_filter_column_open` = TRUE,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`DW-select_fds_filter_column` = "EVID")
  app$set_inputs(`DW-select_fds_filter_column_open` = FALSE,
                 allow_no_input_binding_ = TRUE)

  # Select filter operator = "=="
  app$set_inputs(`DW-select_fds_filter_operator_open` = TRUE,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`DW-select_fds_filter_operator` = "==")
  app$set_inputs(`DW-select_fds_filter_operator_open` = FALSE,
                 allow_no_input_binding_ = TRUE)

  # Add filter element
  app$click("DW-button_dw_add_element")
  app$wait_for_idle(timeout = 10000)

  # Verify the DW code contains dplyr::filter
  code_val <- app$get_value(input = "DW-ui_dw_code")
  expect_true(grepl("dplyr::filter", code_val),
              info = paste("Expected dplyr::filter in DW code, got:", code_val))
})


test_that("DW -- shinytest2 new view with group element", {
  app <- new_app_with_data()
  on.exit(app$stop(), add = TRUE)

  # Navigate to wrangle tab
  goto_tab(app, "wrangle")

  # Create a new view
  app$click("DW-button_dw_new")
  app$wait_for_idle(timeout = 10000)

  # Set key to "Parameters" and save
  app$set_inputs(`DW-current_key` = "Parameters")
  app$click("DW-button_dw_save")
  app$wait_for_idle(timeout = 10000)

  # Switch element type to "group"
  app$set_inputs(`DW-select_dw_element_open` = TRUE,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`DW-select_dw_element` = "group")
  app$set_inputs(`DW-select_dw_element_open` = FALSE,
                 allow_no_input_binding_ = TRUE)
  app$wait_for_idle(timeout = 10000)

  # Select group column = ID
  app$set_inputs(`DW-select_fds_group_column_open` = TRUE,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`DW-select_fds_group_column` = "ID")
  app$set_inputs(`DW-select_fds_group_column_open` = FALSE,
                 allow_no_input_binding_ = TRUE)

  # Add group element
  app$click("DW-button_dw_add_element")
  app$wait_for_idle(timeout = 10000)

  # Verify the DW code contains dplyr::group_by
  code_val <- app$get_value(input = "DW-ui_dw_code")
  expect_true(grepl("dplyr::group_by", code_val),
              info = paste("Expected dplyr::group_by in DW code, got:", code_val))
})


test_that("FG -- shinytest2 figure with line and facet", {
  app <- new_app_with_data()
  on.exit(app$stop(), add = TRUE)

  # --- Navigate to wrangle tab and create a DW view with filter ---
  goto_tab(app, "wrangle")

  app$set_inputs(`DW-current_key` = "Observations")
  app$click("DW-button_dw_save")
  app$wait_for_idle(timeout = 10000)

  # Add filter: EVID == 0
  app$set_inputs(`DW-select_fds_filter_column_open` = TRUE,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`DW-select_fds_filter_column` = "EVID")
  app$set_inputs(`DW-select_fds_filter_column_open` = FALSE,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`DW-select_fds_filter_operator_open` = TRUE,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`DW-select_fds_filter_operator` = "==")
  app$set_inputs(`DW-select_fds_filter_operator_open` = FALSE,
                 allow_no_input_binding_ = TRUE)
  app$click("DW-button_dw_add_element")
  app$wait_for_idle(timeout = 10000)

  # --- Navigate to plot tab ---
  goto_tab(app, "plot")

  # Select data view
  app$set_inputs(`FG-select_current_view_open` = TRUE,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`FG-select_current_view` = "DW_myDS_1")
  app$set_inputs(`FG-select_current_view_open` = FALSE,
                 allow_no_input_binding_ = TRUE)
  app$wait_for_idle(timeout = 10000)

  # Set figure key and save
  app$set_inputs(`FG-text_fig_key` = "Individual profiles by cohort")
  app$click("FG-button_fig_save")
  app$wait_for_idle(timeout = 10000)

  # Set aesthetics: x=TIME_DY, y=DV, color=Cohort, group=ID
  app$set_inputs(`FG-select_component_x_open` = TRUE,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`FG-select_component_x` = "TIME_DY")
  app$set_inputs(`FG-select_component_x_open` = FALSE,
                 allow_no_input_binding_ = TRUE)

  app$set_inputs(`FG-select_component_y_open` = TRUE,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`FG-select_component_y` = "DV")
  app$set_inputs(`FG-select_component_y_open` = FALSE,
                 allow_no_input_binding_ = TRUE)

  app$set_inputs(`FG-select_component_color_open` = TRUE,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`FG-select_component_color` = "Cohort")
  app$set_inputs(`FG-select_component_color_open` = FALSE,
                 allow_no_input_binding_ = TRUE)

  app$set_inputs(`FG-select_component_group_open` = TRUE,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`FG-select_component_group` = "ID")
  app$set_inputs(`FG-select_component_group_open` = FALSE,
                 allow_no_input_binding_ = TRUE)

  # Add line element (default element type is "line")
  app$click("FG-button_element_add")
  app$wait_for_idle(timeout = 10000)

  # Switch element to "facet"
  app$set_inputs(`FG-select_fg_element_open` = TRUE,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`FG-select_fg_element` = "facet")
  app$set_inputs(`FG-select_fg_element_open` = FALSE,
                 allow_no_input_binding_ = TRUE)
  app$wait_for_idle(timeout = 5000)

  # Set facet column = Cohort
  app$set_inputs(`FG-select_component_facet_open` = TRUE,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`FG-select_component_facet` = "Cohort")
  app$set_inputs(`FG-select_component_facet_open` = FALSE,
                 allow_no_input_binding_ = TRUE)

  # Add facet element
  app$click("FG-button_element_add")
  app$wait_for_idle(timeout = 10000)

  # Verify figure code contains ggplot
  fg_code <- app$get_value(input = "FG-ui_fg_code")
  expect_true(!is.null(fg_code) && nchar(fg_code) > 0,
              info = "Expected non-empty FG code output")
  expect_true(grepl("ggplot", fg_code),
              info = paste("Expected ggplot in FG code, got:", fg_code))
})


# ---------------------------------------------------------------------------
# Helper: build an app directory with pre-loaded module state from YAML
# ---------------------------------------------------------------------------
build_preloaded_app_dir <- function(preload_yaml) {
  app_dir <- file.path(tempdir(), paste0("formods_preload_", format(Sys.time(), "%H%M%S")))

  mk_preload_dir(
    directory = app_dir,
    preload   = preload_yaml,
    mod_yaml  = c(
      system.file(package = "formods", "templates", "formods.yaml"),
      system.file(package = "formods", "templates", "ASM.yaml"),
      system.file(package = "formods", "templates", "UD.yaml"),
      system.file(package = "formods", "templates", "DM.yaml"),
      system.file(package = "formods", "templates", "DW.yaml"),
      system.file(package = "formods", "templates", "FG.yaml"))
  )

  # Copy app.R template (strip the if(interactive()) guard)
  src   <- system.file("templates", "FM_compact.R", package = "formods")
  lines <- readLines(src)
  lines <- lines[lines != "if(interactive()){"]
  if (lines[length(lines)] == "}") lines <- lines[-length(lines)]
  writeLines(lines, file.path(app_dir, "app.R"))

  app_dir
}


test_that("DW -- shinytest2 merge workflow via preloaded DM URL sources", {
  skip_if_offline()

  # Ensure the default test-data preload trigger doesn't interfere
  unlink(file.path(tempdir(), "formods.test"))

  preload_yaml <- system.file("preload", "workflow_DW_merge.yaml",
                               package = "formods")
  app_dir <- build_preloaded_app_dir(preload_yaml)
  app <- AppDriver$new(app_dir = app_dir, name = "formods_merge",
                       height = 800, width = 1200,
                       timeout = 60000, load_timeout = 300000)
  on.exit(app$stop(), add = TRUE)

  # Navigate to wrangle tab so DW UI renders
  goto_tab(app, "wrangle")
  Sys.sleep(2)
  app$wait_for_idle(timeout = 30000)

  # Select View 3 (ARD) which contains merge operations.
  # View IDs are "view_1", "view_2", "view_3".
  app$set_inputs(`DW-select_dw_views_open` = TRUE,
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(`DW-select_dw_views` = "view_3")
  app$set_inputs(`DW-select_dw_views_open` = FALSE,
                 allow_no_input_binding_ = TRUE)
  app$wait_for_idle(timeout = 30000)
  Sys.sleep(2)
  app$wait_for_idle(timeout = 30000)

  # -----------------------------------------------------------------------
  # Verify the DW code output for View 3 contains key markers
  # -----------------------------------------------------------------------
  code_val <- app$get_value(input = "DW-ui_dw_code")
  expect_true(!is.null(code_val) && nchar(code_val) > 0,
              info = "Expected non-empty DW code output")
  expect_true(grepl("dplyr::rename",    code_val),
              info = paste("Expected dplyr::rename in DW code, got:", code_val))
  expect_true(grepl("dplyr::select",    code_val),
              info = paste("Expected dplyr::select in DW code, got:", code_val))
  expect_true(grepl("rbind",            code_val),
              info = paste("Expected rbind in DW code, got:", code_val))
  expect_true(grepl("dplyr::left_join", code_val),
              info = paste("Expected dplyr::left_join in DW code, got:", code_val))
  expect_true(grepl("dplyr::arrange",   code_val),
              info = paste("Expected dplyr::arrange in DW code, got:", code_val))
  expect_true(grepl("dplyr::group_by",  code_val),
              info = paste("Expected dplyr::group_by in DW code, got:", code_val))
  expect_true(grepl("dplyr::ungroup",   code_val),
              info = paste("Expected dplyr::ungroup in DW code, got:", code_val))
})
