
session = shiny::MockShinySession$new()
sources = c(system.file(package="formods", "preload", "ASM_preload.yaml"),
            system.file(package="formods", "preload", "UD_preload.yaml"),
            system.file(package="formods", "preload", "DM_preload.yaml"),
            system.file(package="formods", "preload", "DW_preload.yaml"),
            system.file(package="formods", "preload", "FG_preload.yaml")
            )

res = suppressMessages(FM_app_preload(session=session, sources=sources))

session = res$session
state   = res$all_sess_res$FG$state
DSV = FM_fetch_ds(state, session, ids=c("UD", "DM", "DW"), meta_only=TRUE)


test_that("FM -- formods fetch resources ", {
fr_res = fetch_resource(catalog=DSV[["catalog"]], id = "UD", idx="1", res_label = "")
expect_true(fr_res[["isgood"]])
expect_true(fr_res[["res_obj"]] == "myDS")

fr_res = fetch_resource(catalog=DSV[["catalog"]], id = "UD", idx="1", res_label = NULL)
expect_true(fr_res[["isgood"]])
expect_true(fr_res[["res_obj"]] == "myDS")

fr_res = fetch_resource(catalog=DSV[["catalog"]], id = "DW", idx="2", res_label = "")
expect_true(fr_res[["isgood"]])
expect_true(fr_res[["res_obj"]] == "DW_myDS_2")

fr_res = fetch_resource(catalog=DSV[["catalog"]], id = "DW", idx="2", res_label = NULL)
expect_true(fr_res[["isgood"]])
expect_true(fr_res[["res_obj"]] == "DW_myDS_2")

# these should find the resource based on the res_label
fr_res = fetch_resource(catalog=DSV[["catalog"]], id = "DM", idx="1", res_label = "PC")
expect_true(fr_res[["isgood"]])
expect_true(fr_res[["res_obj"]] == "DM_obj_2")

fr_res = fetch_resource(catalog=DSV[["catalog"]], id = "DM", idx=NULL, res_label = "PC")
expect_true(fr_res[["isgood"]])
expect_true(fr_res[["res_obj"]] == "DM_obj_2")

# These should all fail
# No idx or res_label:
fr_res = fetch_resource(catalog=DSV[["catalog"]], id = "DM", idx=NULL, res_label = "")
expect_false(fr_res[["isgood"]])
fr_res = fetch_resource(catalog=DSV[["catalog"]], id = "DM", idx=NULL, res_label = NULL)
expect_false(fr_res[["isgood"]])

# No catalog
fr_res = fetch_resource(state,                           id = "DW", idx="2", res_label = "")
expect_false(fr_res[["isgood"]])

# Bad resource label but valid idx
fr_res = fetch_resource(catalog=DSV[["catalog"]], id = "DM", idx="1", res_label = "BOB")
expect_false(fr_res[["isgood"]])

# Just a bad resource label
fr_res = fetch_resource(catalog=DSV[["catalog"]], id = "DM",          res_label = "BOB")
expect_false(fr_res[["isgood"]])

})
