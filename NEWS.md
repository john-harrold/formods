# formods 0.3.0 (development version)

* Added default colors (yellow and orange).
* Moved data loading/saving over to `{rio}`.
* Added support for sas and sas7bdat files.
* Fixed bug in FG module where a figure built on a data view crashes the app when the data view is deleted.
* Added a return list element to `FM_fetch_ds()` and `FM_fetch_mdl()` to include the choices.
* Added filed in module section of the module yaml file to include description of the module to be associated with assets that module provides. E.g. if the DW module provides datasets we can assoicate those with the DW module in UI elements like data source selection..
* Added STDM test datasets for the PC, EX and DM domains.
* Added `FM_has_ds_changed()` to detect changes in datasets
* Moved workflow elements from UD to ASM and created testing functions `ASM_test_preload()` and `ASM_check_workflow()`. The function `ASM_test_preload()` can be used to automate testing of preload files and workflows.
* Added DM module to handle multpile files 
* Added merging to DW Module and the ability to use previously defined data views in the current data view.
* Added empty preload files for testing purposes 
* Modified `FM_tc()` to capture all objects generated when no capture is specified.

# formods 0.2.2

* Fixed issue with ggplot2 version 4+ that prevented figures from being rendered properly in the UI

# formods 0.2.1

* Changed ggplot build tests to fix breaking changes in the next version of `ggplot2`

# formods 0.2.0 

* Created preload functionality to allow modules to save to and be loaded from yaml files.
* Updated the `ZZ_Server.R` to include save/load of preload yaml files.
* Updated the save/load functionality for the ASM module to switch to use this functionality (this breaks the previous save methodology.)
* Creating workflows to automate analysis initialization .
* Updated FM_compact.R test app to use the preload script in an unzipped saved
  analysis.
* Created `is_shiny()` function. 
* Created `run_formods()` function to run the test app. 


# formods 0.1.7 

* Added word placeholders to the UI so the user can change them when generating reports/saving the app state.
* Fixed issue with `has_updated()` where zero values from ui inputs were not taking effect because of a special case with buttons. This requires an `is_button=TRUE` argument for buttons.  
* Updated the `ZZ_Server.R` template to use `has_updated()`.

# formods 0.1.6 

* Fixed (finallly) the issue where the dataset sources in the FG module are not updating properly

# formods 0.1.5

* Fixes issues with loading analyses breaking the app.
* Added modals and notifications to  loading analyses.
* Added the ability to create XX_onload() functions to give modules the ability to update the state when loading a saved analysis.
* Fixed `FG_append_report()` to account for when there are no reportable elements present.
* Added code to capture errors in `FM_generate_report()`.

# formods 0.1.4

* Added h1, h2 and h3 to `FM_message()`.
* Added `FM_fetch_mdl()`.
* The `icon_link()` function can handle NULL input.
* Added `has_updated()`.
* Added copy to clipboard functionality to the ZZ_Server.R template.
* Added `NULL` support for `autocast()`.
* Abstracted function examples in module template out into a separate file (inst/templates/ZZ_funcs.R).
* Added interface to fetch models from FM modules.
* Added modals to individual report generation buttons.
* Fixed bug preventing FG module from working in shiny 1.8.1

# formods 0.1.3 

* Fixed bug preventing the same file name from being uploaded.
* Fixed bug where user files were stored in the same location for different sessions.

# formods 0.1.2

* Added `new_module_template()` to create new module templates and `use_formods()` 
to automatically add the files to a package. 
* Updated app info in ASM to split up the uiele into diferent components. 

# formods 0.1.1 

* Initial release
