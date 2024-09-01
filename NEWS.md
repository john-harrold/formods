# formods 0.1.7 (development version)

* Added word placeholders to the UI so the user can change them when generating reports/saving the app state.
* Fixed issue with `has_updated()` where zero values from ui inputs were not taking effect because of a special case with buttons. This requires an `is_button=TRUE` argument for buttons.  
* Updated the `ZZ_Server.R` template to use `has_updated()`

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
