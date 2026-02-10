
# General information

The formods package provides a method for creating shiny modules that are
dependent on each other. The path locations here are described relative to the
root of the package repository:

https://github.com/john-harrold/formods/

The vignettes/making_modules.Rmd file describes the structure of the modules.
Modules are generally described by a short name ASM, UD, DM, etc. and are
configured through yaml files. The default configurations are stored in
inst/templates/XX.yaml where XX is the module short name. Module dependencies
are listed in MC -> module -> depends. For example the ASM module has no
dependencies. The UD module depends on the ASM module. The DW module depends on
the ASM, UD, and DM modules.

General aspects that apply across modules are stored in inst/templates/formods.yaml

#  Preload files

Modules can be preloaded with yaml files. The directory inst/preload provides
examples of these. Typically the files that start with module names (e.g.
DM_preload.yaml) represent only that module, but it is typically used in
conjunction with preload yaml files from the modules it depends on. 

Each module has a function of the format XX_test_mksession() where XX is the
module short name. You can look in these functions to see what example preload
files are used together. For example in the DW_test_mksession() function we can
see that it depends on the other preload files here:

  - inst/preload/ASM_preload.yaml
  - inst/preload/DM_preload.yaml
  - inst/preload/UD_preload.yaml

The file inst/preload/workflow_DW_merge.yaml is a complete workflow and has the
components of all the required modules in it.

Note that in these preload files for the UD and DM modules, they will often
contain source files. For example this file:

  - inst/preload/UD_preload.yaml

It has a reference to a file:

 UD -> data_source -> file_name =  'file.path(getwd(), "TEST_DATA.xlsx")'

 This will be evaluated in the working directory of the shiny app. So if we want
 to run this preload we need the TEST_DATA.xlsx file. For these preload files
 they generally reference test data found in inst/test_data. 

 In this file

  - inst/preload/DM_preload.yaml

Potential files are listed under DM -> sources with each source having a source
type. If it has a source_type of file, then it expects the file to be present.
The source_str points to the location of the file. If it's a relative path then
it's relative to the root of the app. If the source_type is url then the
source_str should contain a valid url for the file.

The preload yaml files have entries for fm_yaml and mod_yaml below the top
level. So in the inst/preload/DM_preload.yaml file these are under DM -> fm_yaml
and DM -> mod_yaml. These are typically located like this:

 - config/formods.yaml
 - config/XX.yaml
 
 When the app starts, it will make copies of the module configurations within
 the config directory in the working direcotry of the app. Typically these are
 copies of files of the same name found in inst/templates.