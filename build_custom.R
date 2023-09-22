rm(list=ls())
library(here)
repo_root = here::here()
setwd(repo_root)


#loading everything
devtools::load_all()

# creating app example
FM_compact = system.file(package="formods", "templates", "FM_compact.R")

fmapp = readLines(FM_compact)
fmapp = c("if(interactive()){",
          fmapp,
          "}")

FM_compact_example = file.path(repo_root, "inst", "test_apps", "FM_compact.R")

fileConn=file(FM_compact_example)
writeLines(fmapp, fileConn)
close(fileConn)

# building documentation
devtools::document(roclets = c('rd', 'collate', 'namespace', 'vignette'))

# Rebuilding the pkgdown site
pkgdown::build_site()

# Fixing any broken image references
art_dir = file.path("docs", "articles")

# Getting all of the html files in the article dir
htds = dir(art_dir, "*.html")

for(htd in htds){
  fn = file.path(art_dir, htd)

  cfn = file(fn, open="r")
  htd_lines = readLines(cfn)
  close(cfn)

  # For some reason it's doing this weird relative path thing, so I'm stripping that out here:
  trim_txt = "../../../../../../My%20Drive/projects/formods/github/nlmixr2rpt/articles/"
  htd_lines = gsub(trim_txt, "", htd_lines)

  write(htd_lines, file=fn, append=FALSE)

}
