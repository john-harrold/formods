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
  #trim_txt = "../../../../../Google%20Drive/Ubiquity/github/ubiquity/vignettes/"
  #trim_txt = "../../../../../My%20Drive/nlmixr2rpt/github/nlmixr2rpt/vignettes/"
  trim_txt = "../../../../../../My%20Drive/projects/nlmixr2rpt/github/nlmixr2rpt/articles/"
  htd_lines = gsub(trim_txt, "", htd_lines)

  write(htd_lines, file=fn, append=FALSE)

}