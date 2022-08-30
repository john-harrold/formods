TMPDS = readxl::read_excel(
  path  = system.file(package="formods", "data", "TEST_DATA.xlsx"),
  sheet = "DATA") %>%
  dplyr::filter(EVID==0)

FG_myp_1 = ggplot2::ggplot(data=TMPDS)
FG_myp_1 = FG_myp_1 + geom_line(aes(x=ID, y=ID))
FG_myp_1 = FG_myp_1 + ggforce::facet_wrap_paginate(vars(ROUTE), nrow = 3, ncol = 4, page=1)
FG_myp_1 = FG_myp_1 + theme_light()

fobj = FG_myp_1

tcres =
  FM_tc(tc_env = list(fobj=fobj),
        cmd = "fbuild = ggplot2::ggplot_build(fobj); print(fbuild) ",
        capture = c("fbuild"))

a  = ggplot2::ggplot_build(fobj)
