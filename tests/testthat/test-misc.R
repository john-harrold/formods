library(ggplot2)
library(testthat)

if(formods::is_installed("plotly")){
  suppressMessages(library(plotly))
  #test_that("Misc -- plotly", {
    df = data.frame(x=c(1:10), y=c(1:10))
    p   = ggplot(data=df) +
          geom_point(aes(x=x, y=y)) +
          xlab("x") + ylab("y")
    testthat::expect_no_error(ggplotly(p, source="my-figure"))
  #})
}
