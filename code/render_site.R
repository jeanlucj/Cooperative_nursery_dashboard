## Build the Cooperative Nursery Report webpage into docs/, which is what
## GitHub Pages serves.  Run this after the registry or the T3/Wheat trials
## change:
##
##   Rscript code/render_site.R
##
## Pass refresh = TRUE inside the Rmd's setup chunk (or delete
## data/t3_wheat_studies.rds) to refetch trials from T3/Wheat rather than
## using the cache.

library(tidyverse)

here::i_am("code/render_site.R")

rmarkdown::render(
  here::here("code", "cooperative_nursery_report.Rmd"),
  output_file = "index.html",
  output_dir  = here::here("docs")
)
