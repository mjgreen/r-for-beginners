## ---- echo = FALSE----------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  comment = "#"
)

options(
  knitr.kable.NA = "",
  width = 60
)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
}

## ---- results='asis'--------------------------------------
library(report)
library(dplyr)

cite_packages()

## ----results='asis'---------------------------------------
cite_easystats()

