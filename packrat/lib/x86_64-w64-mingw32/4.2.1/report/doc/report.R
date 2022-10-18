## ---- echo=FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  out.width = "100%",
  comment = "#"
)

options(
  knitr.kable.NA = "",
  width = 60
)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
}

library(dplyr, warn.conflicts = FALSE)
library(report)

## ----eval=FALSE-------------------------------------------
#  install.packages("remotes")
#  remotes::install_github("easystats/report") # You only need to do that once

## ----eval=FALSE-------------------------------------------
#  library("report") # Load the package every time you start R

## ---------------------------------------------------------
report(iris)

## ---------------------------------------------------------
iris %>%
  group_by(Species) %>%
  report_table()

## ---------------------------------------------------------
report(t.test(formula = wt ~ am, data = mtcars))

## ---- eval=FALSE------------------------------------------
#  report(cor.test(mtcars$mpg, mtcars$wt))

## ---------------------------------------------------------
model <- lm(wt ~ am + mpg, data = mtcars)

report(model)

## ---------------------------------------------------------
model <- aov(wt ~ am + mpg, data = mtcars)

report(model)

## ---------------------------------------------------------
model <- glm(vs ~ mpg + cyl, data = mtcars, family = "binomial")

report(model)

## ---------------------------------------------------------
library(lme4)

model <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)

report(model)

