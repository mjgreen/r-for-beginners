skip_on_os(os = "mac")

is_dev_version <- length(strsplit(packageDescription("insight")$Version, "\\.")[[1]]) > 3
run_stan <- .Platform$OS.type == "unix" && is_dev_version

pkgs <- c(
  "lme4",
  "brms",
  "glmmTMB",
  "pscl",
  "mgcv",
  "gamm4",
  "merTools",
  "emmeans",
  "bayestestR",
  "mclust",
  "rstanarm",
  "rstantools",
  "psych"
)
invisible(sapply(pkgs, requiet))


# LM and GLM --------------------------------------------------------------
# =========================================================================

test_that("get_predicted - lm", {
  skip_if(isFALSE(run_stan))
  skip_if_not_installed("rstanarm")

  x <- lm(mpg ~ cyl + hp, data = mtcars)

  # Link vs. relation
  rezlink <- get_predicted(x, predict = "link", ci = .95)
  rezrela <- get_predicted(x, predict = "expectation", ci = .95)
  expect_equal(mean(abs(rezlink - rezrela)), 0, tolerance = 1e-3)
  expect_equal(mean(summary(rezlink)$CI_high - summary(rezrela)$CI_high), 0, tolerance = 1e-3)

  # Relation vs. Prediction
  rezpred <- get_predicted(x, predict = "prediction", ci = .95)
  expect_equal(mean(abs(rezlink - rezpred)), 0, tolerance = 1e-3)
  expect_true(all(mean(summary(rezlink)$CI_high - summary(rezpred)$CI_high) < 0))

  # Confidence
  ref <- predict(x, se.fit = TRUE, interval = "confidence")
  rez <- as.data.frame(get_predicted(x, predict = "expectation", ci = .95))
  expect_equal(nrow(rez), 32)
  expect_equal(max(abs(as.data.frame(ref$fit)$fit - rez$Predicted)), 0, tolerance = 1e-10)
  expect_equal(max(abs(ref$se.fit - rez$SE)), 0, tolerance = 1e-10)
  expect_equal(max(abs(as.data.frame(ref$fit)$lwr - rez$CI_low)), 0, tolerance = 1e-10)

  # Prediction
  ref <- predict(x, newdata = insight::get_data(x), se.fit = TRUE, interval = "prediction")
  rez <- as.data.frame(get_predicted(x, predict = "prediction", ci = .95))
  expect_equal(nrow(rez), 32)
  expect_equal(max(abs(as.data.frame(ref$fit)$fit - rez$Predicted)), 0, tolerance = 1e-10)
  expect_equal(max(abs(as.data.frame(ref$fit)$lwr - rez$CI_low)), 0, tolerance = 1e-10)

  # Bootstrap
  set.seed(333)
  ref <- predict(x, newdata = insight::get_data(x), se.fit = TRUE, interval = "confidence")
  rez <- get_predicted(x, iterations = 600, ci = .95)
  expect_equal(length(rez), 32)
  expect_null(nrow(rez))
  expect_equal(mean(abs(as.data.frame(ref$fit)$fit - summary(rez)$Predicted)), 0, tolerance = 0.1)
  expect_equal(mean(abs(as.data.frame(ref$fit)$lwr - summary(rez)$CI_low)), 0, tolerance = 0.5)
  # TODO: Is it possible to get "prediction" CIs via bootstrapping?

  # vs. Bayesian
  xbayes <- rstanarm::stan_glm(mpg ~ cyl + hp, data = mtcars, refresh = 0, seed = 333)
  rez <- as.data.frame(get_predicted(x, predict = "link", ci = .95))
  rezbayes <- summary(get_predicted(xbayes, predict = "link", ci = .95))
  expect_equal(mean(abs(rez$Predicted - rezbayes$Predicted)), 0, tolerance = 0.1)
  expect_equal(mean(abs(rez$CI_low - rezbayes$CI_low)), 0, tolerance = 0.1)
  rez <- as.data.frame(get_predicted(x, predict = "prediction", ci = .95))
  rezbayes <- summary(get_predicted(xbayes, predict = "prediction", ci = .95))
  expect_equal(mean(abs(rez$Predicted - rezbayes$Predicted)), 0, tolerance = 0.1)
  expect_equal(mean(abs(rez$CI_low - rezbayes$CI_low)), 0, tolerance = 0.2)
})


test_that("get_predicted - glm", {
  skip_if(isFALSE(run_stan))
  skip_if_not_installed("rstanarm")

  x <- glm(vs ~ wt, data = mtcars, family = "binomial")

  # Link vs. relation
  rezlink <- get_predicted(x, predict = "link", ci = .95)
  rezrela <- get_predicted(x, predict = "expectation", ci = .95)
  expect_true(min(rezlink) < 0)
  expect_true(min(rezrela) > 0)
  expect_true(min(summary(rezlink)$CI_low) < 0)
  expect_true(min(summary(rezrela)$CI_low) > 0)

  # Relation vs. Prediction
  rezrela <- get_predicted(x, predict = "expectation", ci = .95)
  rezpred <- get_predicted(x, predict = "prediction", ci = .95)
  expect_equal(mean(abs(rezrela - rezpred)), 0, tolerance = 1e-3)
  expect_true(all(mean(summary(rezrela)$CI_high - summary(rezpred)$CI_high) < 0))

  # Against stats::predict
  ref <- predict(x, se.fit = TRUE, type = "response", ci = .95)
  rez <- as.data.frame(get_predicted(x, predict = "expectation", ci = .95))
  expect_equal(nrow(rez), 32)
  expect_equal(max(abs(ref$fit - rez$Predicted)), 0, tolerance = 1e-4)
  expect_equal(max(abs(ref$se.fit - rez$SE)), 0, tolerance = 1e-4)
  ref <- as.data.frame(suppressWarnings(insight::link_inverse(x)(predict.lm(x, interval = "confidence"))))
  expect_equal(max(abs(ref$lwr - rez$CI_low)), 0, tolerance = 1e-2)

  ref <- predict(x, se.fit = TRUE, type = "link")
  rez <- as.data.frame(get_predicted(x, predict = "link", ci = .95))
  expect_equal(nrow(rez), 32)
  expect_equal(max(abs(ref$fit - rez$Predicted)), 0, tolerance = 1e-4)
  expect_equal(max(abs(ref$se.fit - rez$SE)), 0, tolerance = 1e-4)

  # Bootstrap
  set.seed(333)
  ref <- suppressWarnings(predict(x, se.fit = TRUE, type = "response"))
  rez <- suppressWarnings(summary(get_predicted(x, iterations = 800, verbose = FALSE, ci = .95)))
  expect_equal(mean(abs(ref$fit - rez$Predicted)), 0, tolerance = 0.1)

  # vs. Bayesian
  xbayes <- rstanarm::stan_glm(vs ~ wt, data = mtcars, family = "binomial", refresh = 0, seed = 333)
  rez <- as.data.frame(get_predicted(x, predict = "link", ci = .95))
  rezbayes <- summary(get_predicted(xbayes, predict = "link", ci = .95))
  expect_equal(mean(abs(rez$Predicted - rezbayes$Predicted)), 0, tolerance = 0.1)
  expect_equal(mean(abs(rez$CI_low - rezbayes$CI_low)), 0, tolerance = 0.1)
  rez <- as.data.frame(get_predicted(x, predict = "prediction", ci = .95))
  rezbayes <- summary(get_predicted(xbayes, predict = "prediction", ci = .95))
  expect_equal(mean(abs(rez$Predicted - rezbayes$Predicted)), 0, tolerance = 0.1)
  # expect_equal(mean(abs(rez$CI_low - rezbayes$CI_low)), 0, tolerance = 0.3)
})

test_that("get_predicted - lm (log)", {
  x <- lm(mpg ~ log(hp), data = mtcars)
  rez <- insight::get_predicted(x)
  expect_equal(length(rez), 32)

  expect_equal(max(abs(rez - stats::fitted(x))), 0)
  expect_equal(max(abs(rez - stats::predict(x))), 0)

  data <- as.data.frame(rez)
  expect_equal(max(abs(rez - data$Predicted)), 0)
  expect_equal(nrow(data), 32)
})


test_that("robust vcov", {
  requiet("sandwich")
  mod <- lm(mpg ~ hp, data = mtcars)
  se0 <- insight:::get_predicted_se(mod)
  se1 <- suppressWarnings(insight:::get_predicted_se(mod, vcov_estimation = "HC"))
  se2 <- suppressWarnings(insight:::get_predicted_se(mod, vcov_estimation = "HC", vcov_type = "HC3"))
  se3 <- insight:::get_predicted_se(mod, vcov = "HC", vcov_args = list(type = "HC3"))
  expect_true(all(se0 != se1))
  expect_true(all(se1 == se2))
  expect_true(all(se1 == se3))
  # hardcoded values obtained before vcov_estimation was deprecated
  expect_equal(head(se1), c(
    0.862974605863594, 0.862974605863594,
    1.04476534302177, 0.862974605863594,
    0.942213270105983, 0.911147902473696
  ),
  ignore_attr = TRUE
  )
  # various user inputs
  se1 <- suppressWarnings(insight:::get_predicted_se(mod, vcov_estimation = "HC", vcov_type = "HC2"))
  se2 <- insight:::get_predicted_se(mod, vcov = "HC2")
  se3 <- insight:::get_predicted_se(mod, vcov = "vcovHC", vcov_args = list(type = "HC2"))
  se4 <- insight:::get_predicted_se(mod, vcov = sandwich::vcovHC, vcov_args = list(type = "HC2"))
  expect_true(all(se1 == se2))
  expect_true(all(se1 == se3))
  expect_true(all(se1 == se4))
  se1 <- insight:::get_predicted_se(mod, vcov = "HC1")
  se2 <- insight:::get_predicted_se(mod, vcov = sandwich::vcovHC, vcov_args = list(type = "HC1"))
  expect_true(all(se1 == se2))
})



test_that("MASS::rlm", {
  requiet("MASS")
  mod <- rlm(mpg ~ hp + am, data = mtcars)
  p <- get_predicted.default(mod)
  expect_s3_class(p, "get_predicted")
  p <- data.frame(p)
  expect_true("CI_low" %in% colnames(p))
})



# Mixed --------------------------------------------------------------
# =========================================================================

test_that("get_predicted - lmerMod", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("merTools")
  skip_if_not_installed("rstanarm")
  skip_if(isFALSE(run_stan))
  x <- lme4::lmer(mpg ~ am + (1 | cyl), data = mtcars)

  # Link vs. relation
  rezlink <- get_predicted(x, predict = "link", ci = .95)
  rezrela <- get_predicted(x, predict = "expectation", ci = .95)
  rezpred <- get_predicted(x, predict = "prediction", ci = .95)
  expect_equal(mean(abs(rezlink - rezrela)), 0, tolerance = 1e-3)
  expect_equal(mean(summary(rezlink)$CI_high - summary(rezrela)$CI_high), 0, tolerance = 1e-3)
  expect_true(all(summary(rezlink)$CI_high - summary(rezpred)$CI_high < 0))

  # Bootstrap
  set.seed(333)
  rez <- as.data.frame(get_predicted(x, iterations = 5, ci = .95))
  expect_equal(c(nrow(rez), ncol(rez)), c(32, 9))


  # Compare to merTools
  rez_merTools <- merTools::predictInterval(x, level = 0.95, seed = 333, n.sims = 2000)
  expect_equal(mean(abs(as.data.frame(rezpred)$CI_low - rez_merTools$lwr)), 0, tolerance = 0.5)


  # Compare to emmeans (not sure what it does)
  # refgrid <- emmeans::ref_grid(x, at = as.list(get_data(x)), data = get_data(x))
  # rez_emmeans <- as.data.frame(predict(refgrid, level = 0.95, interval = "prediction"))
  # This is completely off
  # expect_equal(mean(as.data.frame(rez)$CI_low - rez_emmeans$lower.PL), 0, tolerance = 0.5)

  # Compare with glmmTMB
  ref <- insight::get_predicted(glmmTMB::glmmTMB(mpg ~ am + (1 | cyl), data = mtcars), predict = "expectation", ci = .95)
  expect_equal(mean(abs(rezrela - ref)), 0, tolerance = 0.1) # A bit high
  # expect_equal(mean(abs(as.data.frame(rezrela)$SE - as.data.frame(ref)$SE)), 0, tolerance = 1e-5)
  # expect_equal(mean(abs(as.data.frame(rezrela)$CI_low - as.data.frame(ref)$CI_low)), 0, tolerance = 1e-5)

  # Compare with rstanarm
  xref <- suppressWarnings(
    rstanarm::stan_lmer(mpg ~ am + (1 | cyl),
      data = mtcars,
      refresh = 0, iter = 1000, seed = 333
    )
  )
  rez_stan <- insight::get_predicted(xref, predict = "expectation", ci = .95)
  expect_equal(mean(abs(rezrela - rez_stan)), 0, tolerance = 0.1)
  # Different indeed
  # expect_equal(mean(as.data.frame(rezrela)$CI_low - as.data.frame(rez_stan)$CI_low), 0, tolerance = 0.5)
})


test_that("get_predicted - glmer with matrix response", {
  skip_if_not_installed("lme4")
  model <- glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp, family = binomial
  )
  grid <- get_datagrid(model, at = "period", range = "grid", preserve_range = FALSE)
  out <- as.data.frame(get_predicted(model, data = grid, ci = .95))
  expect_equal(out$Predicted, c(0.19808, 0.08392, 0.07402, 0.04843), tolerance = 1e-3)
  expect_equal(out$CI_low, c(0.1357, 0.04775, 0.0404, 0.02164), tolerance = 1e-3)
})


test_that("get_predicted - lmerMod (log)", {
  skip_if_not_installed("lme4")
  x <- lme4::lmer(mpg ~ am + log(hp) + (1 | cyl), data = mtcars)
  rez <- insight::get_predicted(x)
  expect_equal(length(rez), 32)

  expect_equal(max(abs(rez - stats::fitted(x))), 0)
  expect_equal(max(abs(rez - stats::predict(x))), 0)
  expect_equal(nrow(as.data.frame(rez)), 32)

  # No random
  rez2 <- insight::get_predicted(x, newdata = mtcars[c("am", "hp")], verbose = FALSE)
  expect_true(!all(is.na(as.data.frame(rez2))))
})


test_that("get_predicted - merMod", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  x <- lme4::glmer(vs ~ am + (1 | cyl), data = mtcars, family = "binomial")
  rezlink <- get_predicted(x, predict = "link", ci = .95)
  rezrela <- get_predicted(x, predict = "expectation", ci = .95)
  expect_true(min(rezlink) < 0)
  expect_true(min(rezrela) > 0)
  expect_true(min(summary(rezlink)$CI_low) < 0)
  expect_true(min(summary(rezrela)$CI_low) > 0)
  expect_equal(max(abs(rezrela - stats::fitted(x))), 0)
  expect_equal(max(abs(rezrela - stats::predict(x, type = "response"))), 0)
  expect_equal(nrow(as.data.frame(rezlink)), 32)

  # Compare with glmmTMB
  xref <- glmmTMB::glmmTMB(vs ~ am + (1 | cyl), data = mtcars, family = "binomial")
  rez_ref <- insight::get_predicted(xref, predict = "expectation", ci = .95)
  expect_equal(max(abs(rezrela - rez_ref)), 0, tolerance = 1e-5)
  expect_equal(mean(abs(as.data.frame(rezrela)$SE - as.data.frame(rez_ref)$SE)), 0, tolerance = 0.2)
})


test_that("get_predicted - glmmTMB", {
  skip_if_not_installed("glmmTMB")
  x <- glmmTMB::glmmTMB(mpg ~ am + (1 | cyl), data = mtcars)

  # Link vs. relation
  rezlink <- get_predicted(x, predict = "link", ci = .95)
  rezrela <- get_predicted(x, predict = "expectation", ci = .95)
  expect_equal(mean(abs(rezlink - rezrela)), 0, tolerance = 1e-3)
  expect_equal(mean(summary(rezlink)$CI_high - summary(rezrela)$CI_high), 0, tolerance = 1e-3)

  # Bootstrap
  set.seed(333)
  rez <- as.data.frame(get_predicted(x, iterations = 5, predict = "link", ci = .95))
  expect_equal(c(nrow(rez), ncol(rez)), c(32, 9))

  # Binomial
  x <- glmmTMB::glmmTMB(vs ~ am + (1 | cyl), data = mtcars, family = "binomial")
  rezlink <- get_predicted(x, predict = "link", ci = .95)
  rezrela <- get_predicted(x, predict = "expectation", ci = .95)
  expect_true(min(rezlink) < 0)
  expect_true(min(rezrela) > 0)
  expect_true(min(summary(rezlink)$CI_low) < 0)
  expect_true(min(summary(rezrela)$CI_low) > 0)
  expect_equal(max(abs(rezrela - stats::fitted(x))), 0)
  expect_equal(max(abs(rezrela - stats::predict(x, type = "response"))), 0)
  expect_equal(nrow(as.data.frame(rez)), 32)

  # No random
  rez <- insight::get_predicted(x, newdata = mtcars[c("am")], verbose = FALSE, ci = .95)
  expect_true(!all(is.na(as.data.frame(rez))))
  x <- glmmTMB::glmmTMB(Petal.Length ~ Petal.Width + (1 | Species), data = iris)
  rez <- insight::get_predicted(x, data = data.frame(Petal.Width = c(0, 1, 2)), verbose = FALSE)
  expect_equal(length(rez), 3)

  # vs. Bayesian
  # x <- glmmTMB::glmmTMB(mpg ~ am + (1 | cyl), data = mtcars)
  # rez <- summary(insight::get_predicted(x))
  # xref <- rstanarm::stan_lmer(mpg ~ am + (1 | cyl), data = mtcars, refresh = 0, iter = 1000, seed = 333)
  # rezbayes <- summary(insight::get_predicted(xref))
  # expect_equal(mean(abs(rez$Predicted - rezbayes$Predicted)), 0, tolerance = 0.1)
  # expect_equal(mean(abs(rez$CI_low - rezbayes$CI_low)), 0, tolerance = 0.2)
})


# GAM --------------------------------------------------------------
# =========================================================================
test_that("get_predicted - mgcv::gam and gamm", {
  skip_if_not_installed("mgcv")
  x <- mgcv::gam(mpg ~ am + s(wt), data = mtcars)
  expect_equal(length(insight::get_predicted(x, ci = .95)), 32)
  rez <- insight::get_predicted(x, data = data.frame(am = c(0, 0, 1), wt = c(2, 3, 4)), ci = .95)
  expect_equal(length(rez), 3)

  # No smooth
  rez <- insight::get_predicted(x, data = data.frame(am = c(0, 0, 1)), ci = .95)
  expect_equal(length(rez), 3)
  rez2 <- insight::get_predicted(x, data = data.frame(am = c(0, 0, 1), wt = c(2, 3, 4)), ci = .95, include_smooth = FALSE)
  expect_equal(max(abs(as.numeric(rez - rez2))), 0, tolerance = 1e-4)
  expect_equal(length(unique(attributes(rez)$data$wt)), 1)

  # Bootstrap
  set.seed(333)
  rez <- summary(get_predicted(x, iterations = 50, ci = .95))
  expect_equal(nrow(rez), 32)

  # Binomial
  x <- mgcv::gam(vs ~ am + s(wt), data = mtcars, family = "binomial")
  rez <- insight::get_predicted(x, ci = .95)
  expect_equal(length(rez), 32)

  expect_equal(max(abs(rez - stats::fitted(x))), 0)
  expect_equal(max(abs(rez - stats::predict(x, type = "response"))), 0)
  expect_equal(nrow(as.data.frame(rez)), 32)

  # GAMM
  x <- mgcv::gamm(vs ~ am + s(wt), random = list(cyl = ~1), data = mtcars, family = "binomial", verbosePQL = FALSE)
  rez <- insight::get_predicted(x, ci = .95)
  expect_equal(length(rez), 32)
  expect_equal(max(abs(rez - x$gam$fitted.values)), 0)
  expect_equal(max(abs(rez - stats::predict(x$gam, type = "response"))), 0)
  expect_equal(nrow(as.data.frame(rez)), 32)
})


# Bayesian --------------------------------------------------------------
# =========================================================================


test_that("get_predicted - rstanarm", {
  skip_if(isFALSE(run_stan))
  skip_if_not_installed("rstanarm")

  # LM
  x <- rstanarm::stan_glm(mpg ~ cyl + hp, data = mtcars, refresh = 0, seed = 333)
  rezlink <- summary(get_predicted(x, predict = "link", ci = .95))
  rezrela <- summary(get_predicted(x, predict = "expectation", ci = .95))
  expect_equal(mean(abs(rezlink$Predicted - rezrela$Predicted)), 0, tolerance = 0.1)
  expect_equal(mean(abs(rezlink$CI_high - rezrela$CI_high)), 0, tolerance = 0.1)
  rezpred <- summary(get_predicted(x, predict = "prediction", ci = .95))
  expect_equal(mean(abs(rezlink$Predicted - rezpred$Predicted)), 0, tolerance = 0.1)
  expect_true(all(mean(rezlink$CI_high - rezpred$CI_high) < 0))

  # GLM
  x <- rstanarm::stan_glm(vs ~ wt, data = mtcars, family = "binomial", refresh = 0, seed = 333)
  rezlink <- summary(get_predicted(x, predict = "link", ci = .95))
  rezrela <- summary(get_predicted(x, predict = "expectation", ci = .95))
  expect_true(min(rezlink$Predicted) < 0)
  expect_true(min(rezrela$Predicted) > 0)
  expect_true(min(rezlink$CI_high) < 0)
  expect_true(min(rezrela$CI_high) > 0)
  rezpred <- summary(get_predicted(x, predict = "prediction", ci = .95))
  expect_equal(mean(abs(rezrela$Predicted - rezpred$Predicted)), 0, tolerance = 0.1)
  expect_true(all(mean(rezrela$CI_high - rezpred$CI_high) < 0))

  # Mixed
  x <- suppressWarnings(
    rstanarm::stan_lmer(mpg ~ am + (1 | cyl),
      data = mtcars, refresh = 0,
      seed = 333, iter = 500
    )
  )
  rezrela <- summary(get_predicted(x, predict = "expectation", ci = .95))
  rezpred <- summary(get_predicted(x, predict = "prediction", ci = .95))
  rezrela2 <- summary(get_predicted(x, predict = "expectation", ci = .95, include_random = FALSE))
  rezpred2 <- summary(get_predicted(x, predict = "prediction", ci = .95, include_random = FALSE))
  expect_true(mean(abs(rezrela$Predicted - rezrela2$Predicted)) > 0)
  expect_true(mean(abs(rezpred$Predicted - rezpred2$Predicted)) > 0)
  rezrela3 <- summary(get_predicted(x, predict = "expectation", ci = .95, data = mtcars["am"]), verbose = FALSE)
  expect_equal(mean(abs(rezrela2$Predicted - rezrela3$Predicted)), 0, tolerance = 0.001)
})



# FA / PCA ----------------------------------------------------------------
# =========================================================================

test_that("get_predicted - FA / PCA", {
  skip_if_not_installed("fungible")
  skip_if_not_installed("psych")
  # PCA
  x <- get_predicted(psych::principal(mtcars, 3))
  expect_equal(dim(x), c(32, 3))
  x <- get_predicted(psych::principal(mtcars, 3), data = mtcars[1:5, ])
  expect_equal(dim(x), c(5, 3))

  x <- get_predicted(prcomp(mtcars))
  expect_equal(dim(x), c(32, ncol(mtcars)))
  x <- get_predicted(prcomp(mtcars), data = mtcars[1:5, ])
  expect_equal(dim(x), c(5, ncol(mtcars)))

  # FA
  x <- get_predicted(psych::fa(mtcars, 3))
  expect_equal(dim(x), c(32, 3))
  x <- get_predicted(psych::fa(mtcars, 3), data = mtcars[1:5, ])
  expect_equal(dim(x), c(5, 3))

  expect_error(get_predicted(fungible::faMain(mtcars, numFactors = 3)))
  x <- get_predicted(fungible::faMain(mtcars, numFactors = 3), data = mtcars[1:5, ])
  expect_equal(dim(x), c(5, 3))
})


# arguments: `predict` vs. `type` -----------------------------------------
# =========================================================================

test_that("lm: get_predicted vs barebones `predict()`", {
  mod <- lm(mpg ~ hp + factor(cyl), mtcars)
  known <- predict(mod, se.fit = TRUE, interval = "confidence")
  unknown1 <- as.data.frame(get_predicted(mod, ci = .95))
  unknown2 <- as.data.frame(get_predicted(mod, ci = .95, predict = "expectation"))
  unknown3 <- suppressWarnings(as.data.frame(get_predicted(mod, ci = .95, predict = "response")))
  expect_equal(unknown1$Predicted, known$fit[, "fit"], ignore_attr = TRUE)
  expect_equal(unknown1$SE, known$se.fit, ignore_attr = TRUE)
  expect_equal(unknown1$CI_low, known$fit[, "lwr"], ignore_attr = TRUE)
  expect_equal(unknown1$CI_high, known$fit[, "upr"], ignore_attr = TRUE)
  expect_equal(unknown2$Predicted, known$fit[, "fit"], ignore_attr = TRUE)
  expect_equal(unknown2$SE, known$se.fit, ignore_attr = TRUE)
  expect_equal(unknown2$CI_low, known$fit[, "lwr"], ignore_attr = TRUE)
  expect_equal(unknown2$CI_high, known$fit[, "upr"], ignore_attr = TRUE)
  expect_equal(unknown3$Predicted, known$fit[, "fit"], ignore_attr = TRUE)
  expect_equal(unknown3$SE, known$se.fit, ignore_attr = TRUE)
  expect_equal(unknown3$CI_low, known$fit[, "lwr"], ignore_attr = TRUE)
  expect_equal(unknown3$CI_high, known$fit[, "upr"], ignore_attr = TRUE)
})


test_that("using both `predict` and `type` raises an informative warning", {
  mod <- glm(am ~ hp + factor(cyl), family = binomial, data = mtcars)
  expect_message(get_predicted(mod, predict = "response", type = "response"))
})


test_that("`predict` and `type` are both `NULL`", {
  mod <- glm(am ~ hp + factor(cyl), family = binomial, data = mtcars)
  expect_error(get_predicted(mod, predict = NULL), regexp = "supply")
})


test_that("`predict()` vs. `get_predicted` link equivalence", {
  # link
  mod <- glm(am ~ hp + factor(cyl), family = binomial, data = mtcars)
  known <- predict(mod, type = "link", interval = "confidence", se.fit = TRUE)
  unknown <- as.data.frame(get_predicted(mod, predict = NULL, type = "link", ci = .95))
  expect_equal(unname(known$fit), unknown$Predicted)
  expect_equal(unname(known$se.fit), unknown$SE)

  # response
  mod <- glm(am ~ hp + factor(cyl), family = binomial, data = mtcars)
  known <- predict(mod, type = "response", se.fit = TRUE)
  unknown1 <- as.data.frame(get_predicted(mod, predict = "expectation", ci = .95))
  unknown2 <- as.data.frame(get_predicted(mod, predict = NULL, type = "response", ci = .95))
  unknown3 <- as.data.frame(get_predicted(mod, predict = "response", ci = .95))
  expect_equal(unname(known$fit), unknown1$Predicted)
  expect_equal(unname(known$se.fit), unknown1$SE)
  expect_equal(unname(known$fit), unknown2$Predicted)
  expect_equal(unname(known$se.fit), unknown2$SE)
  expect_equal(unname(known$fit), unknown3$Predicted)
  expect_equal(unname(known$se.fit), unknown3$SE)
})


test_that("hurdle: get_predicted matches `predict()`", {
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl")
  mod <- pscl::hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
  known <- predict(mod, type = "response")
  unknown <- get_predicted(mod, predict = "response", ci = .95, verbose = FALSE)
  expect_equal(known, unknown, ignore_attr = TRUE)
  known <- predict(mod, type = "zero")
  unknown <- get_predicted(mod, predict = "zero", ci = .95, verbose = FALSE)
  expect_equal(known, unknown, ignore_attr = TRUE)
})


test_that("bugfix: used to return all zeros", {
  mod <- glm(am ~ hp + factor(cyl), family = binomial, data = mtcars)
  pred <- get_predicted(mod, predict = "response", ci = .95, verbose = FALSE)
  expect_false(any(pred == 0))
  expect_error(expect_warning(get_predicted(mod, predict = "original", ci = .95, verbose = FALSE)))
})


test_that("brms: `type` in ellipsis used to produce the wrong intervals", {
  skip_if(isFALSE(run_stan))
  skip_if_not_installed("brms")
  library(brms)
  void <- capture.output(
    mod <- brm(am ~ hp + mpg,
      family = bernoulli, data = mtcars,
      chains = 2, iter = 1000, seed = 1024, silent = 2
    )
  )
  x <- get_predicted(mod, predict = "link", ci = .95)
  y <- get_predicted(mod, predict = "expectation", ci = .95)
  z <- get_predicted(mod, predict = NULL, type = "response", ci = .95, verbose = FALSE)
  expect_equal(round(x[1], 1), -1.5, tolerance = 1e-1)
  expect_equal(round(y[1], 1), .2, tolerance = 1e-1)
  expect_equal(y, z, ignore_attr = TRUE)

  data <- mtcars
  data$cyl <- as.character(data$cyl)
  void <- capture.output(
    suppressWarnings(model <- brm(cyl ~ mpg * vs + (1 | carb),
      data = data,
      iter = 1000,
      seed = 1024,
      algorithm = "meanfield",
      refresh = 0,
      family = categorical(link = "logit", refcat = "4")
    ))
  )
  x <- as.data.frame(get_predicted(model, ci = .95))
  # Test shape
  expect_equal(c(nrow(x), ncol(x)), c(32 * 3, 1006))
  # Test whether median point-estimate indeed different from default (mean)
  expect_true(max(x$Predicted - get_predicted(model, centrality_function = stats::median)$Predicted) > 0)
})


test_that("zero-inflation stuff works", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("pscl")

  data("fish")
  m1 <- glmmTMB(
    count ~ child + camper,
    ziformula = ~ child + camper,
    data = fish,
    family = poisson()
  )

  m2 <- zeroinfl(
    count ~ child + camper | child + camper,
    data = fish,
    dist = "poisson"
  )

  p1 <- head(predict(m1, type = "response"))
  p2 <- head(predict(m2, type = "response"))
  p3 <- head(get_predicted(m1))
  p4 <- head(get_predicted(m2))

  expect_equal(p1, p2, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p1, p3, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p1, p4, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p2, p3, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p2, p4, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p3, p4, tolerance = 1e-1, ignore_attr = TRUE)

  m1 <- glmmTMB(
    count ~ child + camper,
    ziformula = ~ child + camper,
    data = fish,
    family = truncated_poisson()
  )

  m2 <- hurdle(
    count ~ child + camper | child + camper,
    data = fish,
    dist = "poisson"
  )

  p1 <- head(predict(m1, type = "response"))
  p2 <- head(predict(m2, type = "response"))
  p3 <- head(get_predicted(m1))
  p4 <- head(get_predicted(m2))

  expect_equal(p1, p2, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p1, p3, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p1, p4, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p2, p3, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p2, p4, tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(p3, p4, tolerance = 1e-1, ignore_attr = TRUE)
})
