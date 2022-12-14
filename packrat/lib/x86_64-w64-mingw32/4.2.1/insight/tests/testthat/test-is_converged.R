if (requiet("testthat") && requiet("insight") && requiet("lme4")) {
  data(cbpp)
  data(sleepstudy)
  set.seed(1)
  cbpp$x <- rnorm(nrow(cbpp))
  cbpp$x2 <- runif(nrow(cbpp))

  model <- suppressWarnings(glmer(
    cbind(incidence, size - incidence) ~ period + x + x2 + (1 + x | herd),
    data = cbpp,
    family = binomial()
  ))

  test_that("is_converged", {
    expect_true(is_converged(model))
    expect_equal(is_converged(model), structure(TRUE, gradient = 0.000280307452338331), tolerance = 1e-3)
  })

  model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

  test_that("is_converged", {
    expect_true(is_converged(model))
  })

  if (requiet("glmmTMB")) {
    model <- glmmTMB(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
    test_that("is_converged, glmmTMB", {
      expect_true(is_converged(model))
    })
  }
}
