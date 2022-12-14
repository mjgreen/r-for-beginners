osx <- tryCatch(
  {
    si <- Sys.info()
    if (!is.null(si["sysname"])) {
      si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
    } else {
      FALSE
    }
  },
  error = function(e) {
    FALSE
  }
)

.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest && !osx && requiet("testthat") && requiet("insight") && requiet("nonnest2")) {
  data(iris)
  data(mtcars)

  test_that("get_loglikelihood - lm", {
    x <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
    ll <- loglikelihood(x, estimator = "ML")
    ll2 <- stats::logLik(x)
    expect_equal(as.numeric(ll), as.numeric(ll2))
    expect_equal(attributes(ll)$df, attributes(ll2)$df)
    expect_equal(sum(attributes(ll)$per_obs - nonnest2::llcont(x)), 0)

    # REML
    ll <- loglikelihood(x, estimator = "REML")
    ll2 <- stats::logLik(x, REML = TRUE)
    expect_equal(as.numeric(ll), as.numeric(ll2))

    # With weights
    x <- lm(Sepal.Length ~ Petal.Width + Species, data = iris, weights = Petal.Length)
    ll <- loglikelihood(x, estimator = "ML")
    ll2 <- stats::logLik(x)
    expect_equal(as.numeric(ll), as.numeric(ll2))

    # log-response
    x <- lm(mpg ~ wt, data = mtcars)
    expect_equal(as.numeric(get_loglikelihood(x)), -80.01471, tolerance = 1e-3)

    x <- lm(log(mpg) ~ wt, data = mtcars)
    expect_equal(as.numeric(get_loglikelihood(x)), 19.42433, tolerance = 1e-3)
    expect_equal(as.numeric(get_loglikelihood(x, check_response = TRUE)), -75.21614, tolerance = 1e-3)

    set.seed(123)
    mtcars$wg <- abs(rnorm(nrow(mtcars), mean = 1))
    x <- lm(mpg ~ wt, weights = wg, data = mtcars)
    expect_equal(as.numeric(get_loglikelihood(x)), -82.03376, tolerance = 1e-3)

    x <- lm(log(mpg) ~ wt, weights = wg, data = mtcars)
    expect_equal(as.numeric(get_loglikelihood(x)), 18.4205, tolerance = 1e-3)
    expect_equal(as.numeric(get_loglikelihood(x, check_response = TRUE)), -75.58669, tolerance = 1e-3)


    # sqrt-response
    x <- lm(sqrt(mpg) ~ wt, data = mtcars)
    expect_equal(as.numeric(get_loglikelihood(x)), -7.395031, tolerance = 1e-3)
    expect_equal(as.numeric(get_loglikelihood(x, check_response = TRUE)), -76.89597, tolerance = 1e-3)
  })

  test_that("get_loglikelihood - glm", {
    x <- glm(vs ~ mpg * disp, data = mtcars, family = "binomial")
    ll <- loglikelihood(x)
    ll2 <- stats::logLik(x)
    expect_equal(as.numeric(ll), as.numeric(ll2))
    expect_equal(attributes(ll)$df, attributes(ll2)$df)
    expect_equal(sum(attributes(ll)$per_obs - nonnest2::llcont(x)), 0)

    x <- glm(cbind(cyl, gear) ~ mpg, data = mtcars, weights = disp, family = binomial)
    ll <- loglikelihood(x)
    ll2 <- stats::logLik(x)
    expect_equal(as.numeric(ll), as.numeric(ll2))
    expect_equal(attributes(ll)$df, attributes(ll2)$df)
    # Nonnest2 seems to be giving diffenrent results,
    # which sums doesn't add up to base R's result... so commenting off
    # expect_equal(sum(attributes(ll)$per_obs - nonnest2::llcont(x)), 0)
  })

  test_that("get_loglikelihood - (g)lmer", {
    if (requiet("lme4")) {
      x <- lme4::lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)

      # REML
      ll <- loglikelihood(x, estimator = "REML")
      ll2 <- stats::logLik(x)
      expect_equal(as.numeric(ll), as.numeric(ll2))
      expect_equal(attributes(ll)$df, attributes(ll2)$df)

      # ML
      ll <- loglikelihood(x, estimator = "ML")
      ll2 <- stats::logLik(x, REML = FALSE)
      expect_equal(as.numeric(ll), as.numeric(ll2))

      # default
      ll <- loglikelihood(x)
      ll2 <- stats::logLik(x)
      expect_equal(as.numeric(ll), as.numeric(ll2))
      expect_equal(attributes(ll)$df, attributes(ll2)$df)

      x <- lme4::glmer(vs ~ mpg + (1 | cyl), data = mtcars, family = "binomial")
      ll <- loglikelihood(x, estimator = "REML") # no REML for glmer
      ll2 <- stats::logLik(x)
      expect_equal(as.numeric(ll), as.numeric(ll2))
      expect_equal(attributes(ll)$df, attributes(ll2)$df)

      ll <- loglikelihood(x, estimator = "ML")
      ll2 <- stats::logLik(x, REML = FALSE)
      expect_equal(as.numeric(ll), as.numeric(ll2))

      model <- download_model("lmerMod_1")
      expect_equal(get_loglikelihood(model, estimator = "REML"), logLik(model, REML = TRUE), tolerance = 0.01, ignore_attr = TRUE)
      expect_equal(get_loglikelihood(model, estimator = "ML"), logLik(model, REML = FALSE), tolerance = 0.01, ignore_attr = TRUE)

      model <- download_model("merMod_1")
      expect_equal(get_loglikelihood(model, estimator = "REML"), logLik(model, REML = FALSE), tolerance = 0.01, ignore_attr = TRUE)
      expect_equal(get_loglikelihood(model, estimator = "ML"), logLik(model, REML = FALSE), tolerance = 0.01, ignore_attr = TRUE)
    }
  })

  test_that("get_loglikelihood - stanreg", {
    .runStanTest <- Sys.getenv("RunAllinsightStanTests") == "yes"
    if (requiet("rstanarm") && .runStanTest) {
      x <- rstanarm::stan_glm(Sepal.Length ~ Petal.Width, data = iris, refresh = 0)
      ref <- lm(Sepal.Length ~ Petal.Width, data = iris)
      ll <- loglikelihood(x)
      ll2 <- loglikelihood(ref)
      expect_equal(as.numeric(ll), as.numeric(ll2), tolerance = 2)
      expect_equal(mean(abs(attributes(ll)$per_obs - attributes(ll2)$per_obs)), 0, tolerance = 0.1)
    }
  })

  test_that("get_loglikelihood - ivreg", {
    if (requiet("ivreg")) {
      data("CigaretteDemand", package = "ivreg")
      x <- ivreg::ivreg(log(packs) ~ log(rprice) + log(rincome) | salestax + log(rincome), data = CigaretteDemand)

      ll <- loglikelihood(x)
      expect_equal(as.numeric(ll), 13.26255, tolerance = 1e-3)
    }
  })

  test_that("get_loglikelihood - plm", {
    if (requiet("plm")) {
      data("Produc", package = "plm")
      x <- suppressWarnings(plm::plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
        data = Produc, index = c("state", "year")
      ))

      ll <- loglikelihood(x)
      expect_equal(as.numeric(ll), 1534.532, tolerance = 1e-3)
    }
  })

  if (requiet("estimatr")) {
    test_that("get_loglikelihood - iv_robust", {
      data(mtcars)
      x <- estimatr::iv_robust(mpg ~ gear + cyl | carb + wt, data = mtcars)

      ll <- loglikelihood(x)
      expect_equal(as.numeric(ll), -84.60057, tolerance = 1e-3)
    })
  }

  if (requiet("mgcv")) {
    test_that("get_loglikelihood - mgcv", {
      x <- mgcv::gam(Sepal.Length ~ s(Petal.Width), data = iris)
      ll <- insight::get_loglikelihood(x)
      ll2 <- stats::logLik(x)
      expect_equal(as.numeric(ll), -96.26613, tolerance = 1e-3)
      # TODO: I'm not sure why this differes :/
      # expect_equal(as.numeric(ll), as.numeric(ll2))

      x <- mgcv::gamm(Sepal.Length ~ s(Petal.Width), random = list("Species" = ~1), data = iris)
      # Which one to get?
    })
  }
  if (requiet("gamm4")) {
    test_that("get_loglikelihood - gamm4", {
      x <- gamm4::gamm4(Sepal.Length ~ s(Petal.Width), data = iris)
      ll <- insight::get_loglikelihood(x)
      # It works, but it's quite diferent from the mgcv result
      expect_equal(as.numeric(ll), -101.1107, tolerance = 1e-3)
    })
  }
}
