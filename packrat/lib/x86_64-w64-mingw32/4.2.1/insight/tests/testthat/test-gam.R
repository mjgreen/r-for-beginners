.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest) {
  if (requiet("testthat") && requiet("insight") && requiet("mgcv")) {
    set.seed(123)
    void <- capture.output(
      dat <- mgcv::gamSim(1, n = 400, dist = "normal", scale = 2)
    )
    m1 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)

    m2 <- download_model("gam_zi_1")
    m3 <- download_model("gam_mv_1")

    test_that("model_info", {
      expect_true(model_info(m1)$is_linear)
      expect_true(model_info(m2)$is_count)
      expect_true(model_info(m3)$is_multivariate)
    })

    test_that("n_parameters", {
      expect_equal(n_parameters(m1), 5)
      expect_equal(n_parameters(m1, component = "conditional"), 1)
    })

    test_that("clean_names", {
      expect_equal(clean_names(m1), c("y", "x0", "x1", "x2", "x3"))
      expect_equal(clean_names(m2), c("y", "x2", "x3", "x0", "x1"))
      expect_equal(clean_names(m3), c("y0", "y1", "x0", "x1", "x2", "x3"))
    })

    test_that("get_df", {
      expect_equal(
        get_df(m1, type = "residual"),
        df.residual(m1),
        ignore_attr = TRUE
      )
      expect_equal(
        get_df(m1, type = "normal"),
        Inf,
        ignore_attr = TRUE
      )
      expect_equal(
        get_df(m1, type = "wald"),
        383.0491,
        ignore_attr = TRUE,
        tolerance = 1e-3
      )
    })

    test_that("find_predictors", {
      expect_identical(find_predictors(m1), list(conditional = c("x0", "x1", "x2", "x3")))
      expect_identical(
        find_predictors(m1, flatten = TRUE),
        c("x0", "x1", "x2", "x3")
      )
      expect_null(find_predictors(m1, effects = "random"))

      expect_identical(find_predictors(m2), list(conditional = c("x2", "x3"), zero_inflated = c("x0", "x1")))
      expect_identical(find_predictors(m2, flatten = TRUE), c("x2", "x3", "x0", "x1"))
      expect_null(find_predictors(m2, effects = "random"))

      expect_identical(find_predictors(m3), list(y0 = list(conditional = c("x0", "x1")), y1 = list(conditional = c("x2", "x3"))))
      expect_identical(find_predictors(m3, flatten = TRUE), c("x0", "x1", "x2", "x3"))
      expect_null(find_predictors(m3, effects = "random"))
    })

    test_that("find_response", {
      expect_identical(find_response(m1), "y")
      expect_identical(find_response(m2), "y")
      expect_identical(find_response(m3), c(y0 = "y0", y1 = "y1"))
    })

    test_that("find_smooth", {
      expect_identical(find_smooth(m1), list(smooth_terms = c("s(x0)", "s(x1)", "s(x2)", "s(x3)")))
    })

    test_that("get_call", {
      expect_identical(deparse(get_call(m1)), "mgcv::gam(formula = y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)")
    })

    test_that("get_response", {
      expect_equal(get_response(m1), dat$y)
      expect_equal(length(get_response(m2)), 500)
      expect_equal(ncol(get_response(m3)), 2)
    })

    test_that("link_inverse", {
      expect_equal(link_inverse(m1)(.2), .2, tolerance = 1e-5)
      expect_equal(link_inverse(m2)(.2), .2, tolerance = 1e-5)
      expect_equal(link_inverse(m3)(.2), .2, tolerance = 1e-5)
    })

    test_that("get_data", {
      expect_equal(nrow(get_data(m1)), 400)
      expect_equal(colnames(get_data(m1)), c("y", "x0", "x1", "x2", "x3"))
      expect_equal(nrow(get_data(m2)), 500)
      expect_equal(colnames(get_data(m2)), c("y", "x2", "x3", "x0", "x1"))
      expect_equal(nrow(get_data(m3)), 300)
      expect_equal(colnames(get_data(m3)), c("y0", "x0", "x1", "x2", "x3", "y1"))
    })

    test_that("find_formula", {
      expect_length(find_formula(m1), 1)
      expect_equal(
        find_formula(m1),
        list(conditional = as.formula("y ~ s(x0) + s(x1) + s(x2) + s(x3)")),
        ignore_attr = TRUE
      )
      expect_length(find_formula(m2), 2)
      expect_equal(
        find_formula(m2),
        list(
          conditional = as.formula("y ~ s(x2) + s(x3)"),
          zero_inflated = as.formula("~s(x0) + s(x1)")
        ),
        ignore_attr = TRUE
      )
      expect_length(find_formula(m3), 2)
      expect_equal(
        find_formula(m3),
        structure(
          list(
            y0 = list(conditional = as.formula("y0 ~ s(x0) + s(x1)")),
            y1 = list(conditional = as.formula("y1 ~ s(x2) + s(x3)"))
          ),
          is_mv = "1"
        ),
        ignore_attr = TRUE
      )
    })

    test_that("find_variables", {
      expect_equal(find_variables(m1), list(response = "y", conditional = c("x0", "x1", "x2", "x3")))
      expect_equal(find_variables(m1, flatten = TRUE), c("y", "x0", "x1", "x2", "x3"))
      expect_equal(find_variables(m2), list(response = "y", conditional = c("x2", "x3"), zero_inflated = c("x0", "x1")))
      expect_equal(find_variables(m2, flatten = TRUE), c("y", "x2", "x3", "x0", "x1"))
      expect_equal(find_variables(m3), list(response = c(y0 = "y0", y1 = "y1"), y0 = list(conditional = c("x0", "x1")), y1 = list(conditional = c("x2", "x3"))))
      expect_equal(find_variables(m3, flatten = TRUE), c("y0", "y1", "x0", "x1", "x2", "x3"))
    })

    test_that("n_obs", {
      expect_equal(n_obs(m1), 400)
      expect_equal(n_obs(m2), 500)
      expect_equal(n_obs(m3), 300)
    })

    test_that("linkfun", {
      expect_false(is.null(link_function(m1)))
    })

    test_that("find_parameters", {
      expect_equal(
        find_parameters(m1),
        list(
          conditional = "(Intercept)",
          smooth_terms = c("s(x0)", "s(x1)", "s(x2)", "s(x3)")
        )
      )
      expect_equal(nrow(get_parameters(m1)), 5)
      expect_equal(
        get_parameters(m1)$Parameter,
        c("(Intercept)", "s(x0)", "s(x1)", "s(x2)", "s(x3)")
      )
      expect_equal(nrow(get_parameters(m1, "smooth_terms")), 4)

      expect_equal(
        find_parameters(m2),
        list(
          conditional = c("(Intercept)", "(Intercept).1"),
          smooth_terms = c("s(x2)", "s(x3)", "s.1(x0)", "s.1(x1)")
        )
      )
    })

    test_that("is_multivariate", {
      expect_false(is_multivariate(m1))
      expect_false(is_multivariate(m2))
      expect_true(is_multivariate(m3))
    })

    test_that("find_terms", {
      expect_equal(
        find_terms(m1),
        list(
          response = "y",
          conditional = c("s(x0)", "s(x1)", "s(x2)", "s(x3)")
        )
      )
      expect_equal(
        find_terms(m2),
        list(
          response = "y",
          conditional = c("s(x2)", "s(x3)"),
          zero_inflated = c("s(x0)", "s(x1)")
        )
      )
      expect_equal(
        find_terms(m3),
        list(
          y0 = list(response = "y0", conditional = c("s(x0)", "s(x1)")),
          y1 = list(response = "y1", conditional = c("s(x2)", "s(x3)"))
        )
      )
    })

    test_that("find_algorithm", {
      expect_equal(
        find_algorithm(m1),
        list(algorithm = "GCV", optimizer = "magic")
      )
    })

    test_that("find_statistic", {
      expect_identical(find_statistic(m1), "t-statistic")
    })

    test_that("get_predicted", {
      tmp <- mgcv::gam(y ~ s(x0) + s(x1), data = head(dat, 30))
      pred <- get_predicted(tmp, verbose = FALSE, ci = .95)
      expect_s3_class(pred, "get_predicted")
      expect_equal(
        as.vector(pred),
        c(
          11.99341, 5.58098, 10.89252, 7.10335, 5.94836, 6.5724, 8.5054,
          5.47147, 5.9343, 8.27001, 5.71199, 9.94999, 5.69979, 6.63532,
          6.00475, 5.58633, 11.54848, 6.1083, 6.6151, 5.37164, 6.86236,
          7.80726, 7.38088, 5.70664, 10.60654, 7.62847, 5.8596, 6.06744,
          5.81571, 10.4606
        ),
        tolerance = 1e-3
      )

      x <- get_predicted(tmp, predict = NULL, type = "link", ci = .95)
      y <- get_predicted(tmp, predict = "link", ci = .95)
      z <- predict(tmp, type = "link", se.fit = TRUE)
      expect_equal(x, y)
      expect_equal(x, z$fit, ignore_attr = TRUE)
      expect_equal(as.data.frame(x)$SE, z$se.fit, ignore_attr = TRUE)

      x <- get_predicted(tmp, predict = NULL, type = "response", verbose = FALSE, ci = .95)
      y <- get_predicted(tmp, predict = "expectation", ci = .95)
      z <- predict(tmp, type = "response", se.fit = TRUE)
      expect_equal(x, y, ignore_attr = TRUE)
      expect_equal(x, z$fit, ignore_attr = TRUE)
      expect_equal(as.data.frame(x)$SE, z$se.fit, ignore_attr = TRUE)

      # poisson
      void <- capture.output(
        dat <- gamSim(1, n = 400, dist = "poisson", scale = .25)
      )
      b4 <- gam(
        y ~ s(x0) + s(x1) + s(x2) + s(x3),
        family = poisson,
        data = dat,
        method = "GACV.Cp",
        scale = -1
      )
      d <- get_datagrid(b4, at = "x1")
      p1 <- get_predicted(b4, data = d, predict = "expectation", ci = .95)
      p2 <- predict(b4, newdata = d, type = "response")
      expect_equal(as.vector(p1), as.vector(p2), tolerance = 1e-4, ignore_attr = TRUE)

      p1 <- get_predicted(b4, data = d, predict = "link", ci = .95)
      p2 <- predict(b4, newdata = d, type = "link")
      expect_equal(as.vector(p1), as.vector(p2), tolerance = 1e-4, ignore_attr = TRUE)

      p1 <- get_predicted(b4, data = d, type = "link", predict = NULL, ci = .95)
      p2 <- predict(b4, newdata = d, type = "link")
      expect_equal(as.vector(p1), as.vector(p2), tolerance = 1e-4, ignore_attr = TRUE)

      p1 <- get_predicted(b4, data = d, type = "response", predict = NULL, ci = .95)
      p2 <- predict(b4, newdata = d, type = "response")
      expect_equal(as.vector(p1), as.vector(p2), tolerance = 1e-4, ignore_attr = TRUE)

      void <- capture.output(
        dat <- gamSim(1, n = 400, dist = "poisson", scale = .25)
      )
      b4 <- gam(
        y ~ s(x0) + s(x1) + s(x2) + s(x3),
        family = poisson,
        data = dat,
        method = "GACV.Cp",
        scale = -1
      )

      # exclude argument should be pushed through ...
      p1 <- predict(b4, type = "response", exclude = "s(x1)")
      p2 <- get_predicted(b4, predict = "expectation", exclude = "s(x1)", ci = .95)
      expect_equal(as.vector(p1), as.vector(p2), tolerance = 1e-4, ignore_attr = TRUE)
      p1 <- predict(b4, type = "link", exclude = "s(x1)")
      p2 <- get_predicted(b4, predict = "link", exclude = "s(x1)", ci = .95)
      expect_equal(as.vector(p1), as.vector(p2), tolerance = 1e-4, ignore_attr = TRUE)
    })
  }
}
