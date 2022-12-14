.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (requiet("testthat") &&
  requiet("insight") &&
  requiet("pscl")) {
  data("bioChemists")

  m1 <- zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)

  test_that("model_info", {
    expect_true(model_info(m1)$is_poisson)
    expect_true(model_info(m1)$is_zero_inflated)
    expect_false(model_info(m1)$is_linear)
  })

  test_that("n_parameters", {
    expect_equal(n_parameters(m1), 8)
    expect_equal(n_parameters(m1, component = "conditional"), 5)
  })

  test_that("find_predictors", {
    expect_identical(
      find_predictors(m1),
      list(
        conditional = c("fem", "mar", "kid5", "ment"),
        zero_inflated = c("kid5", "phd")
      )
    )
    expect_identical(
      find_predictors(m1, flatten = TRUE),
      c("fem", "mar", "kid5", "ment", "phd")
    )
    expect_null(find_predictors(m1, effects = "random"))
  })

  test_that("find_response", {
    expect_identical(find_response(m1), "art")
  })

  test_that("link_inverse", {
    expect_equal(link_inverse(m1)(.2), exp(.2), tolerance = 1e-5)
  })

  test_that("get_data", {
    expect_equal(nrow(get_data(m1)), 915)
    expect_equal(
      colnames(get_data(m1)),
      c("art", "fem", "mar", "kid5", "ment", "phd")
    )
  })

  test_that("find_formula", {
    expect_length(find_formula(m1), 2)
    expect_equal(
      find_formula(m1),
      list(
        conditional = as.formula("art ~ fem + mar + kid5 + ment"),
        zero_inflated = as.formula("~kid5 + phd")
      ),
      ignore_attr = TRUE
    )
  })

  test_that("find_terms", {
    expect_equal(
      find_terms(m1),
      list(
        response = "art",
        conditional = c("fem", "mar", "kid5", "ment"),
        zero_inflated = c("kid5", "phd")
      )
    )
    expect_equal(
      find_terms(m1, flatten = TRUE),
      c("art", "fem", "mar", "kid5", "ment", "phd")
    )
  })

  test_that("n_obs", {
    expect_equal(n_obs(m1), 915)
  })

  test_that("linkfun", {
    expect_false(is.null(link_function(m1)))
  })

  test_that("find_parameters", {
    expect_equal(
      find_parameters(m1),
      list(
        conditional = c(
          "count_(Intercept)",
          "count_femWomen",
          "count_marMarried",
          "count_kid5",
          "count_ment"
        ),
        zero_inflated = c("zero_(Intercept)", "zero_kid5", "zero_phd")
      )
    )
    expect_equal(nrow(get_parameters(m1)), 8)
    expect_equal(nrow(get_parameters(m1, component = "zi")), 3)
    expect_equal(
      get_parameters(m1)$Parameter,
      c(
        "count_(Intercept)",
        "count_femWomen",
        "count_marMarried",
        "count_kid5",
        "count_ment",
        "zero_(Intercept)",
        "zero_kid5",
        "zero_phd"
      )
    )
  })

  test_that("find_statistic", {
    expect_identical(find_statistic(m1), "z-statistic")
  })

  test_that("get_statistic", {
    expect_equal(
      get_statistic(m1)$Statistic,
      c(8.26297, -3.90986, 2.07134, -3.43156, 10.05389, -2.143, 0.21384, -1.84259),
      tolerance = 1e-3
    )
    expect_equal(
      get_statistic(m1)$Component,
      c(
        "conditional", "conditional", "conditional", "conditional",
        "conditional", "zero_inflated", "zero_inflated", "zero_inflated"
      ),
      tolerance = 1e-3
    )
  })


  if (.runThisTest && requiet("sandwich")) {
    set.seed(123)
    vc1 <- get_varcov(m1, component = "all", vcov = "BS", vcov_args = list(R = 50))
    set.seed(123)
    vc2 <- sandwich::vcovBS(m1, R = 50)
    expect_equal(vc1, vc2, ignore_attr = TRUE)

    set.seed(123)
    vc1 <- get_varcov(m1, component = "conditional", vcov = "BS", vcov_args = list(R = 50))
    count_col <- grepl("^count_", colnames(vc2))
    expect_equal(vc1, vc2[count_col, count_col], ignore_attr = TRUE)

    set.seed(123)
    vc1 <- get_varcov(m1, component = "zero_inflated", vcov = "BS", vcov_args = list(R = 50))
    zero_col <- grepl("^zero_", colnames(vc2))
    expect_equal(vc1, vc2[zero_col, zero_col], ignore_attr = TRUE)
  }

  m2 <- zeroinfl(formula = art ~ . | 1, data = bioChemists, dist = "negbin")
  .runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

  if (.runThisTest || Sys.getenv("USER") == "travis") {
    test_that("get_statistic", {
      expect_equal(
        get_statistic(m2)$Statistic,
        c(1.84902, -2.97806, 1.83266, -3.32478, 0.42324, 8.38088, -0.14579),
        tolerance = 1e-3
      )
      expect_equal(
        get_statistic(m2)$Component,
        c(
          "conditional", "conditional", "conditional", "conditional",
          "conditional", "conditional", "zero_inflated"
        ),
        tolerance = 1e-3
      )
    })
  }
}
