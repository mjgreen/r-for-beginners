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

if (!osx && .runThisTest && requiet("testthat") && requiet("insight") && requiet("lme4")) {
  data("sleepstudy")
  data("Penicillin")
  set.seed(12345)
  sleepstudy$grp <- sample(1:5, size = 180, replace = TRUE)
  sleepstudy$subgrp <- NA
  for (i in 1:5) {
    filter_group <- sleepstudy$grp == i
    sleepstudy$subgrp[filter_group] <-
      sample(1:30, size = sum(filter_group), replace = TRUE)
  }

  fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  fm2 <- lmer(Reaction ~ Days + (Days || Subject), sleepstudy)
  fm3 <- lmer(
    Reaction ~ Days + (1 + Days || grp / subgrp) + (1 + Days | Subject),
    data = sleepstudy
  )
  fm4 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
  fm5 <- lmer(
    Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject),
    data = sleepstudy
  )
  fm6 <- lmer(diameter ~ 0 + sample + (1 | plate), data = Penicillin)

  v1 <- suppressWarnings(get_variance(fm1))
  v2 <- suppressWarnings(get_variance(fm2))
  v3 <- suppressWarnings(get_variance(fm3))
  v4 <- suppressWarnings(get_variance(fm4))
  v5 <- suppressWarnings(get_variance(fm5))
  v6 <- suppressWarnings(get_variance(fm6))

  test_that("get_variance-1", {
    expect_equal(v1$var.intercept,
      c(Subject = 612.10016),
      tolerance = 1e-2
    )
    expect_equal(v1$var.slope,
      c(Subject.Days = 35.07171),
      tolerance = 1e-2
    )
  })

  test_that("get_variance-2", {
    expect_equal(v2$var.intercept,
      c(Subject = 627.56905),
      tolerance = 1e-2
    )
    expect_equal(v2$var.slope,
      c(Subject.Days = 35.85838),
      tolerance = 1e-2
    )
  })

  test_that("get_variance-3", {
    expect_equal(v3$var.intercept,
      c(subgrp.grp.1 = 0, Subject = 662.52047, grp.1 = 0),
      tolerance = 1e-2
    )
    expect_equal(v3$var.slope,
      c(Subject.Days = 34.25771, subgrp.grp.Days = 7.88485, grp.Days = 0),
      tolerance = 1e-2
    )
  })

  test_that("get_variance-4", {
    expect_equal(v4$var.intercept,
      c(Subject = 1378.17851),
      tolerance = 1e-2
    )
    expect_null(v4$var.slope)
  })

  test_that("get_variance-5", {
    expect_equal(v5$var.intercept,
      c(`subgrp:grp` = 38.76069, Subject = 1377.50569, grp = 3.32031),
      tolerance = 1e-2
    )
    expect_null(v5$var.slope)
  })

  test_that("get_variance-6", {
    expect_equal(v6$var.intercept, c(plate = 0.71691), tolerance = 1e-2)
    expect_equal(v6$var.random, 0.71691, tolerance = 1e-2)
    expect_null(v6$var.slope)
  })


  # further examples

  model <- lmer(Reaction ~ Days + (1 + Days || Subject), data = sleepstudy)
  vmodel <- get_variance(model)

  test_that("get_variance-7", {
    expect_equal(
      vmodel,
      list(
        var.fixed = 908.95336, var.random = 627.56905, var.residual = 653.5835,
        var.distribution = 653.5835, var.dispersion = 0, var.intercept = c(Subject = 627.56905),
        var.slope = c(Subject.Days = 35.85838)
      ),
      tolerance = 1e-2
    )
  })

  model <- lmer(Reaction ~ Days + (0 + Days || Subject), data = sleepstudy)
  vmodel <- get_variance(model)

  test_that("get_variance-8", {
    expect_equal(
      vmodel,
      list(
        var.fixed = 908.95336, var.random = 1502.179, var.residual = 842.02962,
        var.distribution = 842.02962, var.dispersion = 0, var.slope = c(Subject.Days = 52.70804)
      ),
      tolerance = 1e-2
    )
  })


  # categorical rnd slope

  data("sleepstudy")
  sleepstudy$Days2 <- cut(sleepstudy$Days, breaks = c(-1, 3, 6, 10))

  model <- lmer(Reaction ~ Days2 + (1 + Days2 | Subject), data = sleepstudy)
  vmodel <- get_variance(model)

  test_that("get_variance-9", {
    expect_equal(
      vmodel,
      list(
        var.fixed = 807.085453556748, var.random = 1711.44396436951,
        var.residual = 748.811071562908, var.distribution = 748.811071562908,
        var.dispersion = 0, var.intercept = c(Subject = 663.280418978822),
        var.slope = c(`Subject.Days2(3,6]` = 882.364188919403, `Subject.Days2(6,10]` = 1415.70768194576),
        cor.slope_intercept = structure(c(0.361173061386374, 0.331878499015884), dim = 2:1, dimnames = list(c("Days2(3,6]", "Days2(6,10]"), "Subject")),
        cor.slopes = c(`Subject.Days2(3,6]-Days2(6,10]` = 0.847444720096841)
      ),
      tolerance = 1e-2
    )
  })

  model <- suppressWarnings(lmer(Reaction ~ Days2 + (1 + Days2 || Subject), data = sleepstudy))
  vmodel <- suppressWarnings(get_variance(model))

  test_that("get_variance-10", {
    expect_equal(
      vmodel,
      list(
        var.fixed = 807.08545355676, var.residual = 740.875581179784,
        var.distribution = 740.875581179784, var.dispersion = 0,
        var.intercept = c(Subject = 738.635155172211),
        var.slope = c(
          `Subject.Days2(-1,3]` = 0, `Subject.Days2(3,6]` = 994.015865559888,
          `Subject.Days2(6,10]` = 1545.72576115283
        ),
        cor.slopes = c(`Subject.1.Days2(3,6]-Days2(6,10]` = 0.859480774219098)
      ),
      tolerance = 1e-2
    )
  })

  model <- lmer(Reaction ~ Days2 + (0 + Days2 | Subject), data = sleepstudy)
  vmodel <- get_variance(model)

  test_that("get_variance-11", {
    expect_equal(
      vmodel,
      list(
        var.fixed = 807.085453556794, var.random = 1446.13555108848,
        var.residual = 748.813858500395, var.distribution = 748.813858500395,
        var.dispersion = 0, var.slope = c(
          `Subject.Days2(-1,3]` = 663.27445659023,
          `Subject.Days2(3,6]` = 2098.24691538121, `Subject.Days2(6,10]` = 2722.20492158038
        ), cor.slopes = c(
          `Subject.Days2(-1,3]-Days2(3,6]` = 0.796453122321232,
          `Subject.Days2(-1,3]-Days2(6,10]` = 0.732956077304911, `Subject.Days2(3,6]-Days2(6,10]` = 0.924018087860575
        )
      ),
      tolerance = 1e-2
    )
  })

  model <- lmer(Reaction ~ Days2 + (0 + Days2 || Subject), data = sleepstudy)
  vmodel <- get_variance(model)

  test_that("get_variance-12", {
    expect_equal(
      vmodel,
      list(
        var.fixed = 807.085453556794, var.random = 1446.13555108848,
        var.residual = 748.813858500395, var.distribution = 748.813858500395,
        var.dispersion = 0, var.slope = c(
          `Subject.Days2(-1,3]` = 663.27445659023,
          `Subject.Days2(3,6]` = 2098.24691538121, `Subject.Days2(6,10]` = 2722.20492158038
        ), cor.slopes = c(
          `Subject.Days2(-1,3]-Days2(3,6]` = 0.796453122321232,
          `Subject.Days2(-1,3]-Days2(6,10]` = 0.732956077304911, `Subject.Days2(3,6]-Days2(6,10]` = 0.924018087860575
        )
      ),
      tolerance = 1e-2
    )
  })


  # test random slope correlation for categorical random slope

  data(cake)
  m <- lmer(angle ~ temperature + (temperature | recipe), data = cake)

  test_that("get_variance-cat_random_slope", {
    vc <- suppressWarnings(get_variance(m))
    expect_equal(
      vc$cor.slopes,
      c(
        `recipe.temperature.L-temperature.C` = 0.99999964, `recipe.temperature.Q-temperature.C` = 0.99999931,
        `recipe.temperature.L-temperature.Q` = 0.99999941, `recipe.temperature.L-temperature^4` = 0.99999961,
        `recipe.temperature.Q-temperature^4` = 0.99999912, `recipe.temperature.C-temperature^4` = 0.99999996,
        `recipe.temperature.L-temperature^5` = -0.99999977, `recipe.temperature.Q-temperature^5` = -0.99999849,
        `recipe.temperature.C-temperature^5` = -0.99999936, `recipe.temperature^4-temperature^5` = -0.99999941
      ),
      tolerance = 1e-3
    )
  })

  data("sleepstudy")
  set.seed(123)
  sleepstudy$Months <- sample(1:4, nrow(sleepstudy), TRUE)

  m2 <- lmer(Reaction ~ Days + (0 + Days | Subject), data = sleepstudy)
  m5 <- lmer(Reaction ~ Days + (0 + Days + Months | Subject), data = sleepstudy)

  test_that("random effects CIs, simple slope", {
    vc <- suppressWarnings(get_variance(m2))
    expect_equal(
      names(vc),
      c(
        "var.fixed", "var.random", "var.residual", "var.distribution",
        "var.dispersion", "var.slope"
      ),
      tolerance = 1e-3,
      ignore_attr = TRUE
    )
  })

  test_that("random effects CIs, simple slope", {
    vc <- suppressWarnings(get_variance(m5))
    expect_equal(
      vc,
      list(
        var.fixed = 921.929610133035, var.random = 1068.04697608476,
        var.residual = 764.479364064599, var.distribution = 764.479364064599,
        var.dispersion = 0, var.slope = c(
          Subject.Days = 37.4753324942022,
          Subject.Months = 27.6430649522841
        ),
        cor.slopes = c(`Subject.Days-Months` = 0.455625778436967)
      ),
      tolerance = 1e-3,
      ignore_attr = TRUE
    )
  })

  data(cake)
  m <- lmer(angle ~ poly(temp, 2) + (poly(temp, 2) | replicate) + (1 | recipe), data = cake)

  test_that("random effects CIs, poly slope", {
    vc <- suppressWarnings(get_variance(m))
    expect_equal(
      vc$cor.slopes,
      c(`replicate.poly(temp, 2)1-poly(temp, 2)2` = 0.940016422944175),
      tolerance = 1e-3,
      ignore_attr = TRUE
    )
  })
}
