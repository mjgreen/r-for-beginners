if (requiet("testthat") && requiet("insight")) {
  test_that("clean_names", {
    expect_equal(clean_names(""), "")
    expect_equal(clean_names("as.factor(test)"), "test")
    expect_equal(clean_names("log(test)"), "test")
    expect_equal(clean_names("log(test, base = exp(3))"), "test")
    expect_equal(clean_names("log(test,base=exp(3))"), "test")
    expect_equal(clean_names("log(test/10)"), "test")
    expect_equal(clean_names("log(test^2)"), "test")
    expect_equal(clean_names("log(log(test))"), "test")
    expect_equal(clean_names("log(log(test/10))"), "test")
    expect_equal(clean_names("log(log(test*2))"), "test")
    expect_equal(clean_names("scale(log(Days1))"), "Days1")
    expect_equal(clean_names("I(test^2)"), "test")
    expect_equal(clean_names("I(test/10)"), "test")
    expect_equal(clean_names("I(test ^ 2)"), "test")
    expect_equal(clean_names("I(test / 10)"), "test")
    expect_equal(clean_names("poly(test, 2)"), "test")
    expect_equal(clean_names("poly(test, degrees = 2)"), "test")
    expect_equal(clean_names("poly(test, degrees = 2, raw = TRUE)"), "test")
    expect_equal(clean_names("ns(test)"), "test")
    expect_equal(clean_names("ns(test, df = 2)"), "test")
    expect_equal(clean_names("bs(test)"), "test")
    expect_equal(clean_names("bs(test, df = 2)"), "test")
    expect_equal(clean_names("offset(test)"), "test")
    expect_equal(clean_names("offset(log(test))"), "test")
    expect_equal(clean_names("factor(test)"), "test")
    expect_equal(clean_names("as.factor(test)"), "test")
    expect_equal(clean_names("~ 1 | test"), "test")
    expect_equal(clean_names("~1|test"), "test")
    expect_equal(clean_names("1 | test"), "test")
    expect_equal(clean_names("as.factor(Sepal.Length)"), "Sepal.Length")
    expect_equal(clean_names("log(Sepal.Length)"), "Sepal.Length")
    expect_equal(clean_names("log(Sepal.Length, base = exp(3))"), "Sepal.Length")
    expect_equal(clean_names("log(Sepal.Length,base=exp(3))"), "Sepal.Length")
    expect_equal(clean_names("log(Sepal.Length/10)"), "Sepal.Length")
    expect_equal(clean_names("log(Sepal.Length^2)"), "Sepal.Length")
    expect_equal(clean_names("log(log(Sepal.Length))"), "Sepal.Length")
    expect_equal(clean_names("log(log(Sepal.Length/10))"), "Sepal.Length")
    expect_equal(clean_names("log(log(Sepal.Length*2))"), "Sepal.Length")
    expect_equal(clean_names("I(Sepal.Length^2)"), "Sepal.Length")
    expect_equal(clean_names("I(Sepal.Length/10)"), "Sepal.Length")
    expect_equal(clean_names("I(Sepal.Length ^ 2)"), "Sepal.Length")
    expect_equal(clean_names("I(Sepal.Length / 10)"), "Sepal.Length")
    expect_equal(clean_names("poly(Sepal.Length, 2)"), "Sepal.Length")
    expect_equal(clean_names("poly(Sepal.Length, degrees = 2)"), "Sepal.Length")
    expect_equal(clean_names("poly(Sepal.Length, degrees = 2, raw = TRUE)"), "Sepal.Length")
    expect_equal(clean_names("ns(Sepal.Length)"), "Sepal.Length")
    expect_equal(clean_names("ns(Sepal.Length, df = 2)"), "Sepal.Length")
    expect_equal(clean_names("bs(Sepal.Length)"), "Sepal.Length")
    expect_equal(clean_names("bs(Sepal.Length, df = 2)"), "Sepal.Length")
    expect_equal(clean_names("offset(Sepal.Length)"), "Sepal.Length")
    expect_equal(clean_names("offset(log(Sepal.Length))"), "Sepal.Length")
    expect_equal(clean_names("factor(Sepal.Length)"), "Sepal.Length")
    expect_equal(clean_names("as.factor(Sepal.Length)"), "Sepal.Length")
    expect_equal(clean_names("~ 1 | Sepal.Length"), "Sepal.Length")
    expect_equal(clean_names("~1|Sepal.Length"), "Sepal.Length")
    expect_equal(clean_names("1 | Sepal.Length"), "Sepal.Length")
    expect_equal(clean_names(c("scale(a)", "scale(b)", "scale(a):scale(b)")), c("a", "b", "a:b"))
    expect_equal(
      clean_names(c("scale(a)", "scale(b)", "scale(a):scale(b)"), include_names = TRUE),
      c(`scale(a)` = "a", `scale(b)` = "b", `scale(a):scale(b)` = "a:b")
    )
    expect_equal(clean_names("s(x1, x2)"), "x1, x2")
    expect_equal(clean_names("s(x1, x2, k = -1)"), "x1, x2")
    expect_equal(clean_names("s(x1, x2, x3)"), "x1, x2, x3")
  })
}
