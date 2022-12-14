stopifnot(require(RUnit, quietly=TRUE))
stopifnot(require(yaml, quietly=TRUE))

# Set RUnit options
if (!getOption("yaml.verbose", FALSE)) {
  opts <- getOption("RUnit")
  opts$silent <- TRUE
  opts$verbose <- FALSE
  options("RUnit" = opts)
}

# Test helpers
captureWarnings <- function(expr) {
  warnings <- character(0)
  suppressWarnings({
    withCallingHandlers(expr, warning = function(w) {
      warnings <<- c(warnings, w$message)
    })
  })
  warnings
}

checkWarning <- function(expr) {
  warnings <- captureWarnings(expr)
  checkTrue(length(warnings) > 0)
}

checkNamedListEquals <- function(x, y) {
  checkEquals(class(x), class(y))
  checkEquals(length(x), length(y))

  ns <- sort(names(x))
  checkEquals(ns, sort(names(y)))
  for (n in ns) {
    checkEquals(x[[n]], y[[n]])
  }
}

# Define tests
testSuite <- defineTestSuite(name = "yaml tests",
                             dirs = system.file("tests", package = "yaml"),
                             testFileRegexp = "^test_.+",
                             testFuncRegexp = "^test_.+",
                             rngKind = 'Mersenne-Twister',
                             rngNormalKind = 'Inversion')

tests <- runTestSuite(testSuite)

# Print results
printTextProtocol(tests, showDetails = FALSE)

# Return success or failure to R CMD CHECK
if (getErrors(tests)$nFail > 0) {
  stop("TEST FAILED!")
}
if (getErrors(tests)$nErr > 0) {
  stop("TEST HAD ERRORS!")
}
if (getErrors(tests)$nTestFunc < 1) {
  stop("NO TEST FUNCTIONS RUN!")
}
