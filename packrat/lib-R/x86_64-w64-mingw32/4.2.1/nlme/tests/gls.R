## reported by simon bond <shug0131@yahoo.co.uk> to R-help 2007-03-16
library(nlme)
x <- rnorm(10, 0.1, 1)
try(gls(x ~ 0))  # segfaulted in 3.1-79


## PR#10364
# copied verbatim from Pinheiro & Bates 8.3.3
fm1Dial.gnls <-
  gnls(rate ~ SSasympOff(pressure, Asym, lrc, c0),
       data = Dialyzer, params = list(Asym + lrc ~ QB, c0 ~ 1),
       start = c(53.6, 8.6, 0.51, -0.26, 0.225))
(p1 <- predict(fm1Dial.gnls))
(p2 <- predict(fm1Dial.gnls, newdata = Dialyzer))
# failed, factor levels complaint
# also, missed row names as names
stopifnot(all.equal(as.vector(p1), as.vector(p2)), # 'label' differs
          identical(names(p1), names(p2)))


## PR#13418
fm1 <- gls(weight ~ Time * Diet, BodyWeight)
(V10 <- Variogram(fm1, form = ~ Time | Rat)[1:10,])
## failed in 3.1-89
stopifnot(all.equal(V10$variog,
                    c(0.0072395216, 0.014584634, 0.014207936, 0.018442267,
                      0.011128505, 0.019910082, 0.027072311, 0.034140379,
                      0.028320657, 0.037525507)),
          V10$dist == c(1, 6, 7, 8, 13, 14, 15, 20, 21, 22),
          V10$n.pairs == 16*c(1, 1, 9, 1, 1, 8, 1, 1, 7, 1))

intervals(fm1)

## predict from model with factor and no intercept
fm1b <- gls(weight ~ Diet - 1, BodyWeight)
stopifnot(all.equal(predict(fm1b, BodyWeight[1,]), coef(fm1b)[1],
                    check.attributes = FALSE))
## in nlme <= 3.1-155, failed with
## Error in X[, names(cf), drop = FALSE] : subscript out of bounds

## predict.gls(): handling newdata for factor variables
stopifnot(all.equal(
    predict(fm1, newdata = data.frame(Time = 1, Diet = "1", stringsAsFactor = FALSE)),
    fitted(fm1)[1], check.attributes = FALSE))
## in nlme <= 3.1-155, predict() failed with
## Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
##   contrasts can be applied only to factors with 2 or more levels
stopifnot(all.equal(
    predict(fm1, data.frame(Time = 71, Diet = c("2", "3"), stringsAsFactor = FALSE)),
    predict(fm1, data.frame(Time = 71, Diet = c("2", "3"), stringsAsFactor = TRUE))))
## in nlme <= 3.1-155, using character input failed with
## Error in X[, names(cf), drop = FALSE] : subscript out of bounds
tools::assertError(predict(fm1, data.frame(Time = 71, Diet = 2)), verbose = TRUE)
## more helpful error + warning now

## PR#17226: same for predict.gnls(), also without intercept
fm2 <- gnls(weight ~ f, data = BodyWeight, params = list(f ~ Diet - 1),
            start = rep(coef(fm1)[1], 3))
stopifnot(all.equal(predict(fm2, head(BodyWeight)), head(fitted(fm2)),
                    check.attributes = FALSE))
## in nlme <= 3.1-155, failed with
## Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
##   contrasts can be applied only to factors with 2 or more levels
stopifnot(all.equal(
    predict(fm2, data.frame(Time = 71, Diet = c("2", "3"), stringsAsFactor = FALSE)),
    predict(fm2, data.frame(Time = 71, Diet = c("2", "3"), stringsAsFactor = TRUE))))
## in nlme <= 3.1-155, failed with
## Error in p %*% beta[pmap[[nm]]] : non-conformable arguments


## PR#17880: offset() terms are (currently) not supported in package nlme
y <- 10:20; off <- rep(10, length(y))
tools::assertError(gls(y ~ 1 + offset(off)), verbose = TRUE)
## the following was TRUE in nlme <= 3.1-155, unfortunately:
## all.equal(coef(gls(y ~ 1 + offset(off))), coef(gls(y ~ 1)))


## PR#18283: gls() did not keep terms so predict() lacked "predvars"
fm_poly   <- gls(distance ~ poly(age, 1), data = Orthodont)
fm_nopoly <- gls(distance ~      age,     data = Orthodont)
stopifnot(all.equal(predict(fm_poly,   data.frame(age = 10)),
                    predict(fm_nopoly, data.frame(age = 10))))
## in nlme <= 3.1-155, prediction from fm_poly failed with
## Error in poly(age, 1) : 
##   'degree' must be less than number of unique points
stopifnot(all.equal(predict(fm_poly, head(Orthodont)),
                    head(fitted(fm_poly)), check.attributes = FALSE))
## predictions were wrong due to data-dependent bases
