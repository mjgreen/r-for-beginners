#
#   Copyright 2007-2021 by the individuals mentioned in the source code history
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
# 
#        http://www.apache.org/licenses/LICENSE-2.0
# 
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.


library(OpenMx)

# Simulate some data

x=rnorm(1000, mean=0, sd=1)
y= 0.5*x + rnorm(1000, mean=0, sd=1)
tmpFrame <- data.frame(x, y)
tmpNames <- names(tmpFrame)
wdata <- mxData(tmpFrame, type="raw")

# Define the matrices

S <- mxMatrix(type = "Full", nrow = 2, ncol = 2, values=c(1,0,0,1),
              free=c(TRUE,FALSE,FALSE,TRUE), labels=c("Vx", NA, NA, "Vy"), name = "S")
A <- mxMatrix(type = "Full", nrow = 2, ncol = 2, values=c(0,1,0,0),
              free=c(FALSE,TRUE,FALSE,FALSE), labels=c(NA, "b", NA, NA), name = "A")
I <- mxMatrix(type="Iden", nrow=2, ncol=2, name="I")

# Define the expectation

expCov <- mxAlgebra(solve(I-A) %*% S %*% t(solve(I-A)), name="expCov")
expFunction <- mxExpectationNormal(covariance="expCov", dimnames=tmpNames)

# Choose a fit function

fitFunction <- mxFitFunctionWLS('DWLS')

# Define the model

tmpModel <- mxModel(model="exampleModel", S, A, I, expCov, expFunction, fitFunction,
                    wdata, mxCI("A"))

# Fit the model and print a summary

tmpModelOut <- mxRun(tmpModel)
summary(tmpModelOut)

tmpModel2 <- mxModel(tmpModel,name="tmp2")
twoGroup <- mxModel("two", tmpModel, tmpModel2, mxFitFunctionMultigroup(c("exampleModel","tmp2")))
twoGroup <- mxRun(twoGroup)
omxCheckError(mxRun(twoGroup, intervals=TRUE),
              "Confidence intervals are not supported for DWLS or ULS.  Try mxSE or switch 'exampleModel' to full WLS")

# Experiment with multigroup automatic start values
autModel1 <- mxModel(tmpModel, name="auto1", mxData(tmpFrame, 'raw'),
	mxMatrix(name='expMean', nrow=1, ncol=2, free=TRUE),
	mxExpectationNormal(covariance="expCov", means='expMean', dimnames=tmpNames),
	mxFitFunctionML())
autModel2 <- mxModel(autModel1, name="auto2")
autGroup <- mxModel("group", autModel1, autModel2, mxFitFunctionMultigroup(c("auto1","auto2")))
autStart <- mxAutoStart(autGroup)

# Starting values from mxAutoStart are close to the final estimates from multigroup WLS
omxCheckCloseEnough(coef(autStart)[names(coef(twoGroup))], coef(twoGroup), 1e-3)

# ---------------------
# Are SEs correct?

mgen <- mxModel('mg', type='RAM', manifestVars = c('a','b'),
              mxPath(c('a','b'), arrows=2, values=1, labels=paste0(c('a','b'),'Var')),
              mxPath('a', 'b', values=.5, labels="reg"))
# marginals TODO
              #mxPath('one', c('a','b'), labels=paste0(c('a','b'),'Mean'))

data1 <- mxGenerateData(mgen, nrows = 400)

mgen$S$values['a','a'] <- .5
mgen$S$values['b','b'] <- .5

data2 <- mxGenerateData(mgen, nrows = 200)  # unequal sample size!

m1 <- mxModel(mgen, mxData(data1, 'raw'), mxFitFunctionWLS(), name='g1')
m2 <- mxModel(mgen, mxData(data2, 'raw'), mxFitFunctionWLS(), name='g2')
mg <- mxModel('two', m1,m2, mxFitFunctionMultigroup(paste0('g', 1:2)))
mg <- mxRun(mg)

mc <- mxModel(mgen, mxData(rbind(data1,data2), 'raw'), mxFitFunctionWLS())
mc <- mxRun(mc)

c(mg$output$standardErrors - mc$output$standardErrors) # should be zero TODO
