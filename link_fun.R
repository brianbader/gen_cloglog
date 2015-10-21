
## Creating the generalized link function, call it gen_cloglog
gen_cloglog <- function(a , b)
{
    ## link
    linkfun <- function(y) { log(-(1/a)*log(1 - y^(1/b))) }

    ## inverse link
    linkinv <- function(eta)
    {
        pmax(pmin(((1 - exp(-a*exp(eta)))^b), 1 - .Machine$double.eps), .Machine$double.eps)
    }

    ## derivative of invlink wrt eta
    mu.eta <- function(eta)
    {
        eta <- pmin(eta, 700)
        pmax((a*b*exp(eta)*exp(-a*exp(eta))*((1-exp(-a*exp(eta)))^(b-1))), .Machine$double.eps)
    }

    valideta <- function(eta) TRUE
    link <- "log(-(1/a)*log(1 - y^(1/b)))"
    structure(list(linkfun = linkfun, linkinv = linkinv,
                   mu.eta = mu.eta, valideta = valideta,
                   name = link), class = "link-glm")
}


######################################################
######################################################
######################################################


## Menarche Data
library(MASS)
data(menarche)

## To optimize over a and b - minimize deviance.
f <- function(params)
{
    ## Set this glm statement as you would normally specify your model and data (don't touch the family and link setting)
    y <- glm(cbind(Menarche, Total - Menarche) ~ Age, data=menarche, family=binomial(link=gen_cloglog(params[1], params[2])))
    result <- y$deviance
    return(as.vector(result))
}


## Optimize values of a,b. Sometimes need to change the starting values in order to get convergence
## result returns the optimized values of a,b
result <- optim(c(3, 3), f, method="BFGS")

## Fit the glm with the optimal a and b values.
y <- glm(cbind(Menarche, Total - Menarche) ~ Age, data=menarche, family=binomial(link=gen_cloglog(result$par[1], result$par[2])))

## Fit the a=b=1 cloglog
z <- glm(cbind(Menarche, Total - Menarche) ~ Age, data=menarche, family=binomial(link=cloglog))

## Logit model
v <- glm(cbind(Menarche, Total - Menarche) ~ Age, data=menarche, family=binomial(link=logit))

## Probit model
w <- glm(cbind(Menarche, Total - Menarche) ~ Age, data=menarche, family=binomial(link=probit))



## Likelihood ratio test between generalize cloglog and restricted (a=b=1)
L0 <- logLik(z)
L1 <- logLik(y)
L01 <- as.vector(- 2 * (L0 - L1))
pval <- pchisq(L01, 2, lower.tail = FALSE)
pval





######################################################
######################################################
######################################################


## Bacteria growth data
## http://mathinsight.org/bacteria_growth_logistic_model
bacteria <- read.csv(file="bacteria.csv", header=TRUE)

## To optimize over a and b - minimize deviance.
f <- function(params)
{
    ## Set this glm statement as you would normally specify your model and data (don't touch the family and link setting)
    y <- glm(cbind(BacteriaNum, Total - BacteriaNum)~TimeInd, data=bacteria, family=binomial(link=gen_cloglog(params[1] , params[2])))
    result <- y$deviance
    return(as.vector(result))
}


## Optimize values of a,b. Sometimes need to change the starting values in order to get nice convergence...
## result returns the optimized values of a,b
result <- optim(c(3, 3), f, method="BFGS")

## Fit the glm with the optimal a and b values.
y <- glm(cbind(BacteriaNum, Total - BacteriaNum)~TimeInd, data=bacteria, family=binomial(link=gen_cloglog(result$par[1],result$par[2])))

## Fit the a=b=1 cloglog
z <- glm(cbind(BacteriaNum, Total - BacteriaNum)~TimeInd, data=bacteria, family=binomial(link=cloglog))

## Logit
v <- glm(cbind(BacteriaNum, Total - BacteriaNum)~TimeInd, data=bacteria, family=binomial(link=logit))

## Probit
w <- glm(cbind(BacteriaNum, Total - BacteriaNum)~TimeInd, data=bacteria, family=binomial(link=probit))



######################################################
######################################################
######################################################



## Beetle Data
beetle <- read.csv(file="beetle.csv", header=TRUE)

## To optimize over a and b - minimize deviance.
f <- function(params)
{
    ## Set this glm statement as you would normally specify your model and data (don't touch the family and link setting)
    y <- glm(cbind(Number_killed, Number_tested - Number_killed) ~ logDose, data=beetle, family=binomial(link=gen_cloglog(params[1], params[2])))
    result <- y$aic
    return(as.vector(result))
}


## Optimize values of a,b. Sometimes need to change the starting values in order to achieve convergence
## result returns the optimized values of a,b
result <- optim(c(1, 1), f)

## Fit the glm with the optimal a and b values.
y <- glm(cbind(Number_killed, Number_tested - Number_killed) ~ logDose, data=beetle, family=binomial(link=gen_cloglog(result$par[1],result$par[2])))

## Fit the a=b=1 cloglog
z <- glm(cbind(Number_killed, Number_tested - Number_killed) ~ logDose, data=beetle, family=binomial(link=cloglog))

## Logit
v <- glm(cbind(Number_killed, Number_tested - Number_killed) ~ logDose, data=beetle, family=binomial(link=logit))

## Probit
w <- glm(cbind(Number_killed, Number_tested - Number_killed) ~ logDose, data=beetle, family=binomial(link=probit))



