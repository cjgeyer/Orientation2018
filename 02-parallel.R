## ------------------------------------------------------------------------
knitr::purl("02-parallel.Rmd")

## ------------------------------------------------------------------------
# sample size
n <- 10
# simulation sample size
nsim <- 1e4
# true unknown parameter value
# of course in the simulation it is known, but we pretend we don't
# know it and estimate it
theta <- 1

doit <- function(estimator, seed = 42) {
    set.seed(seed)
    result <- double(nsim)
    for (i in 1:nsim) {
        x <- rnorm(n, theta, abs(theta))
        result[i] <- estimator(x)
    }
    return(result)
}

mlogl <- function(theta, x) sum(- dnorm(x, theta, abs(theta), log = TRUE))

mle <- function(x) {
    if (all(x == 0))
        return(0)
    nout <- nlm(mlogl, sign(mean(x)) * sd(x), x = x)
    while (nout$code > 3)
        nout <- nlm(mlogl, nout$estimate, x = x)
    return(nout$estimate)
}

## ------------------------------------------------------------------------
theta.hat <- doit(mle)

## ----fig.align='center'--------------------------------------------------
hist(theta.hat, probability = TRUE, breaks = 30)
curve(dnorm(x, mean = theta, sd = theta / sqrt(3 * n)), add = TRUE)

## ----cache=TRUE----------------------------------------------------------
time1 <- system.time(theta.hat.mle <- doit(mle))
time1

## ----cache=TRUE----------------------------------------------------------
nsim <- 1e5
nrep <- 7
time1 <- NULL
for (irep in 1:nrep)
    time1 <- rbind(time1, system.time(theta.hat.mle <- doit(mle)))
time1
apply(time1, 2, mean)
apply(time1, 2, sd) / sqrt(nrep)

## ------------------------------------------------------------------------
library(parallel)
ncores <- detectCores()
mclapply(1:ncores, function(x) Sys.getpid(), mc.cores = ncores)

## ------------------------------------------------------------------------
RNGkind("L'Ecuyer-CMRG")
set.seed(42)
mclapply(1:ncores, function(x) rnorm(5), mc.cores = ncores)
set.seed(42)
mclapply(1:ncores, function(x) rnorm(5), mc.cores = ncores)

## ------------------------------------------------------------------------
doit <- function(nsim, estimator) {
    result <- double(nsim)
    for (i in 1:nsim) {
        x <- rnorm(n, theta, abs(theta))
        result[i] <- estimator(x)
    }
    return(result)
}

## ----cache=TRUE----------------------------------------------------------
mout <- mclapply(rep(nsim / ncores, ncores), doit,
    estimator = mle, mc.cores = ncores)
lapply(mout, head)

## ------------------------------------------------------------------------
length(mout)
sapply(mout, length)
lapply(mout, head)
lapply(mout, range)

## ----fig.align='center'--------------------------------------------------
theta.hat <- unlist(mout)
hist(theta.hat, probability = TRUE, breaks = 30)
curve(dnorm(x, mean = theta, sd = theta / sqrt(3 * n)), add = TRUE)

## ----cache=TRUE----------------------------------------------------------
time4 <- NULL
for (irep in 1:nrep)
    time4 <- rbind(time4, system.time(theta.hat.mle <-
        unlist(mclapply(rep(nsim / ncores, ncores), doit,
            estimator = mle, mc.cores = ncores))))
time4
apply(time4, 2, mean)
apply(time4, 2, sd) / sqrt(nrep)

## ------------------------------------------------------------------------
apply(time4, 2, mean)["elapsed"]

## ------------------------------------------------------------------------
apply(time1, 2, mean)["elapsed"]

## ----echo = FALSE--------------------------------------------------------
rats <- apply(time1, 2, mean)["elapsed"] / apply(time4, 2, mean)["elapsed"]

## ------------------------------------------------------------------------
library(parallel)
ncores <- detectCores()
cl <- makePSOCKcluster(ncores)
parLapply(cl, 1:ncores, function(x) Sys.getpid())
stopCluster(cl)

## ------------------------------------------------------------------------
cl <- makePSOCKcluster(ncores)
clusterSetRNGStream(cl, 42)
parLapply(cl, 1:ncores, function(x) rnorm(5))
parLapply(cl, 1:ncores, function(x) rnorm(5))

## ------------------------------------------------------------------------
clusterExport(cl, c("doit", "mle", "mlogl", "n", "nsim", "theta"))

## ----cache=TRUE----------------------------------------------------------
pout <- parLapply(cl, rep(nsim / ncores, ncores), doit, estimator = mle)

## ------------------------------------------------------------------------
length(pout)
sapply(pout, length)
lapply(pout, head)
lapply(pout, range)

## ----fig.align='center'--------------------------------------------------
theta.hat <- unlist(mout)
hist(theta.hat, probability = TRUE, breaks = 30)
curve(dnorm(x, mean = theta, sd = theta / sqrt(3 * n)), add = TRUE)

## ----cache=TRUE----------------------------------------------------------
time5 <- NULL
for (irep in 1:nrep)
    time5 <- rbind(time5, system.time(theta.hat.mle <-
        unlist(parLapply(cl, rep(nsim / ncores, ncores),
            doit, estimator = mle))))
time5
apply(time5, 2, mean)
apply(time5, 2, sd) / sqrt(nrep)

## ------------------------------------------------------------------------
apply(time5, 2, mean)["elapsed"]

## ------------------------------------------------------------------------
apply(time1, 2, mean)["elapsed"]

## ------------------------------------------------------------------------
stopCluster(cl)

