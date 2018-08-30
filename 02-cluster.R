
## where are we ???
system("hostname -f", intern = TRUE)

## ------------------------------------------------------------------------
# sample size
n <- 10
# simulation sample size
nsim <- 1e7
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
library(parallel)
# don't figure out cores on LATIS with
# ncores <- detectCores()
# that gives the number of physical cores, not how many you are allowed
# instead use
ncores <- as.numeric(Sys.getenv("PBS_NP"))
ncores
# don't use PSOCK cluster on LATIS, use MPI cluster
cl <- makeCluster(ncores, "MPI")
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
nrep <- 7
time5 <- NULL
for (irep in 1:nrep)
    time5 <- rbind(time5, system.time(theta.hat.mle <-
        unlist(parLapply(cl, rep(nsim / ncores, ncores),
            doit, estimator = mle))))
time5
apply(time5, 2, mean)
apply(time5, 2, sd) / sqrt(nrep)

## ------------------------------------------------------------------------
stopCluster(cl)

