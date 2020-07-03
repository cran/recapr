## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
set.seed(149)

## -----------------------------------------------------------------------------
library(recapr)
NChapman(n1=100, n2=150, m2=20)    # Abundance estimate
vChapman(n1=100, n2=150, m2=20)    # estimated variance
seChapman(n1=100, n2=150, m2=20)   # standard error

## -----------------------------------------------------------------------------
ciChapman(n1=100, n2=150, m2=20)

## ----fig.width=7, fig.height=4------------------------------------------------
draws <- rChapman(length=10000, N=1500, n1=100, n2=120)
plotdiscdensity(draws)  

## ----fig.width=7, fig.height=4------------------------------------------------
output <- pChapman(nullN=500, n1=100, n2=100, m2=28, alternative="less")
output

plotdiscdensity(rChapman(length=100000, N=500, n1=100, n2=100))   # null distribution
abline(v=500, lwd=2, lty=2)              # Null hypothesis abundance plotted as a dashed line
abline(v=output$estN, lwd=2, col=2)      # Observed (estimated) abundance plotted as a red line

## ----fig.width=7, fig.height=4------------------------------------------------
powChapman(nullN=500, trueN=400, n1=100, n2=100, nsim=1000)

Ntotry <- seq(from=250, to=450, by=25)
power <- sapply(Ntotry, function(x)
  powChapman(nullN=500, trueN=x, n1=100, n2=100, nsim=1000))
plot(Ntotry, power)  

## -----------------------------------------------------------------------------
n2RR(N=1000, n1=100)

## -----------------------------------------------------------------------------
n2RR(N=1000, n1=100, conf=c(0.9,0.95), acc=c(0.15,0.1,0.05))

## ----fig.width=7, fig.height=6------------------------------------------------
plotn2sim(N=1000, n1=100)

## ----fig.width=7, fig.height=6------------------------------------------------
plotn1n2simmatrix(N=1000)

## -----------------------------------------------------------------------------
mat <- matrix(c(30,15,1,0,22,15), nrow=2, ncol=3, byrow=TRUE)
consistencytest(n1=c(284,199), n2=c(347,3616,1489), stratamat=mat)

## -----------------------------------------------------------------------------
mat <- matrix(c(1,2,3,10,3,2,1,10), nrow=2, ncol=4, byrow=TRUE)
powconsistencytest(n1=c(100,200), n2=c(100,100,100), pmat=mat)

## -----------------------------------------------------------------------------
strattest(n1=c(100,100), n2=c(50,200), m2=c(20,15))

## -----------------------------------------------------------------------------
powstrattest(N=c(1000,2000), n1=c(100,200), n2=c(200,200))

## -----------------------------------------------------------------------------
mat <- matrix(c(59,30,1,45,280,38,0,42,25), nrow=3, ncol=3, byrow=TRUE)
NDarroch(n1=c(484,1468,399), n2=c(847,6616,2489), stratamat=mat)

## ----fig.width=7, fig.height=5------------------------------------------------
Nstrat(n1=c(100,200), n2=c(100,500), m2=c(10,10))
vstrat(n1=c(100,200), n2=c(100,500), m2=c(10,10))
sestrat(n1=c(100,200), n2=c(100,500), m2=c(10,10))
cistrat(n1=c(100,200), n2=c(100,500), m2=c(10,10))
draws <- rstrat(length=10000, N=c(500,1000), n1=c(100,200), n2=c(100,500))
plotdiscdensity(draws)
draws <- rstrat(length=100000, N=c(5000,10000), n1=c(500,200), n2=c(500,200))
plotdiscdensity(draws)

