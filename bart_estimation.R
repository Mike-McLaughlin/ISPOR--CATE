#File shows how to use Bayesian Additive Regression Trees (BART) to non-parametrically model individual treatment effects
#Code draws heavily on examples from Hill (2011): https://www.tandfonline.com/doi/abs/10.1198/jcgs.2010.08162
#And BART package vignettes

#Many libraries implement BART (BART, dbarts, bartMachine)
#Use the BART package as it appears to be well-maintained and has a larger set of potential features. Other implementations should
#be similar.
library(BART)

meps_cln <- read_rds(file = "~/Desktop/ISPOR--CATE/data/meps_cln.rds")

set.seed(99) #MCMC, so set the seed

control <- subset(meps_cln, dm_composite == 0, select = c(-dm_composite, -dupersid))
treat <- subset(meps_cln, dm_composite == 1, select = c(-dm_composite, -dupersid))

bart_control = wbart(control[,!(names(control) %in% c("totexp15"))], control$totexp15,
  nskip = 3000, ndpost = 2000)
# plot(bart_control$sigma) #To check burn-in
# abline(v=3000,lwd=2,col="red")

bart_treat = wbart(treat[,!(names(control) %in% c("totexp15"))], treat$totexp15,
  nskip = 1000, ndpost = 2000)
# plot(bart_treat$sigma)
# abline(v=1000,lwd=2,col="red")

#Now form predictions among the full data set in two conditions:
#Everyone is 'treated' and everyone is 'control'

