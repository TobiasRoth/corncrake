#******************************************************************************
#
# Multi-state occupancy model for calling sites of Corncrakes
# The model ist based on Kendall et al 2013 
#
# Autor:    Tobias Roth (t.roth@unibas.ch)
#		
#******************************************************************************

rm(list=ls(all=TRUE))

#---------------------------------------------------------------------------------------------------
# Settings and load data
#---------------------------------------------------------------------------------------------------
# Load data
load("Data/bugsdat.RData")

# Libraries
library(rjags)

# JAGS settings
t.n.thin <- 10
t.n.chains <- 2
t.n.burnin <-10000
t.n.iter <-  10000

t.n.thin <- 1
t.n.chains <- 2
t.n.burnin <-100
t.n.iter <-  100


# Parameters to be monitored
param <- c("mu.a0", "sd.a0", "a1", "a2", "a3", "mu.alpha0", "sd.alpha0", "alpha0", "alpha1", 
           "beta", "mu.p", "sd.p", "p1", "p2", "p3", "pop", "deptot")

#---------------------------------------------------------------------------------------------------
# Function to create initial values
#---------------------------------------------------------------------------------------------------
inits <- function() {
  z <- array(1, dim = c(bugsdat$N,bugsdat$J, bugsdat$nyear))
  for(t in 1:bugsdat$nyear) {
    firstobs <- apply(bugsdat$y[,,t], 1, function(x) ifelse(sum(x, na.rm = TRUE)>0, min(which(x==1)), 0))
    for(ll in 1:dim(z)[1]) {
      if(firstobs[ll]>0) z[ll,firstobs[ll]:ncol(z),t] <- 2
    }    
  }
  list(
    mu.a0 = rnorm(2, 0, 1),
    sd.a0 = runif(1,0.8,1),
    a1 = rnorm(1, 0, 0.1),
    a2 = rnorm(1, 0, 0.1),
    a3 = rnorm(1, 0, 0.1),
    beta = array(rep(1/bugsdat$J, bugsdat$nyear * bugsdat$J), dim = c(bugsdat$J, bugsdat$nyear)),

    mu.alpha0 = rnorm(bugsdat$J, 0, 1),
    sd.alpha0 = runif(bugsdat$J,0,1),
    alpha1 = rnorm(1, 0, 2),
    
    mu.p = rnorm(1, 0, 1),
    sd.p = runif(1,0,1),
    p1 = 0,
    p2 = 0,
    p3 = 0,
    
    z = z
    )
}

#---------------------------------------------------------------------------------------------------
# Run JAGS and save results
#---------------------------------------------------------------------------------------------------
# Run JAGS
jagres <- jags.model("Scripts/Model.R", data = bugsdat, n.chains = t.n.chains, inits = inits, n.adapt = t.n.burnin)
mod <- coda.samples(jagres, param, n.iter=t.n.iter, thin=t.n.thin)

# Save results
save(mod, file = "Results/mod.RData")
