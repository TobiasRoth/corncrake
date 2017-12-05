model {
  ## Multistate site-occupancy model based on Kendall et al. 2013
  ## Definitions of state z:
  ##   1: site not yet occupied
  ##   2: site occupied by calling CC
  ##   3: CC departed from site
  ## Definition of parameters:
  ##   psi: site-occupancy, i.e. the probability that a site will be occupied by CC at least during one visit
  ##   a0: intercept of logit site-occupancy (psi)
  ##   a1: elevational effect on logit site-occupancy
  ##   a2: effect of the number of years without use on logit site-occupancy
  ##   beta[j]: probability that a site becomes occupied between visit j-1 and j
  ##   alpha0[j]: arrival probability of CC during visit j given that site is not managed
  ##   alpha1: effect of management (that occured before visit) on departure probability, note that
  ##           this effect does not depend on visit nor on year.
  ##   p[j]: detection probability during visit j
  ## Definition of the data:
  ## y[i,j,t]: counts of site i=1,...,N, j=1,...,J visits and t=1,...,T years.

  ## Vorgenommene Änderungen am Modell:
  ## - 08.05.2015: Extended to multi-year model with changes between years modelled as random effect 
  ## - 14.06.2016: Detection probability should be constant over years (but not over season)
  ## - 22.08.2016: Quadratic term for detection probability added; period specific psi

  ## Priors for calling site occupancy
  mu.a0[1] ~ dnorm(0,0.1) # first period
  mu.a0[2] ~ dnorm(0,0.1) # second period
  sd.a0 ~ dunif(0,1)
  prec.a0 <- 1/(sd.a0*sd.a0) 
  for(i in 1:N) {
    a0[i,1] ~ dnorm(mu.a0[1], prec.a0)
    a0[i,2] ~ dnorm(mu.a0[2], prec.a0)
  }
  a1 ~ dnorm(0,1)
  a2 ~ dnorm(0,1)
  a3 ~ dnorm(0,1)

  ## Priors for arrival probability
  for(t in 1:nyear) {
    beta[1:J, t] ~ ddirich(ones[])
  }
  
  ## Priors for departure probability
  for(j in 1:J) {
    mu.alpha0[j] ~ dnorm(0,0.1)
    sd.alpha0[j] ~ dunif(0,1)
    prec.alpha0[j] <- 1/(sd.alpha0[j] * sd.alpha0[j]) 
    for(t in 1:nyear) {
      alpha0[j,t] ~ dnorm(mu.alpha0[j] ,prec.alpha0[j])                  
    }                                     
  }
  alpha1 ~ dnorm(0,0.1)

  ## Priors for detection probability
  mu.p ~ dnorm(0, 0.1)
  sd.p ~ dunif(0,1)
  prec.p <- 1/(sd.p * sd.p) 
  p1 ~ dnorm(0,0.1)
  p2 ~ dnorm(0,1)
  p3 ~ dnorm(0,1)
  for(j in 1:J) {
    tmu.p[j] <- mu.p + p1*j + p2*j*j + p3*j*j*j
    for(t in 1:nyear) {
      logitp[j,t] ~ dnorm(tmu.p[j], prec.p) # <- tmu.p[j]
      logit(p[j,t]) <- logitp[j,t] 
    }                                     
  }
  
  ## Transition matrix D for state transitions
  ## for more details see page 612 of Kendall et al. (2013)
  for(t in 1:nyear) {
    for(i in 1:N) {
      for(j in 1:J) {
        D[i,j,1,2,t] <- beta[j,t]
        D[i,j,1,1,t] <- 1 - D[i,j,1,2,t]
        D[i,j,1,3,t] <- 0
        D[i,j,2,1,t] <- 0
        logit(D[i,j,2,3,t]) <- alpha0[j,t] + alpha1*man[i,j,t]
        D[i,j,2,2,t] <- 1 - D[i,j,2,3,t]
        D[i,j,3,1,t] <- 0
        D[i,j,3,2,t] <- 0
        D[i,j,3,3,t] <- 1
      }
    }
  }
  
  ## Detetection probability
  for(j in 1:J) {
    for(t in 1:nyear) {
      P[j,t,1] <- 0
      P[j,t,2] <- p[j,t]
      P[j,t,3] <- 0
    }
  }
  
  ## Likelihood
  for(t in 1:nyear) {
    for(i in 1:N) {
      logit(psi[i,t]) <- a0[i,1+step(t-4)] + a1*flood[i] + a2*use[i,t] #+ a3*use[i,t]*use[i,t]
      x[i,t] ~ dbern(psi[i,t])
      z[i,1,t] ~ dcat(D[i,1,1,,t])
      muz[i,1,t] <- x[i,t] * P[1,t,z[i,1,t]]
      y[i,1,t] ~ dbern(muz[i,1,t])
      for(j in 2:J) {
        z[i,j,t] ~ dcat(D[i,j,z[i,j-1,t],,t])
        muz[i,j,t] <-  x[i,t] * P[j,t,z[i,j,t]]
        y[i,j,t] ~ dbern(muz[i,j,t])
      }
    }
    
    ## Derived number of occupied calling sites
    for(i in 1:N) {
      for(j in 1:J) {
        mupop[i,j,t] <- equals(z[i,j,t], 2)
      }
    }
    for(j in 1:J) {
      pop[j,t] <- sum(mupop[1:N,j,t])
      deptot[j,t] <- mu.alpha0[j] + alpha1 * mean(man[1:N,j,t])
    }
  }
  
}

