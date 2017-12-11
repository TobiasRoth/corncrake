model {
  
  # Priors for the calling site occupancy
  mu.a0[1] ~ dnorm(0,0.1)
  mu.a0[2] ~ dnorm(0,0.1)
  sd.a0 ~ dunif(0,1)
  prec.a0 <- 1/(sd.a0*sd.a0) 
  for(i in 1:N) {
    a0[i,1] ~ dnorm(mu.a0[1], prec.a0)
    a0[i,2] ~ dnorm(mu.a0[2], prec.a0)
  }
  a1 ~ dnorm(0,1)
  a2 ~ dnorm(0,1)

  # Priors for the arrival probability
  for(t in 1:nyear) {
    beta[1:J, t] ~ ddirich(ones[])
  }
  
  # Priors for departure probability
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
  for(j in 1:J) {
    tmu.p[j] <- mu.p + p1*j + p2*j*j
    for(t in 1:nyear) {
      logitp[j,t] ~ dnorm(tmu.p[j], prec.p)
      logit(p[j,t]) <- logitp[j,t] 
    }                                     
  }
  
  # Transition matrix D for state transitions
  # for more details see page 612 of Kendall et al. (2013)
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
  
  # Detetection probability
  for(j in 1:J) {
    for(t in 1:nyear) {
      P[j,t,1] <- 0
      P[j,t,2] <- p[j,t]
      P[j,t,3] <- 0
    }
  }
  
  # Likelihood
  for(t in 1:nyear) {
    for(i in 1:N) {
      logit(psi[i,t]) <- a0[i, 1 + step(t-4)] + a1*flood[i] + a2*use[i,t]
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
    
    # Derived number of occupied calling sites
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

