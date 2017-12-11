#******************************************************************************
#
# Extract results from jags-output and make figures
#
# Autor:    Tobias Roth (t.roth@unibas.ch)
#		
#******************************************************************************

rm(list = ls(all = TRUE))

#---------------------------------------------------------------------------------------------------
# Settings
#---------------------------------------------------------------------------------------------------
load("Results/mod.RData")
load("Data/meandat.RData")

# Libraries
library(RColorBrewer)
library(coda)

#---------------------------------------------------------------------------------------------------
# Load JAGS results and have a look at the traceplots
#---------------------------------------------------------------------------------------------------
post <- data.frame(rbind(mod[[1]], mod[[2]]))
names(post) <- dimnames(mod[[1]])[[2]]

par(mfrow = c(3,2))
param <- c("mu.a0[1]", "a1", "a2", "beta[2,1]", "beta[6,1]")
traceplot(mod[, match(param, names(post))])

par(mfrow = c(3,2))
param <- c("mu.alpha0[2]", "mu.alpha0[6]", "alpha1", "mu.p", "p1", "p3")
traceplot(mod[, match(param, names(post))])

#---------------------------------------------------------------------------------------------------
# Results for table (estimate and CrI)
#---------------------------------------------------------------------------------------------------
r <- summary(mod)
round(r$quantiles[c("mu.a0[1]", "mu.a0[2]", "sd.a0", "a1", "a2", "alpha1", "mu.p", "sd.p", "p1", "p2"), 
                  c("50%", "2.5%", "97.5%")], 2)

#---------------------------------------------------------------------------------------------------
# Fig: Effect of management on departure probability at visit j
#---------------------------------------------------------------------------------------------------
j <- 7
res <- data.frame()
pdf("Fig_1_managementeffect_on_departure_10T.pdf", width = 4, height = 5)
par(mfrow = c(1,1), mar = c(3,5,1,1))
txmin <- 0.5
tymin <- 0
plot(NA, xlim = c(txmin, 2 + txmin), ylim = c(tymin,1), axes = FALSE, xlab = "", ylab = "")
axis(1, at = 1:2, labels = rep("", 2), pos = tymin)
mtext(c("Without", "With"), side = 1, at = 1:2, line = 0.5, cex = 1)
mtext(c("management"), side = 1, at = 1.5, line = 1.5, cex = 1)
axis(2, las = 1, pos = txmin)
mtext("Departure probability", 2, line = 3.5)
mtext("(between 3 July and 12 July)", 2, line = 2.7, cex = 0.8)
lines(c(txmin,1), c(tymin, tymin))
lines(c(2, 2 + txmin), c(tymin, tymin))
tt <- cbind(
  plogis(post[,paste("mu.alpha0[", j, "]", sep = "")]),
  plogis(post[,paste("mu.alpha0[", j, "]", sep = "")] + post$alpha1)
  )
points(1:2, apply(tt, 2, median), pch = 16)
res[1:2, "Estimate"] <- apply(tt, 2, median)
segments(1:2, apply(tt, 2, quantile, probs = 0.025), 1:2, apply(tt, 2, quantile, probs = 0.975))
res[1:2, "CI_low"] <- apply(tt, 2, quantile, probs = 0.025)
res[1:2, "CI_up"] <- apply(tt, 2, quantile, probs = 0.975)
dev.off()

#---------------------------------------------------------------------------------------------------
# Effect of the number of years without land-use on calling site occupancy
#---------------------------------------------------------------------------------------------------
nyear <- 20
pdf("Fig_2_managementeffect_on_callingsiteoccupancy_10T.pdf", width = 8, height = 5)
par(mfrow = c(1,1), mar = c(3,5,1,1))
plot(NA, xlim = c(0, nyear + 1), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
axis(1, at = 1:nyear, labels = rep("", nyear), pos = 0)
mtext(1:nyear, side = 1, at = 1:nyear, cex = 0.8, line = 0.2)
mtext("Years since last management", side = 1, line = 1.5)
axis(2, las = 1, pos = 0)
mtext("Calling site occupancy", side = 2, line = 3.5)
mtext("(prob. that a calling site is occupied once during the season)", 2, line = 2.7, cex = 0.8)
lines(c(0,1), c(0,0))
lines(c(nyear,nyear + 1), c(0,0))
tyr <- data.frame(yr = 1:nyear)
for (t in 1:nrow(tyr)) {
  tt <- plogis(post$`mu.a0[2]` + post$a2 * tyr$yr[t]) #+ post$a3 * tyr$yr[t]^2)
  tyr[t, "median"] <- median(tt)
  tyr[t, "low"] <- quantile(tt, probs = 0.025)
  tyr[t, "up"] <- quantile(tt, probs = 0.975)
}
points(1:nyear, tyr$median, pch = 16 )
segments(1:nyear, tyr$low, 1:nyear, tyr$up)
dev.off()

#---------------------------------------------------------------------------------------------------
# Effect of flood duration on calling site occupancy
#---------------------------------------------------------------------------------------------------
telev <- seq(0, 200, 1)
pdf("Fig_3_floodneffect_on_callingsiteoccupancy_10T.pdf", width = 8, height = 5)
par(mfrow = c(1,1), mar = c(3,5,1,1))
plot(NA, xlim = range(telev), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
res <- data.frame(elev=telev)
for(t in 1:nrow(res)){
  tt <- plogis(post$`mu.a0[2]` + post$a1 * res$elev[t] + post$a2 * 5)
  res[t, "median"] <- median(tt)
  res[t, "low"] <- quantile(tt, probs = 0.025)
  res[t, "up"] <- quantile(tt, probs = 0.975)
}
polygon(x = c(telev, rev(telev)), c(res$low, rev(res$up)), col = "grey80", border = "grey80")
points(telev, res$median, pch = 16, ty = "l")
axis(1, at = seq(min(telev), max(telev), 50), labels = rep("", length(seq(min(telev), max(telev), 50))), pos = 0)
mtext(seq(min(telev), max(telev), 50), side = 1, at = seq(min(telev), max(telev), 50), cex = 0.8, line = 0.2)
mtext("Flood duration", side = 1, line = 1.5)
axis(2, las = 1, pos = min(telev))
mtext("Calling site occupancy", side = 2, line = 3.5)
mtext("(prob. that a calling site is occupied once during the season)", 2, line = 2.7, cex = 0.8)
dev.off()

#---------------------------------------------------------------------------------------------------
# Detection probability
#---------------------------------------------------------------------------------------------------
pdf("Fig_4_detection_probability_over_season_10T.pdf", width = 8, height = 5)
par(mfrow = c(1,1), mar = c(3,5,1,1))
plot(NA, xlim = c(-5, 95), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
axis(1, at = c(-2,29,59,90), labels = rep("", 4), pos = 0)
mtext(c("May", "June", "July"), 1, at = c(13.5, 44, 74.5), cex = 0.8)
axis(2, las = 1, pos = -5)
mtext("Detection probability", 2, line = 3.5)
mtext("(prob. that a present male is heard singing during a visit)", 2, line = 2.7, cex = 0.8)
lines(c(-5,-2), c(0,0))
lines(c(90,93), c(0,0))
res <- data.frame(j = 1:9)
for(j in 1:9) {
  tt <- plogis(post$mu.p + post$p1 * j + post$p2 * j^2 + post$p3 * j^3)
  res[j, "median"] <- median(tt)
  res[j, "low"] <- quantile(tt, probs = 0.025)
  res[j, "up"] <- quantile(tt, probs = 0.975)
}
#polygon(x = c(meandat, rev(meandat)), c(apply(tt, 2, quantile, probs=0.025), rev(apply(tt, 2, quantile, probs=0.975))), col = "grey80", border = "grey80")
segments(meandat, res$low, meandat, res$up)
points(meandat, res$median, pch = 16, ty = "p")
dev.off()

#---------------------------------------------------------------------------------------------------
# Population size
#---------------------------------------------------------------------------------------------------
pdf("Fig_5_population_size_over_season_10T.pdf", width = 8, height = 5)
res <- data.frame()
res[1:8, "Periode"] <- 1:8
par(mfrow = c(1,1), mar = c(3,5,1,1))
plot(NA, xlim = c(-5, 95), ylim = c(0,250), axes = FALSE, xlab = "", ylab = "")
axis(1, at = c(-2,29,59,90), labels = rep("", 4), pos = 0)
mtext(c("May", "June", "July"), 1, at = c(13.5, 44, 74.5), cex = 0.8)
axis(2, las = 1, pos = -5)
mtext("Population size", 2, line = 3.5)
mtext("(number of male concrakes present at a visit)", 2, line = 2.7, cex = 0.8)
lines(c(-5,-2), c(0,0))
lines(c(90,93), c(0,0))
tcol <- brewer.pal(11,"RdYlBu")[c(1:3,8:11)]
for(l in 1:7) {
  tchit <- (l-3.5)/2
  tt <- post[, paste("pop[",1:8, ",", l, "]", sep = "")]
  segments(meandat[1:8]+tchit, apply(tt, 2, quantile, probs=0.025), meandat[1:8]+tchit, apply(tt, 2, quantile, probs=0.975), col = tcol[l])
  points(meandat[1:8]+tchit, apply(tt, 2, median), pch = 16, ty = "b", col = tcol[l])
  res[, paste0(c(1998, 1999, 2000, 2012, 2013, 2014, 2015)[l], "_", "mean")] <- apply(tt, 2, mean)
  res[, paste0(c(1998, 1999, 2000, 2012, 2013, 2014, 2015)[l], "_", "low")] <- apply(tt, 2, quantile, probs=0.025)
  res[, paste0(c(1998, 1999, 2000, 2012, 2013, 2014, 2015)[l], "_", "up")] <- apply(tt, 2, quantile, probs=0.975)
}
legend(80, 250, c(1998, 1999, 2000, 2012, 2013, 2014, 2015), col = tcol, pch = 16, bty = "n", cex = 0.8)
dev.off()

#---------------------------------------------------------------------------------------------------
# Arrival Probability
#---------------------------------------------------------------------------------------------------
pdf("Fig_6_arrival_probability_over_season_10T.pdf", width = 8, height = 5)
res <- data.frame()
res[1:8, "Periode"] <- 1:8
par(mfrow = c(1,1), mar = c(3,5,1,1))
plot(NA, xlim = c(-5, 95), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
axis(1, at = c(-2,29,59,90), labels = rep("", 4), pos = 0)
mtext(c("May", "June", "July"), 1, at = c(13.5, 44, 74.5), cex = 0.8)
axis(2, las = 1, pos = -5)
mtext("Arrival probability", 2, line = 3.5)
mtext("(prob. that male is arriving between two 10-day-periods)", 2, line = 2.7, cex = 0.8)
lines(c(-5,-2), c(0,0))
lines(c(90,93), c(0,0))
tcol <- brewer.pal(11,"RdYlBu")[c(1:3,8:11)]
for(l in 1:7) {
  tchit <- (l-3.5)/2
  tt <- post[, paste("beta[",1:9, ",", l, "]", sep = "")]
  segments(meandat[1:8]+tchit, apply(tt[1:8], 2, quantile, probs=0.025), meandat[1:8]+tchit, apply(tt[1:8], 2, quantile, probs=0.975), col = tcol[l])
  points(meandat[1:8]+tchit, apply(tt[1:8], 2, mean), pch = 16, ty = "b", col = tcol[l])
  res[, paste0(c(1998, 1999, 2000, 2012, 2013, 2014, 2015)[l], "_", "mean")] <- apply(tt[1:8], 2, mean)
  res[, paste0(c(1998, 1999, 2000, 2012, 2013, 2014, 2015)[l], "_", "low")] <- apply(tt[1:8], 2, quantile, probs=0.025)
  res[, paste0(c(1998, 1999, 2000, 2012, 2013, 2014, 2015)[l], "_", "up")] <- apply(tt[1:8], 2, quantile, probs=0.975)
}
legend(80, 1, c(1998, 1999, 2000, 2012, 2013, 2014, 2015), col = tcol, pch = 16, bty = "n", cex = 0.8)
dev.off()

#---------------------------------------------------------------------------------------------------
# Arrival Probability (getrennt nach Gebieten)
#---------------------------------------------------------------------------------------------------
pdf("Fig_6b_arrival_probability_over_season_Perioden_10T.pdf", width = 8, height = 5)
res <- data.frame()
res[1:8, "Periode"] <- 1:8
par(mfrow = c(1,1), mar = c(3,5,1,1))
plot(NA, xlim = c(-5, 95), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
axis(1, at = c(-2,29,59,90), labels = rep("", 4), pos = 0)
mtext(c("May", "June", "July"), 1, at = c(13.5, 44, 74.5), cex = 0.8)
axis(2, las = 1, pos = -5)
mtext("Arrival probability", 2, line = 3.5)
mtext("(prob. that male is arriving between two 10-day-periods)", 2, line = 2.7, cex = 0.8)
lines(c(-5,-2), c(0,0))
lines(c(90,93), c(0,0))
tcol <- brewer.pal(11,"RdYlBu")[c(1:3,8:11)]
# 1. Periode
tt <- post[, paste("beta[",1:9, ",", 1, "]", sep = "")] + post[, paste("beta[",1:9, ",", 2, "]", sep = "")] + post[, paste("beta[",1:9, ",", 3, "]", sep = "")]
tt <- tt / 3
segments(meandat[1:8]+tchit, apply(tt[1:8], 2, quantile, probs=0.025), meandat[1:8]+tchit, apply(tt[1:8], 2, quantile, probs=0.975), col = tcol[2])
points(meandat[1:8]+tchit, apply(tt[1:8], 2, mean), pch = 16, ty = "b", col = tcol[2])
res[, "1998-2000_mean"] <- apply(tt[1:8], 2, mean)
res[, "1998-2000_low"] <- apply(tt[1:8], 2, quantile, probs=0.025)
res[, "1998-2000_up"] <- apply(tt[1:8], 2, quantile, probs=0.975)
# 2. Periode
tt <- post[, paste("beta[",1:9, ",", 4, "]", sep = "")] + post[, paste("beta[",1:9, ",", 5, "]", sep = "")] + post[, paste("beta[",1:9, ",", 6, "]", sep = "")] + post[, paste("beta[",1:9, ",", 7, "]", sep = "")]
tt <- tt / 4
segments(meandat[1:8]+tchit, apply(tt[1:8], 2, quantile, probs=0.025), meandat[1:8]+tchit, apply(tt[1:8], 2, quantile, probs=0.975), col = tcol[7])
points(meandat[1:8]+tchit, apply(tt[1:8], 2, mean), pch = 16, ty = "b", col = tcol[7])
res[, "2012-2015_mean"] <- apply(tt[1:8], 2, mean)
res[, "2012-2015_low"] <- apply(tt[1:8], 2, quantile, probs=0.025)
res[, "2012-2015_up"] <- apply(tt[1:8], 2, quantile, probs=0.975)
legend(5, 1, c("1998-2000", "2012-2015"), col = tcol[c(2,7)], pch = 16, bty = "n", cex = 0.8)
dev.off()

#---------------------------------------------------------------------------------------------------
# Departure Probability (seperately for all years)
#---------------------------------------------------------------------------------------------------
pdf("Fig_7a_departure_probability_over_season_sep_10T.pdf", width = 8, height = 5)
res <- data.frame()
res[1:7, "Periode"] <- 2:8
par(mfrow = c(1,1), mar = c(3,5,1,1))
plot(NA, xlim = c(-5, 95), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
axis(1, at = c(-2,29,59,90), labels = rep("", 4), pos = 0)
mtext(c("May", "June", "July"), 1, at = c(13.5, 44, 74.5), cex = 0.8)
axis(2, las = 1, pos = -5)
mtext("Departure probability", 2, line = 3.5)
mtext("(prob. that male is departing between two 10-day-periods)", 2, line = 2.7, cex = 0.8)
lines(c(-5,-2), c(0,0))
lines(c(90,93), c(0,0))
tcol <- brewer.pal(11,"RdYlBu")[c(1:3,8:11)]
for(l in 1:7) {
  tchit <- (l-3.5)/2
  tt <- post[, paste("alpha0[",1:9, ",", l, "]", sep = "")]
  for(u in 1:ncol(tt)) tt[, u] <- plogis(tt[, u])
  segments(meandat[2:8]+tchit, apply(tt[2:8], 2, quantile, probs=0.025), meandat[2:8]+tchit, apply(tt[2:8], 2, quantile, probs=0.975), col = tcol[l])
  points(meandat[2:8]+tchit, apply(tt[2:8], 2, mean), pch = 16, ty = "b", col = tcol[l])
  res[, paste0(c(1998, 1999, 2000, 2012, 2013, 2014, 2015)[l], "_", "mean")] <- apply(tt[2:8], 2, mean)
  res[, paste0(c(1998, 1999, 2000, 2012, 2013, 2014, 2015)[l], "_", "low")] <- apply(tt[2:8], 2, quantile, probs=0.025)
  res[, paste0(c(1998, 1999, 2000, 2012, 2013, 2014, 2015)[l], "_", "up")] <- apply(tt[2:8], 2, quantile, probs=0.975)
}
legend(5, 1, c(1998, 1999, 2000, 2012, 2013, 2014, 2015), col = tcol, pch = 16, bty = "n", cex = 0.8)
dev.off()

#---------------------------------------------------------------------------------------------------
# Departure Probability (separat Gebiete)
#---------------------------------------------------------------------------------------------------
pdf("Fig_7b_departure_probability_over_season_Period_10T.pdf", width = 8, height = 5)
res <- data.frame()
res[1:7, "Periode"] <- 2:8
par(mfrow = c(1,1), mar = c(3,5,1,1))
plot(NA, xlim = c(-5, 95), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
axis(1, at = c(-2,29,59,90), labels = rep("", 4), pos = 0)
mtext(c("May", "June", "July"), 1, at = c(13.5, 44, 74.5), cex = 0.8)
axis(2, las = 1, pos = -5)
mtext("Departure probability", 2, line = 3.5)
mtext("(prob. that male is departing between two 10-day-periods)", 2, line = 2.7, cex = 0.8)
lines(c(-5,-2), c(0,0))
lines(c(90,93), c(0,0))
tcol <- brewer.pal(11,"RdYlBu")[c(1:3,8:11)]
# 1. Periode
tt <- post[, paste("alpha0[",1:9, ",", 1, "]", sep = "")] + post[, paste("alpha0[",1:9, ",", 2, "]", sep = "")] + post[, paste("alpha0[",1:9, ",", 3, "]", sep = "")]
tt <- tt / 3
for (u in 1:ncol(tt)) tt[, u] <- plogis(tt[, u])
segments(meandat[2:8], apply(tt[2:8], 2, quantile, probs = 0.025), meandat[2:8], apply(tt[2:8], 2, quantile, probs = 0.975), col = tcol[2])
points(meandat[2:8], apply(tt[2:8], 2, mean), pch = 16, ty = "b", col = tcol[2])
res[, "1998-2000_mean"] <- apply(tt[2:8], 2, mean)
res[, "1998-2000_low"] <- apply(tt[2:8], 2, quantile, probs = 0.025)
res[, "1998-2000_up"] <- apply(tt[2:8], 2, quantile, probs = 0.975)
# 2. Periode
tt <- post[, paste("alpha0[",1:9, ",", 4, "]", sep = "")] + post[, paste("alpha0[",1:9, ",", 5, "]", sep = "")] + post[, paste("alpha0[",1:9, ",", 6, "]", sep = "")] + post[, paste("alpha0[",1:9, ",", 7, "]", sep = "")]
tt <- tt / 4
for (u in 1:ncol(tt)) tt[, u] <- plogis(tt[, u])
segments(meandat[2:8], apply(tt[2:8], 2, quantile, probs = 0.025), meandat[2:8], apply(tt[2:8], 2, quantile, probs = 0.975), col = tcol[7])
points(meandat[2:8], apply(tt[2:8], 2, mean), pch = 16, ty = "b", col = tcol[7])
res[, "2012-2015_mean"] <- apply(tt[2:8], 2, mean)
res[, "2012-2015_low"] <- apply(tt[2:8], 2, quantile, probs = 0.025)
res[, "2012-2015_up"] <- apply(tt[2:8], 2, quantile, probs = 0.975)
legend(5, 1, c("1998-2000", "2012-2015"), col = tcol[c(2,7)], pch = 16, bty = "n", cex = 0.8)
dev.off()

#---------------------------------------------------------------------------------------------------
# Total departure probability including management (für separat Gebiete)
#---------------------------------------------------------------------------------------------------
pdf("Fig_7c_total_departure_probability_over_season_Period_10T.pdf", width = 8, height = 5)
res <- data.frame()
res[1:7, "Periode"] <- 2:8
par(mfrow = c(1,1), mar = c(3,5,1,1))
plot(NA, xlim = c(-5, 95), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
axis(1, at = c(-2,29,59,90), labels = rep("", 4), pos = 0)
mtext(c("May", "June", "July"), 1, at = c(13.5, 44, 74.5), cex = 0.8)
axis(2, las = 1, pos = -5)
mtext("Total departure probability", 2, line = 3.5)
mtext("(prob. that male is departing between two 10-day-periods)", 2, line = 2.7, cex = 0.8)
lines(c(-5,-2), c(0,0))
lines(c(90,93), c(0,0))
tcol <- brewer.pal(11,"RdYlBu")[c(1:3,8:11)]
# 1. Periode
tt <- post[, paste("deptot[",1:9, ",", 1, "]", sep = "")] + post[, paste("deptot[",1:9, ",", 2, "]", sep = "")] + post[, paste("deptot[",1:9, ",", 3, "]", sep = "")]
tt <- tt / 3
for(u in 1:ncol(tt)) tt[, u] <- plogis(tt[, u])
segments(meandat[2:8], apply(tt[2:8], 2, quantile, probs=0.025), meandat[2:8], apply(tt[2:8], 2, quantile, probs=0.975), col = tcol[2])
points(meandat[2:8], apply(tt[2:8], 2, mean), pch = 16, ty = "b", col = tcol[2])
res[, "1998-2000_mean"] <- apply(tt[2:8], 2, mean)
res[, "1998-2000_low"] <- apply(tt[2:8], 2, quantile, probs=0.025)
res[, "1998-2000_up"] <- apply(tt[2:8], 2, quantile, probs=0.975)
# 2. Periode
tt <- post[, paste("deptot[",1:9, ",", 4, "]", sep = "")] + post[, paste("deptot[",1:9, ",", 5, "]", sep = "")] + post[, paste("deptot[",1:9, ",", 6, "]", sep = "")] + post[, paste("deptot[",1:9, ",", 7, "]", sep = "")]
tt <- tt / 4
for(u in 1:ncol(tt)) tt[, u] <- plogis(tt[, u])
segments(meandat[2:8], apply(tt[2:8], 2, quantile, probs=0.025), meandat[2:8], apply(tt[2:8], 2, quantile, probs=0.975), col = tcol[7])
points(meandat[2:8], apply(tt[2:8], 2, mean), pch = 16, ty = "b", col = tcol[7])
res[, "2012-2015_mean"] <- apply(tt[2:8], 2, mean)
res[, "2012-2015_low"] <- apply(tt[2:8], 2, quantile, probs=0.025)
res[, "2012-2015_up"] <- apply(tt[2:8], 2, quantile, probs=0.975)
legend(5, 1, c("1998-2000", "2012-2015"), col = tcol[c(2,7)], pch = 16, bty = "n", cex = 0.8)
dev.off()

#---------------------------------------------------------------------------------------------------
# Departure probability
#---------------------------------------------------------------------------------------------------
tcol <- brewer.pal(12, "Paired")
pdf("Fig_8_departure_probability_over_season_10T.pdf", width = 8, height = 5)
res <- data.frame()
res[1:7, "Periode"] <- 2:8
par(mfrow = c(1,1), mar = c(3,5,1,1))
plot(NA, xlim = c(-5, 95), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
axis(1, at = c(-2,29,59,90), labels = rep("", 4), pos = 0)
mtext(c("May", "June", "July"), 1, at = c(13.5, 44, 74.5), cex = 0.8)
axis(2, las = 1, pos = -5)
mtext("Departure probability", 2, line = 3.5)
mtext("(prob. that male is departing between two 10-day-periods)", 2, line = 2.7, cex = 0.8)
lines(c(-5,-2), c(0,0))
lines(c(90,93), c(0,0))
tt <- post[, paste("mu.alpha0[", 1:9, "]", sep = "")]
for(i in 1:ncol(tt)) tt[,i] <- plogis(tt[,i])
#polygon(x = c(meandat[-1], rev(meandat[-1])), c(apply(tt, 2, quantile, probs=0.025), rev(apply(tt, 2, quantile, probs=0.975))), col = "grey80", border = "grey80")
segments(meandat[2:8], apply(tt, 2, quantile, probs=0.10)[2:8], meandat[2:8], apply(tt, 2, quantile, probs=0.90)[2:8], col = tcol[4])
points(meandat[2:8], apply(tt, 2, mean)[2:8], pch = 16, ty = "p", lwd = 3, col = tcol[4])
res[, "mean"] <- apply(tt[2:8], 2, mean)
res[, "low"] <- apply(tt[2:8], 2, quantile, probs=0.025)
res[, "up"] <- apply(tt[2:8], 2, quantile, probs=0.975)
dev.off()
